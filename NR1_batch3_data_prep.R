library(dplyr)
library(lubridate)
library(zoo)
library(scales)
library(data.table)

# Xsights data path
xsights_obs_path <- "data/Combined observation record_AUG24.xlsx"

# Function to process pig data
preprocess_pig_health_data <- function(data_path) {
  
  # Read data
  xsights_data <- 
    readxl::read_excel(data_path) |>
    as.data.table()
  
  # Process data
  # clean names
  data.table::setnames(xsights_data, 
                       colnames(xsights_data), 
                       colnames(janitor::clean_names(xsights_data)))
  
  xsights_data[, dmac := ifelse(is.na(dmac), 
                                dmac, 
                                substr(gsub(":", "", dmac), 
                                       nchar(gsub(":", "", dmac)) - 3, 
                                       nchar(gsub(":", "", dmac))))]
  
  xsights_data[, dmac := factor(dmac)]
  xsights_data[, condition := factor(condition)]
  xsights_data[, time := format(time, "%H:%M:%S")]
  xsights_data[, time_stamp := lubridate::ymd_hms(paste(date, time))]
  xsights_data[, pen_no := factor(round(as.numeric(pen_no), 0))]
  
  # logical columns to 0 or 1
  xsights_data[, (names(xsights_data)) := lapply(.SD, function(x) if (is.logical(x)) as.integer(x) else x)]
  
  # Rename"wheezing_aug2024_was_ocular_discharge" to just "wheezing"
  names(xsights_data)[13] <-  "wheezing"
  
  # Remove 3 rows without 'condition' value
  xsights_data <- xsights_data[condition %notin% c("Tag Off")]
  
  # Turn condition into 0 and 1 (healthy or sick/dead)
  xsights_data[, target := ifelse(is.na(condition), 
                                  condition, 
                                  ifelse(condition %in% "Healthy",
                                         0,
                                         1))]
  
  return(xsights_data[])
}

processed_data <- preprocess_pig_health_data(xsights_obs_path)

# select cols to make a model dataset
cols_to_keep <- 
  setdiff(colnames(processed_data), c("condition","time_stamp","dmac", "comment", "date", "time", "room_no", "pen_no", "rectal_temp"))

model_df <- processed_data[, ..cols_to_keep]

# Check missing target values (0 healthy, or 1 sick/dead)
holdout_set <- model_df[which(is.na(model_df$target))]

model_df <- model_df[-which(is.na(model_df$target))]

# modelling
library(caret)
set.seed(108) 

# Split data into training and test sets stratified by the label
train_index <- createDataPartition(model_df$target, p = 0.8, list = FALSE)
train_df <- model_df[train_index, ]
test_df <- model_df[-train_index, ]

library(ranger)

# Train a Random Forest model
rf_model <- ranger(target ~ ., data = train_df, probability = TRUE)

# Predict on the test set
rf_predictions <- predict(rf_model, data = test_df)$predictions


library(xgboost)

# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_df |> select(-target)), label = train_df$target)
test_matrix <- xgb.DMatrix(data = as.matrix(test_df |> select(-target)))

# Set parameters for XGBoost
params <- list(
  objective = "binary:logistic",  # Use appropriate objective for your problem
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.3
)

# Train the XGBoost model
xgb_model <- xgboost(data = train_matrix, params = params, nrounds = 200)

# Predict on the test set
xgb_predictions <- predict(xgb_model, test_matrix)

# Example for evaluating accuracy for binary classification
rf_accuracy <- mean(round(rf_predictions) == test_df$target)
xgb_accuracy <- mean(round(xgb_predictions) == test_df$target)

print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("XGBoost Accuracy:", xgb_accuracy))


engineer_features <- function(df) {
  # Create time-based features
  df <- df |>
    mutate(hour = hour(datetime),
           day_of_week = wday(datetime, label = TRUE))
  
  # Create rolling averages for numerical columns
  categorical_cols <- c('body condition', 'diarrhoea', 'respiratory', 'coughing', 
                        'nasal discharge', 'wheezing', 'lameness', 'skin lesions', 'tail bite')
  numerical_cols <- c('rectal temp', categorical_cols)
  
  df <- df |>
    group_by(dmac) |>
    arrange(datetime) |>
    mutate(across(all_of(numerical_cols), 
                  list(rolling_mean_6h = ~ rollapplyr(.x, width = 6*4, mean, fill = NA, align = "right"),
                       rolling_std_6h = ~ rollapplyr(.x, width = 6*4, sd, fill = NA, align = "right")))) |>
    ungroup()
  
  # Create binary flags for any observation in the last 24 hours
  df <- df |>
    group_by(dmac) |>
    arrange(datetime) |>
    mutate(across(all_of(boolean_cols), 
                  list(last_24h = ~ rollapplyr(.x, width = 24*4, max, fill = NA, align = "right")))) |>
    ungroup()
  
  # Calculate the number of observations per animal in the last 24 hours
  df <- df |>
    group_by(dmac) |>
    arrange(datetime) |>
    mutate(obs_count_24h = rollapplyr(datetime, width = 24*4, FUN = length, fill = NA, align = "right")) |>
    ungroup()
  
  # Normalize numerical features
  numerical_features <- df |>
    select(where(is.numeric)) |>
    names()
  
  df[numerical_features] <- df |>
    select(all_of(numerical_features)) |>
    mutate(across(everything(), rescale))
  
  return(df)
}

# Main processing pipeline
process_pig_health_data <- function(raw_df) {
  df <- preprocess_pig_health_data(raw_df)
  df <- engineer_features(df)
  return(df)
}

# Example usage
processed_data <- process_pig_health_data(Combined_observation_record_AUG24)
# write.csv(processed_data, 'processed_pig_health_data.csv', row.names = FALSE)
