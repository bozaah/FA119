library(dplyr)
library(zoo)
library(scales)
library(data.table)
library(lubridate)

library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(mlr3pipelines)
# library(mlr3measures)


# Xsights data path
# xsights_obs_path <- "data/Combined observation record_AUG24.xlsx"
xsights_obs_path <- "data/Trial-1-3PS-Combined_Nov2024.xlsx"


# Function to process pig data
preprocess_pig_health_data <- function(data_path) {
  
  # Read data
  xsights_data <- 
    as.data.table(
      readxl::read_xlsx(file.path(xsights_obs_path), 
                        sheet = "Combined Observations"))
  
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
  # xsights_data[, pen_no := factor(round(as.numeric(pen_no), 0))]
  xsights_data[, pen_number := factor(round(as.numeric(pen_number), 0))]
  xsights_data[, date := lubridate::ymd(date, tz = "Australia/Perth")]
  xsights_data[, time_stamp := lubridate::ymd_hms(paste(date, time))]
  
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

# Min and max dates observations were recorded
start_date <- min(processed_data$date, na.rm = TRUE)
end_date <- max(processed_data$date, na.rm = TRUE)

# select cols to make a model dataset
cols_to_keep <- 
  setdiff(colnames(processed_data), c("condition","time_stamp","dmac", "comment", "date", "time", "room", "pen_number", "rectal_temp", "alert", "user_name"))
  # setdiff(colnames(processed_data), c("condition","time_stamp","dmac", "comment", "date", "time", "room_no", "pen_no", "rectal_temp"))

raw_model_df <- processed_data[, ..cols_to_keep]

# Check missing target values (0 healthy, or 1 sick/dead)
holdout_set <- raw_model_df[which(is.na(raw_model_df$target))]

model_df <- raw_model_df[-which(is.na(raw_model_df$target))]

model_df[, target := factor(target)]
model_df[, bcs := as.numeric(factor(bcs))]
model_df <- model_df[!is.na(target)]
model_df <- model_df[!is.na(bcs)]

# Load your data (assuming it is stored in model_df and the target column is 'target')
task <- TaskClassif$new(id = "pig_health", backend = model_df, target = "target")

# Ensure the split is stratified by the target column
# Stratified Resampling
resampling <- rsmp("holdout", ratio = .7)
resampling$instantiate(task)

# Create the learners
# learner_ranger <- lrn("classif.ranger", predict_type = "prob")
learner_xgboost <- lrn("classif.xgboost", predict_type = "prob")
learner_glm <- lrn("classif.log_reg", predict_type = "prob")  # Standard GLM as baseline
learner_knn <- lrn("classif.kknn", predict_type = "prob")

# Define a list of learners
learners <- list(learner_xgboost, learner_glm, learner_knn)

# Function to perform tuning and evaluation
tune_and_evaluate <- function(learner, task, resampling) {
  
  # Define parameter search space based on the learner
  if (learner$id == "classif.ranger") {
    param_set <- ParamSet$new(params = list(
      ParamInt$new("num.trees", lower = 100, upper = 500),
      ParamDbl$new("mtry", lower = 2, upper = 10)
    ))
  } else if (learner$id == "classif.xgboost") {
    param_set <- ParamSet$new(params = list(
      ParamDbl$new("eta", lower = 0.01, upper = 0.3),
      ParamInt$new("max_depth", lower = 3, upper = 10)
    ))
  } else if (learner$id == "classif.kknn") {
    param_set <- ParamSet$new(params = list(
      ParamInt$new("k", lower = 3, upper = 15)
    ))
  } else {
    # No tuning for GLM, return the default model
    learner$train(task, row_ids = resampling$train_set(1))
    prediction <- learner$predict(task, row_ids = resampling$test_set(1))
    return(prediction$score(msr("classif.acc")))
  }
  
  # Define tuner and auto-tuner
  tuner <- tnr("grid_search", resolution = 10)
  
  at <- AutoTuner$new(
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.acc"),
    search_space = param_set,
    terminator = trm("evals", n_evals = 20),
    tuner = tuner
  )
  
  # Train the model
  at$train(task, row_ids = resampling$train_set(1))
  
  # Predict on the test set
  prediction <- at$predict(task, row_ids = resampling$test_set(1))
  
  # Return the accuracy
  return(prediction$score(msr("classif.acc")))
}

# Train and evaluate all models
results <- lapply(learners, function(learner) tune_and_evaluate(learner, task, resampling))

# Display the results
names(results) <- sapply(learners, function(learner) learner$id)
results

# holdout_set
holdout_set[, target := NULL]


# Step 1: Train Models on the Full Training Data
train_and_evaluate <- function(learner, task, resampling) {
  # Train the model
  learner$train(task, row_ids = resampling$train_set(1))
  
  # Predict on the original test set from the task
  prediction_test <- learner$predict(task, row_ids = resampling$test_set(1))
  
  # Calculate the accuracy on the original test set
  test_accuracy <- prediction_test$score(msr("classif.acc"))
  
  # Return the trained learner and the test accuracy
  return(list(learner = learner, test_accuracy = test_accuracy, prediction_test = prediction_test))
}

# Train and evaluate all models on the original test set
trained_models <- lapply(learners, function(learner) train_and_evaluate(learner, task, resampling))

# Extract the trained learners and test accuracies
learners_trained <- lapply(trained_models, `[[`, "learner")
test_accuracies <- sapply(trained_models, `[[`, "test_accuracy")
test_preds <- lapply(trained_models, `[[`, "prediction_test")



# Define the function to calculate metrics
calculate_metrics <- function(predictions) {
  # Calculate each metric
  auc <- predictions$score(msr("classif.auc"))
  recall <- predictions$score(msr("classif.recall"))
  sensitivity <- predictions$score(msr("classif.sensitivity"))
  specificity <- predictions$score(msr("classif.specificity"))
  false_positive_rate <- predictions$score(msr("classif.fpr"))
  false_negative_rate <- predictions$score(msr("classif.fnr"))
  true_positive_rate <- predictions$score(msr("classif.tpr"))
  true_negative_rate <- predictions$score(msr("classif.tnr"))
  classification_error <- predictions$score(msr("classif.ce"))
  precision <- predictions$score(msr("classif.precision"))

  # Return a data frame with all metrics
  return(data.frame(
    AUC = auc,
    Recall = recall,
    Sensitivity = sensitivity,
    Specificity = specificity,
    False_Positive_Rate = false_positive_rate,
    False_Negative_Rate = false_negative_rate,
    True_Positive_Rate = true_positive_rate,
    True_Negative_Rate = true_negative_rate,
    Class_error = classification_error,
    Precision = precision
  ))
}

# Assuming test_preds is a list of prediction objects
metrics_list <- lapply(test_preds, calculate_metrics)

# Name the metrics for clarity
names(metrics_list) <- c("xgboost", "log", "kknn")

# Print out the metrics for each model
gt::gt(rbindlist(metrics_list))


# Print each model's metrics
for (model_name in names(metrics_list)) {
  cat("Metrics for", model_name, ":\n")
  print(metrics_list[[model_name]])
  cat("\n")
}

# Generate predictions using each trained model
validation_predictions <- lapply(learners_trained, function(learner) {
  # Predict on the validation_df using predict_newdata
  prediction <- learner$predict_newdata(holdout_set)
  
  # Return the predicted classes or probabilities
  return(prediction)
})

# Extract predicted classes for each model
predicted_classes <- lapply(validation_predictions, function(prediction) {
  return(prediction$response)
})

# Extract predicted probabilities for each model 
predicted_probabilities <- lapply(validation_predictions, function(prediction) {
  return(round(prediction$prob))
})

# Print the predicted classes for each model
names(predicted_classes) <- sapply(learners_trained, function(learner) learner$id)
print(predicted_classes)

# Print the predicted probabilities for each model (if applicable)
names(predicted_probabilities) <- sapply(learners_trained, function(learner) learner$id)
print(predicted_probabilities)

holdout_set[, xgboost := predicted_classes[[1]]]
holdout_set[, log_reg := predicted_classes[[2]]]
holdout_set[, kknn := predicted_classes[[3]]]


holdout_set[,id := as.character(processed_data[which(is.na(processed_data$target))]$dmac)]
