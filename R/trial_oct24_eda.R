library(data.table)
library(ggplot2)

dt <- 
  as.data.table(
    readxl::read_xlsx(file.path("data/Trial-1-3PS-Combined_Nov2024.xlsx"), 
                      sheet = "Combined Observations")
  ) 

dt <- janitor::clean_names(dt)

dt[, dmac := factor(dmac)]
dt[, alert := factor(alert)]
dt[, condition := factor(condition)]
dt[, time := format(time, "%H:%M:%S")]
dt[, pen_number := factor(round(as.numeric(pen_number), 0))]
dt[, date := lubridate::ymd(date, tz = "Australia/Perth")]
dt[, time_stamp := lubridate::ymd_hms(paste(date, time), tz = "Australia/Perth")]

# logical columns to 0 or 1
dt[, (names(dt)) := lapply(.SD, function(x) if (is.logical(x)) as.integer(x) else x)]

# Turn condition into 0 and 1 (healthy or sick/dead)
dt[, target := factor(ifelse(is.na(condition), 
                             condition, 
                             ifelse(condition %in% "Healthy",
                                    0,
                                    1)))]

# one dmac 1A64 on 2024-10-28 10:28:30, was on NR3 and I moved it to NR1
dt[which(dt$room == "NR3"), room := "NR1"]

# Filter rows with missing dmac ids
dt <- dt[!is.na(dmac)]
dt <- dt[!is.na(condition)]
dt <- dt[!is.na(bcs)]

# Clean and convert bcs to numeric
dt[, bcs := as.numeric(gsub("\\.+", ".", bcs))] # Removes extra dots, then converts to numeric

# Convert any NAs introduced by the gsub to 0.5
dt[is.na(bcs), bcs := 0.5]

# Convert bcs to ordered factor
dt[, bcs := factor(bcs, levels = sort(unique(bcs)), ordered = TRUE)]
bcs_counts <- dt[, .(bcs_count = .N), by = .(bcs)]

# Merge the counts back into the original data
dt <- merge(dt, bcs_counts, by = c("bcs"), all.x = TRUE)

# Remove duplicates
dt <- dt[!duplicated(dt)]

# Ensure data is sorted by dmac and timestamp for accurate cumulative calculations
setorder(dt, time_stamp, dmac)

# Extract dates with the temperature data in mind.
unique_dates <- dt |> dplyr::pull(time_stamp)
unique_dmac <- dt |> dplyr::pull(dmac)
unique_ids_dates <- data.table(dmac = unique_dmac, time_stamp = unique_dates)

date_start <- min(dt$time_stamp)
date_end <- max(dt$time_stamp)

# Check RED alerts data
dt[grep("^RED", dt$alert)]

# Get only dead pigs data
only_dead_dt <- subset(dt, condition == "Dead") 
dead_pigs <- as.character(unique(subset(only_dead_dt, condition == "Dead")$dmac))
n_dead_pigs <- length(dead_pigs)
n_dead_pigs

ggplot(subset(only_dead_dt, bcs %notin% c(NA)), aes(x = dmac, y = bcs)) +
  geom_col(alpha = .8) +
  facet_wrap(. ~ alert, nrow = 2) + 
  theme_minimal(base_size = 16)

ggplot(subset(only_dead_dt, bcs %notin% c(NA)), aes(x = bcs)) +
  geom_histogram(alpha = .8, stat = "count") +
  # facet_wrap(. ~ alert, nrow = 2) + 
  theme_minimal(base_size = 16)

# Get only healthy pigs data
only_healthy_dt <- subset(dt, condition == "Healthy") 
healthy_pigs <- as.character(unique(subset(only_healthy_dt, condition == "Healthy")$dmac))
n_healthy_pigs <- length(healthy_pigs)
n_healthy_pigs

ggplot(subset(only_healthy_dt, bcs %notin% c(NA)), aes(x = bcs)) +
  geom_histogram(alpha = .8, stat = "count") +
  # facet_wrap(. ~ alert, nrow = 2) + 
  theme_minimal(base_size = 16)

## Cummulative alerts
# Step 1: Create a binary column indicating the presence of an alert
dt[, alert_flag := as.integer(!is.na(alert) & alert %notin% c("None", "<NA>"))]

# Step 2: Calculate cumulative alerts for each dmac ID over time
dt[, cumulative_alerts := cumsum(alert_flag), by = dmac]

ggplot(dt, aes(x = bcs)) +
  geom_histogram(stat = "count") +
  facet_grid(. ~ condition) +
  theme_light(base_size = 16)

ggplot(dt, aes(bcs)) +
  geom_histogram(stat = "count") +
  facet_grid(. ~ target) +
  theme_light(base_size = 16)

##############################################################################################################################
# Use condition to calculate weights
# Load necessary libraries
library(ranger)

# Step 2: Ensure condition is a factor and in the right order
model_dt <- dt[, condition := factor(condition, levels = c("Healthy", "Sick", "Dead"))]

# Step 3: Define health indicator columns, adding `bcs`
health_columns <- c("diarrhoea", "respiratory", "coughing", "neuro", "ataxia", 
                    "paddling", "seizure", "nasal_disc", "lameness", 
                    "number_swollen_joints", "left_front", "right_front", 
                    "left_hind", "right_hind", "skin_lesion", "tail_bite",
                    "abscess", "prolapse", "hernia", "bloating", 
                    "hema_left", "hema_right", "treated_prev",
                    "med1", "med2", "med3", "tosick", "fromsick", "euthanise", "cumulative_alerts")

# Ensure all health_columns are numeric
model_dt[, (health_columns) := lapply(.SD, as.numeric), .SDcols = health_columns]

# Step 5: Exclude health columns with zero variance
health_columns <- health_columns[sapply(dt[, ..health_columns], function(col) var(col, na.rm = TRUE) > 0)]

# Step 6: Define formula and train Random Forest with `condition` as target
formula <- as.formula(paste("condition ~", paste(health_columns, collapse = " + ")))

rf_model <- ranger(formula, 
                   data = model_dt, 
                   importance = "impurity",
                   num.trees = 500, 
                   max.depth = 3, 
                   oob.error = TRUE)

# get weights - Extract feature importance and normalize to 0-1 range
rf_importances <- rf_model$variable.importance
rf_weights <- (rf_importances - min(rf_importances, na.rm = TRUE)) / (max(rf_importances, na.rm = TRUE) - min(rf_importances, na.rm = TRUE))

barplot(sort(rf_weights))

# Separate and normalize weights
other_weights <- 
  rf_importances[health_columns %notin% c("cumulative_alerts")] / 
  sum(rf_importances[health_columns %notin% c("cumulative_alerts")])

cumulative_alerts_weight <- rf_importances["cumulative_alerts"] / sum(rf_importances["cumulative_alerts"])

# Apply proportion (e.g., 0.8 for other indicators, 0.2 for `bcs`)
combined_weights <- c(other_weights * 0.9, cumulative_alerts_weight * 0.1)

# Separate weights for 'tosick' and 'fromsick' and adjust calculation
tosick_weight <- combined_weights["tosick"]
fromsick_weight <- combined_weights["fromsick"]

# Remove 'tosick' and 'fromsick' from the main health columns for direct calculation
main_health_columns <- setdiff(health_columns, c("tosick", "fromsick"))

# Assuming 'rf_weights' is your original data (likely a matrix or similar)
# rf_weights_dt <- as.data.table(t(combined_weights))
# rf_weights_dt$bcs <- NULL
rf_weights_dt <- as.data.table(t(combined_weights))

# Calculate health_index with positive contribution from 'tosick' and negative from 'fromsick'
model_dt[, health_index := (
  rowSums(.SD * rf_weights[main_health_columns]) / sum(rf_weights[main_health_columns])
  - tosick_weight * tosick / sum(rf_weights[main_health_columns])
  + fromsick_weight * fromsick / sum(rf_weights[main_health_columns])
), .SDcols = main_health_columns]

# Invert and normalize the health_index to make lower values healthier
model_dt[, health_index := (health_index / max(health_index, na.rm = TRUE))]

# Scale down the weight of `bcs` by a factor of 0.5
# rf_importances["bcs"] <- rf_importances["bcs"] * 0.5

# Normalize all weights to a 0-1 range after scaling down `bcs`
# rf_weights <- (rf_importances - min(rf_importances, na.rm = TRUE)) / 
# (max(rf_importances, na.rm = TRUE) - min(rf_importances, na.rm = TRUE))

# Reshape to long format
rf_weights_dt_long <- melt(rf_weights_dt, 
                           measure.vars = names(rf_weights_dt), # Use all columns as measure variables
                           variable.name = "symptom",
                           value.name = "weight")

rf_weights_dt_long[, symptom := factor(symptom, levels = symptom[order(weight)])]

# Create the ggplot
ggplot(rf_weights_dt_long, aes(x = symptom, y = weight)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" for pre-calculated values
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
  labs(title = "Health observation weights", x = "Symptom\n", y = "\nWeights") + # Add labels 
  theme_light(base_size = 16) 

# Now calculate health_index using the normalized weights
# model_dt[, health_index := (rowSums(.SD * rf_weights[health_columns]) / sum(rf_weights[health_columns])), .SDcols = health_columns]

# Step 9: Scale health_index to be between 0-1 for final normalization
model_dt[, health_index := health_index / max(health_index, na.rm = TRUE)]

# Print summary statistics of health_index
summary_stats <- model_dt[, .(
  min_health_index = min(health_index, na.rm = TRUE),
  max_health_index = max(health_index, na.rm = TRUE),
  mean_health_index = mean(health_index, na.rm = TRUE),
  median_health_index = median(health_index, na.rm = TRUE)
)]
print(summary_stats)

with(model_dt, boxplot(health_index ~ condition))
with(model_dt, boxplot(health_index ~ target))
with(model_dt, plot(density(health_index)))

ggplot(model_dt, aes(x = health_index)) +
  geom_density(alpha = .7) +
  facet_wrap(. ~ condition) +
  theme_light(base_size = 16)

##############################################################################################################################
library(sentimentr)
# Perform sentiment analysis
sentiment_scores <- sentiment(model_dt$comment)
sentiment_scores <- sentiment_scores[-424]

# Add the sentiment scores and classification directly to the data.table
model_dt[, sentiment_score := sentiment_scores$sentiment]
model_dt[, sentiment_word_count := sentiment_scores$word_count]
model_dt[, sentiment := factor(ifelse(sentiment_score > 0, "healthy", ifelse(sentiment_score < 0, "sick", "neutral")))]
model_dt[, sentiment_int := as.integer(sentiment)]

ggplot(subset(model_dt, sentiment_word_count > 0), aes(x = sentiment_score, y = health_index, color = condition)) +
  geom_point(size = 3.3, alpha = .7) + 
  scale_color_manual(values = c("forestgreen","blue4", "firebrick")) +
  labs(title = "Health Index vs. Sentiment Score", 
       subtitle = "Only for word count > 0",
       x = "\nSentiment Score", 
       y = "Health Index\n", 
       color = "Condition") +
  theme_light(base_size = 16)

subset(model_dt, condition == "Dead" & sentiment_score < 0)
subset(model_dt, condition == "Sick" & sentiment_score > 0)
subset(model_dt, condition == "Healthy" & sentiment_score < 0)

#### New target variable
# Load necessary libraries
# library(data.table)
library(tidymodels)

# Ensure `condition` is a factor and create initial binary target for Healthy vs. others
model_dt[, target_weighted := factor(ifelse(condition == "Healthy", 0, 1))]

# Define logistic regression model using tidymodels
log_formula <- as.formula(paste("target ~", paste(c(health_columns,
                                                    "bcs", 
                                                    "sentiment_score",
                                                    'health_index'), collapse = " + ")))

logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(log_formula,
      data = model_dt)

# Get predicted probabilities and assign target_weighted based on threshold (e.g., 0.5)
model_dt[, target_weighted := predict(logistic_model, new_data = model_dt, type = "prob")[,2] > 0.9]
model_dt[, target_weighted := factor(as.integer(target_weighted))]  # Convert TRUE/FALSE to 1/0

autoplot(conf_mat(model_dt, target, target_weighted), type = "heatmap") 

with(model_dt, boxplot(bcs ~ target_weighted))
with(model_dt, boxplot(health_index ~ target_weighted))
with(model_dt, boxplot(health_index ~ bcs + target))

subset(model_dt, condition == "Sick")
subset(model_dt, condition == "Sick" & health_index > 0.5)
