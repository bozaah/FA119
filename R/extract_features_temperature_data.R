library(forecast)
library(anomalize)
library(ggplot2)

# load data
batch3_focus_temp <- arrow::read_parquet("data/processed/batch3_focus_pig_temperature.parquet")

# Split data based on location ID (ie where the pigs where)
batch3_focus_bylocation <- 
  group_split(batch3_focus_temp,
              location_code)

# Most data is from nursery room
# The other locations only have one pig (E79CF8753649) with 3 observations for 
# each location.

# Select only NR data
batch3_focus_NR <- 
  batch3_focus_bylocation[[5]] |>
  dplyr::arrange(timestamp_awst) |>
  mutate(
    location_code = as.character(location_code)
  ) |>
  as.data.table()

animal_ts <- batch3_focus_NR[animal_id == "E79CF8753649", .(timestamp_awst, temperature)]
animal_ts <- ts(animal_ts$temperature, frequency = 96) # 15-minute intervals in a day (96 intervals/day)
decomposed_ts <- stl(animal_ts, s.window = "periodic")

# Plot the decomposition
plot(decomposed_ts)

  # anomaly
results <- batch3_focus_NR |>
  group_by(animal_id) |>
  time_decompose(temperature, method = "stl", frequency = "daily", trend = "1 days") |>
  anomalize(remainder) |>
  time_recompose() |>
  as.data.table()

results_test <- ts(results[animal_id == "DD88A0D815D6", .(observed, season, trend, remainder)], frequency = 96)
class(results_test) <- c(class(results_test), "stl")
plot(results_test)

# View the detected anomalies
anomalies <- results |>
  filter(anomaly == "Yes")

# Aggregate temperature across all healthy animals
healthy_patterns <- results |>
  filter(anomaly == "No") |>
  group_by(timestamp_awst) 
  # summarize(avg_temp = mean(observed))

# Plot the pattern
ggplot(subset(healthy_patterns), aes(x = timestamp_awst, y = trend)) +
  geom_line(alpha = 0.4) +
  labs(title = "Average Temperature Pattern for Healthy Pigs", x = "Time", y = "Temperature")



  library(e1071)  # for kurtosis and skewness



# Apply the feature calculation to each animal
features <- batch3_focus_NR[, calculate_features(.SD), by = animal_id]

# Clustering
set.seed(108)  # for reproducibility

# Normalize the features
features_scaled <- scale(features[, -1, with = FALSE])

# Apply K-means clustering
kmeans_result <- kmeans(features_scaled, centers = 2)  # You can adjust the number of clusters

# Add cluster information to the original data
features[, cluster := kmeans_result$cluster]

# Merge the rolling features with summary features
combined_features <- merge(batch3_focus_NR, features, by = "animal_id")

with(combined_features, boxplot(temperature ~ cluster))

combined_features[, `:=`(
  rolling_mean_1h = zoo::rollapply(temperature, width = 4, FUN = mean, fill = NA, align = "right"),  # 1 hour
  rolling_mean_4h = zoo::rollapply(temperature, width = 4*4, FUN = mean, fill = NA, align = "right"),  # 1 hour
  rolling_mean_6h = zoo::rollapply(temperature, width = 4*6, FUN = mean, fill = NA, align = "right"),  # 1 hour
  rolling_mean_24h = zoo::rollapply(temperature, width = 96, FUN = mean, fill = NA, align = "right"),  # 1 hour
  rolling_sd_1h = zoo::rollapply(temperature, width = 4, FUN = sd, fill = NA, align = "right"),
  rolling_sd_4h = zoo::rollapply(temperature, width = 4*4, FUN = sd, fill = NA, align = "right"),
  rolling_sd_6h = zoo::rollapply(temperature, width = 4*6, FUN = sd, fill = NA, align = "right"),
  rolling_sd_24h = zoo::rollapply(temperature, width = 96, FUN = sd, fill = NA, align = "right"),
  diff_15m = c(NA, diff(temperature))
)]

model_df <- 
  combined_features |>
  group_by(animal_id) |>
  time_decompose(temperature, method = "stl", frequency = "daily", trend = "1 days") |>
  anomalize(remainder) |>
  time_recompose()

x <- merge(model_df, features, by = "animal_id")


NAs_to_remove <- which(is.na(combined_features)$rolling_sd_24h))
combined_features <- combined_features[-NAs_to_remove]

ggplot(combined_features, aes(x = factor(cluster), y = sd_temp)) + geom_boxplot()

pigs_high_sd <- subset(combined_features, sd_temp <= 1.1) |> pull(animal_id) |> unique() |> as.character()

pigs_high_sd <- subset(x, anomaly == "Yes" & sd_temp <= 1.1) |> pull(animal_id) |> unique() |> as.character()


plots_high_sd <- lapply(pigs_high_sd,
                        plot_pig_temp_batch3,
                        combined_features)
cowplot::plot_grid(
  plotlist = plots_high_sd, nrow = 4
)


set.seed(108)
train_indices <- sample(1:nrow(combined_features), 0.7 * nrow(combined_features))
train_data <- combined_features[train_indices]
test_data <- combined_features[-train_indices]

# vector with cleaned names
# c("body_condition", "diarrhoea", "respiratory", "coughing", "nasal", "discharge", "wheezing", "(Aug2024)", "(was", "ocular", "discharge)", "lameness", "Left", "front", "Right", "front", "Left", "hind", "Right", "hind", "skin", "lesions", "tail", "bite", "abscess", "prolapse", "hernia", "hematoma", "left", "hematoma", "right", "med1", "med2", "med3", "toSick", "fromSick", "Euthanised")


