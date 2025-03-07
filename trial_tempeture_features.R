library(data.table)
library(zoo)
library(forecast)
library(anomalize)
library(ggplot2)
library(httpgd)

# Start HTTPGd
httpgd::hgd()

## Load in temperature data from python extraction and analysis ----
# List all .parquet files in the directory
parquet_files <- list.files("data/trial_data_chunked", pattern = "\\.parquet$", full.names = TRUE)

# Read each file into a data.table and store in a list
dt_list <- lapply(parquet_files, function(file) {
     arrow::read_parquet(file)
})

# Combine all data.tables into one
combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# View the combined data.table
print(combined_dt)

# Add date and time columns
combined_dt[, date := lubridate::as_date(timestamp)]
combined_dt[, time := format(timestamp, "%H:%M:%S")]
combined_dt[, doy := lubridate::yday(date)]
combined_dt[, week := lubridate::week(date)]

# extract last 4 digits to match dmac on focus health obs
combined_dt[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]

# Count the number of observations per animal_id
observation_counts <- combined_dt[, .N, by = animal_id]

# Rename the count column for clarity
setnames(observation_counts, "N", "observation_count")

# View the result
print(observation_counts)

# check one tag
with(
     subset(combined_dt, animal_id == "C0B54C068B81" & doy %in% c(278:300)),
     plot(temperature ~ timestamp, type = "l")
)

# Function to calculate features for one animal
calculate_features <- function(data) {
     return(data.table(
          mean_temp = mean(data$temperature, na.rm = TRUE),
          median_temp = median(data$temperature, na.rm = TRUE),
          sd_temp = sd(data$temperature, na.rm = TRUE),
          min_temp = min(data$temperature, na.rm = TRUE),
          max_temp = max(data$temperature, na.rm = TRUE),
          range_temp = max(data$temperature, na.rm = TRUE) - min(data$temperature, na.rm = TRUE),
          std = plotrix::std.error(data$temperature, na.rm = TRUE)
          # skewness_temp = skewness(data$temperature, na.rm = TRUE),
          # kurtosis_temp = kurtosis(data$temperature, na.rm = TRUE)
     ))
}

# Apply the feature calculation to each animal
features_daily <- combined_dt[, calculate_features(.SD), by = .(animal_id, doy)]
features_weeky <- combined_dt[, calculate_features(.SD), by = .(animal_id, week)]

with(
     features_weeky[which(!is.na(features_weeky$mean_temp))],
     plot(density(mean_temp))
)

features_daily[, sd_group := ifelse(sd_temp >= 1.0, "high SD", "low SD")]
features_weeky[, sd_group := ifelse(sd_temp >= 1.0, "high SD", "low SD")]

# quick plot
ggplot(na.omit(features_daily), aes(x = mean_temp, fill = sd_group), group = sd_group) +
     ggthemes::scale_fill_fivethirtyeight(name = "Variation") +
     geom_density(alpha = .65) +
     theme_light(base_size = 16)

# Define the periods in hours
periods <- c(3, 6, 12, 24, 30, 48)

dplyr::pull(subset(features_weeky, sd_group == "low SD"), animal_id)

ggplot(subset(combined_dt, animal_id == "E4E32FB782F0"), aes(x = timestamp, y = temperature)) +
     geom_line() +
     theme_light(base_size = 16)

animal_ts <- combined_dt[animal_id == "E4E32FB782F0", .(timestamp, temperature)]
animal_ts <- ts(animal_ts$temperature, frequency = 144) # 15-minute intervals in a day (96 intervals/day)
decomposed_ts <- stl(animal_ts, s.window = "periodic")

# Plot the decomposition
plot(decomposed_ts)

# anomaly
results <- combined_dt |>
     dplyr::group_by(animal_id) |>
     time_decompose(temperature, method = "stl", frequency = "daily", trend = "1 days") |>
     anomalize(remainder) |>
     time_recompose() |>
     as.data.table()

# View the detected anomalies
anomalies <- results |>
     dplyr::filter(anomaly == "Yes")

# Aggregate temperature across all healthy animals
healthy_patterns <- results |>
     dplyr::filter(anomaly == "No") |>
     dplyr::group_by(timestamp)
# summarize(avg_temp = mean(observed))

# Plot the pattern
ggplot(subset(anomalies), aes(x = timestamp, y = trend)) +
     geom_line(alpha = 0.4) +
     theme_light(base_size = 16) +
     labs(title = "Average Temperature Pattern for Healthy Pigs", x = "Time", y = "Temperature")


#### Combine temp data with 3ps health obs
str(combined_dt)
str(model_dt)

merged_dt <- dplyr::left_join(combined_dt, model_dt, by = c("dmac", "date"))

# initial da cleaning for modelling
clean_dt <- merged_dt[which(!is.na(merged_dt$condition))]
clean_dt <- clean_dt[which(!is.na(clean_dt$temperature))]
# clean_dt <- clean_dt[which(!is.na(clean_dt$dmac))]

arrow::write_parquet(clean_dt, "trial_3Ps_merged_temp_health.parquet")

# check for duplicated
sum(duplicated(clean_dt))
str(clean_dt)

ggplot(results, aes(x = remainder, fill = anomaly, group = anomaly)) +
     geom_density(alpha = 0.4) +
     theme_light(base_size = 16) +
     labs(title = "10-min Temperature Pattern ", y = "Density", x = "Ear tag temperature")

ggplot(clean_dt, aes(x = health_index, fill = condition, group = condition)) +
     geom_density(alpha = 0.4) +
     theme_light(base_size = 16) +
     labs(title = "10-min Temperature Pattern ", y = "Density", x = "Health Index")

ggplot(clean_dt, aes(x = bcs, fill = condition, group = condition)) +
     geom_density(alpha = 0.4) +
     theme_light(base_size = 16) +
     labs(title = "Body condition score", y = "Density", x = "BCS")


# Check pugs with health status
healthy_pigs_obs <- subset(clean_dt, condition == "Healthy")


####
# Calculate temperature features
z <- create_features(combined_dt)
z

# check one tag
subset(z, animal_id == "C0B54C068B81")

with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     plot(temperature ~ timestamp, type = "l", col = "grey", lwd = 1.6)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_3h ~ timestamp, col = "orange", lwd = 4)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_6h ~ timestamp, col = "forestgreen", lwd = 4)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_12h ~ timestamp, col = "darkblue", lwd = 4)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_24h ~ timestamp, col = "steelblue", lwd = 4)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_30h ~ timestamp, col = "purple2", lwd = 4)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_Avg_48h ~ timestamp, col = "firebrick", lwd = 4)
)

# with(subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
# plot(temperature ~ timestamp, type = "l", col = "grey", lwd = 1.6))
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     plot(Rolling_SD_3h ~ timestamp, type = "l", col = "orange", lwd = 3)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_SD_6h ~ timestamp, col = "forestgreen", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_SD_12h ~ timestamp, col = "darkblue", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_SD_24h ~ timestamp, col = "steelblue", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_SD_30h ~ timestamp, col = "purple2", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_SD_48h ~ timestamp, col = "firebrick", lwd = 2)
)

# with(subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
# plot(temperature ~ timestamp, type = "l", col = "grey", lwd = 1.6))
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     plot(Rolling_range_3h ~ timestamp, type = "l", col = "orange", lwd = 3)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_range_6h ~ timestamp, col = "forestgreen", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_range_12h ~ timestamp, col = "darkblue", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_range_24h ~ timestamp, col = "steelblue", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_range_30h ~ timestamp, col = "purple2", lwd = 2)
)
with(
     subset(z, animal_id == "C0B54C068B81" & doy %in% c(278:290)),
     lines(Rolling_range_48h ~ timestamp, col = "firebrick", lwd = 2)
)


# Set theoretical temperature thresholds
# stats_by_tag <- setDT(stats_by_tag)

z[, hypo_crit := fifelse(Rolling_Avg_3h <= 35 & Rolling_Avg_3h > 32, 1, 0)]
z[, hypo_severe := fifelse(Rolling_Avg_3h <= 32, 1, 0)]

# Fever
z[, fever_crit := fifelse(Rolling_Avg_3h >= 40 & Rolling_Avg_3h < 42, 1, 0)]
z[, fever_severe := fifelse(Rolling_Avg_3h >= 42, 1, 0)]

# Optimum
z[, optimum_temp := fifelse(Rolling_Avg_3h >= 38.5 & Rolling_Avg_3h <= 39.5, 1, 0)]

z[, condition := fifelse(
     fever_crit == 1 | hypo_crit, "watch",
     fifelse(
          fever_severe == 1 | hypo_severe == 1, "sick",
          fifelse(optimum_temp == 1, "optimal", "normal")
     )
)]


with(z, boxplot(Rolling_Avg_3h ~ condition,
     main = "Rolling avg temperature (3h)",
     ylab = "Ear tag temperature (C)"
))


best_pigs <- subset(z, optimum_temp == 1)
best_pig_tags <- dplyr::pull(best_pigs, animal_id) |> unique()

with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     plot(Rolling_SD_48h ~ timestamp, type = "l", col = "firebrick", lwd = 3, ylim = c(.5, 2))
)
with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_SD_6h ~ timestamp, col = "forestgreen", lwd = 2)
)
with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_SD_12h ~ timestamp, col = "darkblue", lwd = 2)
)
with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_SD_24h ~ timestamp, col = "steelblue", lwd = 2)
)
with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_SD_30h ~ timestamp, col = "purple2", lwd = 2)
)
with(
     subset(best_pigs, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_SD_3h ~ timestamp, col = "orange", lwd = 3)
)

with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     plot(temperature ~ timestamp, type = "l", col = "grey", lwd = 1.6)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_3h ~ timestamp, col = "orange", lwd = 4)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_6h ~ timestamp, col = "forestgreen", lwd = 4)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_12h ~ timestamp, col = "darkblue", lwd = 4)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_24h ~ timestamp, col = "steelblue", lwd = 4)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_30h ~ timestamp, col = "purple2", lwd = 4)
)
with(
     subset(z, animal_id == "C3A1942BDD80" & doy %in% c(278:290)),
     lines(Rolling_Avg_48h ~ timestamp, col = "firebrick", lwd = 4)
)


library(censored)
library(survival)
library(tidymodels)

tidy_dt <-
     clean_dt |>
     dplyr::mutate(
          target_surv = Surv(temperature, dplyr::if_else(target_weighted == "Sick", 1, 0)),
          .keep = "unused"
     ) |>
     as_tibble() |>
     select(bcs, condition, bcs_count, health_index, target, target_surv)

set.seed(108)

tidy_dt_split <- initial_split(tidy_dt, strata = target)

tidy_dt_train <- training(tidy_dt_split)
tidy_dt_test <- testing(tidy_dt_split)

tidy_dt_recipe <- recipe(target_surv ~ ., data = tidy_dt_train) |>
     step_zv(all_predictors())

tidy_dt_spec <- proportional_hazards() |>
     set_mode("censored regression") |>
     set_engine("survival")

tidy_dt_work <- workflow() |>
     add_recipe(tidy_dt_recipe) |>
     add_model(tidy_dt_spec)

tidy_dt_fit <- fit(tidy_dt_work, data = tidy_dt_train)
