# Data
library(data.table)
library(arrow)
library(zoo)
library(anomalize)

# ML
library(mlr3)
library(mlr3verse)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3viz)
library(paradox)

is_empty <- function(x) {
  if (length(x) == 0 & !is.null(x)) {
    TRUE
  } else {
    FALSE
  }
}

# List .parquet files in the directory
parquet_files <- list.files("outputs_3PS_trial3", pattern = "\\.parquet$", full.names = TRUE)

if (length(parquet_files) > 0) {
  # Get file information for each file
  file_info <- file.info(parquet_files)
  
  # Identify the latest file based on creation time (or modification time if preferred)
  latest_file <- rownames(file_info)[which.max(file_info$ctime)]
  
  # Calculate the age of the latest file in hours
  age_hours <- as.numeric(difftime(Sys.time(), file_info[latest_file, "ctime"], units = "hours"))
  
  if (age_hours <= 1) {
    message("\nLatest .parquet file (", latest_file, ") is only ", round(age_hours, 2),
            " hours old. Skipping data download\n")
    
  } else if (age_hours > 1) {
    
    # Run python
    library(reticulate)
    reticulate::use_condaenv("fa119")
    reticulate::py_config()
    
    # Source new data
    # 1. Construct the argument vector as you would on the command line
    args <- c("../../../../Desktop/Workbench/Code/3Ps_snowflake/source_data_last_48h_3PS_trial3.py", 
              "--start_datetime=2025-03-01 00:00:00",
              "--end_datetime=2025-03-07 08:00:00")
    
    # 2. Assign that vector to sys.argv within Python
    sys <- import("sys")
    sys$argv <- args
    
    py_run_file("../../../../Desktop/Workbench/Code/3Ps_snowflake/source_data_last_48h_3PS_trial3.py")
    
  } else {
    stop("There is an error with the temperature data download", call. = F)
  }
  
} else if (is_empty(parquet_files)) {
  
  # Run python
  library(reticulate)
  reticulate::use_condaenv("fa119")
  reticulate::py_config()
  
  # Source new data
  # 1. Construct the argument vector as you would on the command line
  args <- c("../../../../Desktop/Workbench/Code/3Ps_snowflake/source_data_last_48h_3PS_trial3.py", 
            "--start_datetime=2025-02-10 00:00:00",
            "--end_datetime=2025-02-13 08:00:00")
  
  # 2. Assign that vector to sys.argv within Python
  sys <- import("sys")
  sys$argv <- args
  
  py_run_file("../../../../Desktop/Workbench/Code/3Ps_snowflake/source_data_last_48h_3PS_trial3.py")
  
} else {
  stop("\nThere is an error with the temperature data download or loading\n", call. = F)
}

# Source functions 
source("R/utils.R")

# List files
message("\nLoad and manipulate data\n")
parquet_files <- list.files("outputs_3PS_trial3", pattern = "\\.parquet$", full.names = TRUE)
parquet_files <- sort(parquet_files)
dt <- arrow::read_parquet(parquet_files[length(parquet_files)]) |> setDT()

dt[, date_awst := as.IDate(timestamp, tz = "Australia/Perth")]
dt[, time_awst := as.ITime(timestamp, tz = "Australia/Perth")]
dt[, timestamp_awst := lubridate::ymd_hms(paste0(date_awst, " ", time_awst), tz = "Australia/Perth")]
dt[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# dt[, date := lubridate::as_date(timestamp)]
# dt[, time := format(timestamp, "%H:%M:%S")]
dt[, doy := lubridate::yday(date_awst)]
dt[, week := lubridate::week(date_awst)]
setorder(dt, timestamp_awst, animal_id)

# Check counts of observations for ts
obs_counts <- dt[, .N, by = animal_id]
to_remove_idx <- which(obs_counts$N < 144) 
to_remove_tag <- obs_counts[to_remove_idx, animal_id] 

# remove tags with unsif
dt <- subset(dt, animal_id %notin% to_remove_tag)

# # manually add output from snowflake dashboard
# dt_manual <- arrow::read_parquet("temperature_data_20250213_074616.parquet") |> setDT()
# dt_manual[, date_awst := as.IDate(timestamp, tz = "Australia/Perth")]
# dt_manual[, time_awst := as.ITime(timestamp, tz = "Australia/Perth")]
# dt_manual[, timestamp_awst := lubridate::ymd_hms(paste0(date_awst, " ", time_awst), tz = "Australia/Perth")]
# dt_manual[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# # dt[, date := lubridate::as_date(timestamp)]
# # dt[, time := format(timestamp, "%H:%M:%S")]
# dt_manual[, doy := lubridate::yday(date_awst)]
# dt_manual[, week := lubridate::week(date_awst)]
# setorder(dt_manual, timestamp_awst, animal_id)

# dt_latest <- data.table::fread("outputs_3PS_trial3/temperature_data_Feb05-Feb07_manual.csv")
# names(dt_latest) <- tolower(names(dt_latest))
# 
# # Remove unused cols
# dt_latest[, client := NULL]
# dt_latest[, farm := NULL]
# dt_latest[, id := NULL]
# 
# # Match dt columns
# dt_latest[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# dt_latest[, date := lubridate::as_date(timestamp)]
# dt_latest[, time := format(timestamp, "%H:%M:%S")]
# dt_latest[, doy := lubridate::yday(date)]
# dt_latest[, week := lubridate::week(date)]
# setorder(dt_latest, timestamp, animal_id)
# setcolorder(dt_latest, neworder = names(dt))
# 
# # Load manual data again
# dt_latest2 <- data.table::fread("outputs_3PS_trial3/temperature_data_Feb05-Feb10_manual.csv")
# names(dt_latest2) <- tolower(names(dt_latest2))
# 
# # Remove unused cols
# dt_latest2[, client := NULL]
# dt_latest2[, farm := NULL]
# dt_latest2[, id := NULL]
# 
# # Match dt columns
# dt_latest2[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# dt_latest2[, date := lubridate::as_date(timestamp)]
# dt_latest2[, time := format(timestamp, "%H:%M:%S")]
# dt_latest2[, doy := lubridate::yday(date)]
# dt_latest2[, week := lubridate::week(date)]
# setorder(dt_latest2, timestamp, animal_id)
# setcolorder(dt_latest2, neworder = names(dt))
# 
# # Load manual data again
# dt_latest3 <- data.table::fread("outputs_3PS_trial3/temperature_data_Feb09-Feb11_manual.csv")
# names(dt_latest3) <- tolower(names(dt_latest3))
# 
# # Remove unused cols
# dt_latest3[, client := NULL]
# dt_latest3[, farm := NULL]
# dt_latest3[, id := NULL]
# 
# # Match dt columns
# dt_latest3[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# dt_latest3[, date := lubridate::as_date(timestamp)]
# dt_latest3[, time := format(timestamp, "%H:%M:%S")]
# dt_latest3[, doy := lubridate::yday(date)]
# dt_latest3[, week := lubridate::week(date)]
# setorder(dt_latest3, timestamp, animal_id)
# setcolorder(dt_latest3, neworder = names(dt))
# 
# # Join both data
# dt_combined <- rbind(dt, dt_latest2, dt_latest)
# 
# # Check counts of observations for ts
# obs_counts <- dt_combined[, .N, by = animal_id]
# to_remove_idx <- which(obs_counts$N < 144)
# to_remove_tag <- obs_counts[to_remove_idx, animal_id]
# 
# # remove tags with unsif
# dt_combined <- subset(dt_combined, animal_id %notin% to_remove_tag)
# dt_combined
# setorder(dt_combined, timestamp, animal_id)

# Time-series analysis
dt[, timestamp := NULL]
dt[, date_awst := NULL]
dt[, time_awst := NULL]

timeseries_ts <- data.table::copy(dt) |>
  dplyr::group_by(animal_id) |>
  time_decompose(temperature, method = "stl") |>
  anomalize(remainder) |>
  time_recompose() |>
  as.data.table()

message("\nFinished time-series analysis\n")

# Create features
features_dt <- create_features_inference(timeseries_ts)

message("\nFinished feature creation\n")

# Export data ready to use (nr1)
out <- copy(features_dt)
attr(out, "index_quo") <- NULL
attr(out, "index_time_zone") <- NULL
attr(out, ".Environment") <- NULL
attr(out, "groups") <- NULL
# str(model_dt_clean)

# Define columns used for training
train_cols <- 
  c("week_age", "trend", "Rolling_Avg_24h", "Rolling_Avg_30h", 
    "Rolling_Avg_48h","Rolling_SD_24h", "Rolling_SD_30h", "Rolling_SD_48h", 
    "Rolling_max_12h", "Rolling_max_24h", "Rolling_max_30h", "Rolling_max_48h", 
    "Rolling_min_24h", "Rolling_min_30h", "Rolling_min_48h", 
    "Rolling_range_24h", "Rolling_range_30h", "Rolling_range_48h")

# Remove NAs given rolling calcs
model_dt <- na.omit(out)
setorder(model_dt, animal_id, timestamp_awst)
model_dt[, week_age := lubridate::week(timestamp_awst)]

# filter dt only to train cols
model_dt_clean <- model_dt[, ..train_cols]

# Dummy label for inference
model_dt_clean[, target := factor(rep(0, .N))]  
model_dt_clean[1, target := factor(1)]

message("\nFinished model data manipulation\n")

# Load model
inference_model <- readRDS("outputs/ensemble_trained_on_trial2.rds")

message("\nML model successfully loaded.\n")

# Create mlr3 task for inference (classification)
task_inference <- TaskClassif$new(
  id = "ml_inference",
  backend = model_dt_clean,
  target = "target",  # No target for inference
  positive = "1" # Specify your positive class
)

# Run inference **without creating a task**
message("\nGet model predictions\n")
predictions <- inference_model$predict(task_inference)$classif.rpart.output

# Add predictions to dataset
model_dt[, prediction_prob := predictions$prob[, 1]]  # Assuming column 2 is "sick" probability
model_dt[, prediction_class := predictions$response]
model_dt[, date := lubridate::as_date(timestamp_awst)]
# model_dt <- arrow::read_parquet("outputs_3PS_trial_to_XSights/3PS_ML_inference_data_NR6_05-Feb-25-17:34:36.parquet")

# daily summary
daily_status_detailed <- 
  model_dt[,
           .(
             total_obs = .N,
             prop_sick = round(mean(prediction_class == "1"), 3),
             mean_prob = round(mean(prediction_prob), 3),
             sd_prob = round(sd(prediction_prob), 3),
             health_status = factor(ifelse(mean(prediction_class == "1") > 0.8, "sick", "ok"))
           ),
           by = .(animal_id, date)
  ]

x <- daily_status_detailed[,
  .(
    final_status = factor(
      ifelse(
        # Condition 1
        mean(mean_prob >= 0.5) > 0.5 & mean(sd_prob <= 0.1),
        "sick",
        ifelse(
          # Condition 2
          mean(mean_prob >= 0.5) > 0.5 & mean(sd_prob > 0.1),
          "alert",
          "ok"
        )
      ),
      levels = c("ok", "alert", "sick")  # optional, for consistent factor levels
    )
  ),
  by = .(animal_id, date = as.Date(date))
]

# data to export
message("\nDaily output summary complete\n")

out_status <- data.table::merge.data.table(daily_status_detailed, x)
out_status[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
setorder(out_status, date, animal_id)
setcolorder(out_status, c("animal_id", "dmac", "date"))

################################################################################
# # Ensure data is sorted by timestamp before applying rolling summary
# setorder(model_dt, animal_id, timestamp_awst)
# 
# # Rolling window summary over the past 144 observations
# rolling_summary <- model_dt[, 
#                             .(
#                               total_obs = .N,
#                               prop_sick = round(mean(tail(prediction_class == "1", 144)), 3),
#                               mean_prob = round(mean(tail(prediction_prob, 144)), 3),
#                               sd_prob = round(sd(tail(prediction_prob, 144)), 3),
#                               health_status = factor(ifelse(mean(tail(prediction_class == "1", 144)) > 0.8, "sick", "ok"))
#                             ), 
#                             by = .(animal_id, timestamp_awst)  # Keeping timestamp instead of calendar date
# ]
# 
# # Assign final status based on the last 144 observations
# x <- rolling_summary[,
#                      .(
#                        final_status = factor(
#                          ifelse(
#                            # Condition 1
#                            mean(mean_prob >= 0.5) > 0.5 & mean(sd_prob <= 0.1),
#                            "sick",
#                            ifelse(
#                              # Condition 2
#                              mean(mean_prob >= 0.5) > 0.5 & mean(sd_prob > 0.1),
#                              "alert",
#                              "ok"
#                            )
#                          ),
#                          levels = c("ok", "alert", "sick")  # Keep factor levels consistent
#                        )
#                      ),
#                      by = .(animal_id, timestamp_awst)
# ]
# 
# # Merge and clean up output
# out_status <- merge.data.table(rolling_summary, x, by = c("animal_id", "timestamp_awst"))
# out_status[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# setorder(out_status, timestamp_awst, animal_id)
# setcolorder(out_status, c("animal_id", "dmac", "timestamp_awst"))
# 
# message("\nRolling window output summary complete\n")
# )
# 
# -------
# # Ensure data is sorted
# setorder(out_status, animal_id, timestamp_awst)
# 
# # Get the latest timestamp per animal
# latest_timestamps <- out_status[, .(latest_time = max(timestamp_awst)), by = animal_id]
# 
# # Filter to keep only the last 24 hours before the latest timestamp
# last_24h_data <- out_status[latest_timestamps, 
#                             on = "animal_id"
# ][timestamp_awst >= (latest_time - hours(24))]
# 
# # Summarize to one value per animal_id
# summary_last_24h <- last_24h_data[, .(
#   total_obs = .N,
#   prop_sick = round(mean(final_status == "sick"), 3),
#   mean_prob = round(mean(mean_prob, na.rm = TRUE), 3),
#   sd_prob = round(sd(mean_prob, na.rm = TRUE), 3),
#   final_status = factor(
#     ifelse(mean(mean_prob >= 0.5, na.rm = TRUE) > 0.5 & mean(sd_prob <= 0.1, na.rm = TRUE), "sick",
#            ifelse(mean(mean_prob >= 0.5, na.rm = TRUE) > 0.5 & mean(sd_prob > 0.1, na.rm = TRUE), "alert", "ok")),
#     levels = c("ok", "alert", "sick")
#   ),
#   latest_time = max(timestamp_awst)  # Include for reference
# ), by = .(animal_id)]
# 
# # Add dmac identifier and reorder columns
# summary_last_24h[, dmac := factor(substring(animal_id, nchar(animal_id) - 3, nchar(animal_id)))]
# summary_last_24h[, date := as.Date(max(latest_time))]
# setorder(summary_last_24h, latest_time, animal_id)
# setcolorder(summary_last_24h, c("animal_id", "dmac", "latest_time"))
# 
# message("\nFinal 24h summary per animal_id complete\n")

# Uncomment if you want to save the output
# fwrite(summary_last_24h, "summary_last_24h.csv")

# Save or export as needed
# fwrite(last_24h_data, "last_24h_data.csv")  # Uncomment to save as CSV
############################################################################################

# Return only up to the complete record for the day
# out_status <- subset(out_status, date == Sys.Date())
out_status_day <- subset(out_status, date == max(date-1, na.rm = T))

# Export results
timestamp_export <- format(Sys.time(), "%d-%b-%y-%X")

# Export raw summary (all obs)
data.table::fwrite(out_status, 
                   paste0("outputs_3PS_trial_to_XSights/",
                          "3PS_ML_health_status_NR6_", 
                          timestamp_export,
                          ".csv")
)


# Export only sick and alert labels
data.table::fwrite(subset(out_status_day, final_status %notin% c("ok")), 
                   paste0("outputs_3PS_trial_to_XSights/",
                          "3PS_ML_health_status_NR6_", 
                          timestamp_export,
                          "_sick_only.csv")
)

# Export model dataset and predictions (prob and class)
arrow::write_parquet(model_dt, 
                     paste0("outputs_3PS_trial_to_XSights/",
                            "3PS_ML_inference_data_NR6_", 
                            timestamp_export,
                            ".parquet"))

message("\nInference completed and health status saved.\n")
