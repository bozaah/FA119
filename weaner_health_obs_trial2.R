library(data.table)
library(ggplot2)
library(ranger)

source("R/utils.R")

# set the path to your folder
data_dir <- "data/health_observations_3PS"

# list all CSV files
csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE) |> sort()

# read and combine
obs_dt <- rbindlist(lapply(csv_files, fread), fill = TRUE)
names(obs_dt) <- c("Date",
                   "Time",
                   "Room",
                   "Pen #",
                   "dmac",
                   "Alert",
                   "Condition",
                   "BCS",
                   "Rectal Temp",
                   "Diarrhoea",
                   "Respiratory",
                   "Coughing",
                   "Neuro",
                   "Ataxia",
                   "Paddling",
                   "Seizure",
                   "Nasal Disc.",
                   "Lameness",
                   "# Swollen Joints",
                   "left front",
                   "right front",
                   "left hind",
                   "right hind",
                   "Skin Lesion",
                   "Tail Bite",
                   "Abscess",
                   "Prolapse",
                   "Hernia",
                   "Bloating",
                   "hema-left",
                   "hema-right",
                   "treated prev",
                   "med1",
                   "med2",
                   "med3",
                   "tosick",
                   "fromsick",
                   "Euthanise",
                   "User name",
                   "Comment")

dt <- janitor::clean_names(obs_dt)

# preview combined data
print(paste("Combined", length(csv_files), "files."))
print(dim(dt))
head(dt)

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

# start and end info
date_start <- min(dt$time_stamp)
date_end <- max(dt$time_stamp)

# Check RED alerts data
dt[grep("^RED", dt$alert)]

## Cummulative alerts
# Create a binary column indicating the presence of an alert
dt[, alert_flag := as.integer(!is.na(alert) & alert %notin% c("None", "<NA>"))]

# Calculate cumulative alerts for each dmac ID over time
dt[, cumulative_alerts := cumsum(alert_flag), by = dmac]

# Check data
str(dt)
# Hmisc::describe(dt)

# Step 2: Ensure condition is a factor and in the right order
health_dt <- dt[, condition := factor(condition, levels = c("Healthy", "Sick", "Dead"))]

# Step 3: Define health indicator columns, adding `bcs`
health_columns <- c("diarrhoea", "respiratory", "coughing", "neuro", "ataxia", 
                    "paddling", "seizure", "nasal_disc", "lameness", 
                    "number_swollen_joints", "left_front", "right_front", 
                    "left_hind", "right_hind", "skin_lesion", "tail_bite",
                    "abscess", "prolapse", "hernia", "bloating", 
                    "hema_left", "hema_right", "treated_prev",
                    "med1", "med2", "med3", "tosick", "fromsick", "euthanise", "cumulative_alerts")

# Ensure all health_columns are numeric
health_dt[, (health_columns) := lapply(.SD, as.numeric), .SDcols = health_columns]

ggplot(dt, aes(x = bcs)) +
  geom_histogram(stat = "count") +
  facet_grid(. ~ condition) +
  theme_light(base_size = 16)

ggplot(dt, aes(bcs)) +
  geom_histogram(stat = "count") +
  facet_grid(. ~ target) +
  theme_light(base_size = 16)



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

# No missing data
model_dt_clean <- dt[!is.na(dt$condition)]
model_dt_clean <- model_dt_clean[condition %in% c("Healthy", "Sick")]
model_dt_clean[, target := ifelse(condition == "Healthy", 0, 1)]

# Step 6: Define formula and train Random Forest with `condition` as target
formula <- as.formula(paste("target ~", paste(health_columns, collapse = " + ")))

rf_model <- ranger(formula, 
                   data = model_dt_clean, 
                   importance = "impurity",
                   num.trees = 300, 
                   max.depth = 3, 
                   min.node.size = 3,
                   probability = TRUE,
                   num.threads = parallel::detectCores(),
                   oob.error = TRUE,
                   verbose = TRUE)

# get weights - Extract feature importance and normalize to 0-1 range
rf_importances <- rf_model$variable.importance
rf_weights <- (rf_importances - min(rf_importances, na.rm = TRUE)) / (max(rf_importances, na.rm = TRUE) - min(rf_importances, na.rm = TRUE))

plot_var_importance(rf_weights)
