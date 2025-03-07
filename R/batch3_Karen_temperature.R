# Load Karen's batch 3 files 
library(data.table)
library(ggplot2) 

# temp data
temp_data <- read.csv("data/batch3_Karen_temperature.csv") |>
  janitor::clean_names() |>
  as.data.table()

# Identify the columns to keep as id variables
id_vars <- c("pen", "pig_no", "xiot_tag_id", "tag_id", "sex", "ear_temp", "tympanic_temp", "liveweight")

# Identify the columns to melt (temperature measurements)
measure_vars <- setdiff(names(temp_data), id_vars)

# Reshape the data from wide to long format using melt
temp_data_long <- melt(
  temp_data,
  id.vars = id_vars,
  measure.vars = measure_vars,
  variable.name = "date",
  value.name = "temperature",
  variable.factor = FALSE
)

# Clean up the 'date' column by removing the 'x' prefix
temp_data_long[, date := sub("^x", "", date)]

# Append a year to the date strings (adjust the year as necessary)
temp_data_long[, date := paste0(date, "_2024")]

# Convert 'date' to Date format
temp_data_long[, date := as.Date(date, format = "%d_%b_%Y")]

# Handle special entries in 'temperature' (e.g., 'DIP (28/2)', 'SP (since 21/2)', 'low')
# Extract numeric temperature values and notes
temp_data_long[, temp_value := as.numeric(gsub("[^0-9\\.]", "", temperature))]
temp_data_long[, notes := ifelse(grepl("[A-Za-z]", temperature), temperature, NA)]

# Replace 'temperature' with 'temp_value' and remove 'temp_value' column
temp_data_long[, temperature := temp_value]
temp_data_long[, temp_value := NULL]
temp_data_long[, xiot_tag_id := factor(xiot_tag_id, levels = unique(xiot_tag_id))]
temp_data_long[, pen := factor(pen, levels = unique(pen))]
temp_data_long[, pig_no := factor(pig_no, levels = unique(pig_no))]
temp_data_long[, tag_id := factor(tag_id, levels = unique(tag_id))]
temp_data_long[, sex := factor(sex, levels = unique(sex))]
temp_data_long[, ear_temp := factor(ear_temp, levels = unique(ear_temp))]
temp_data_long[, tympanic_temp := factor(tympanic_temp, levels = unique(tympanic_temp))]
temp_data_long[, liveweight := factor(liveweight, levels = unique(liveweight))]

# Remove rows where both 'temperature' and 'notes' are NA
# temp_data_long <- temp_data_long[!is.na(temperature) | !is.na(notes)]

temp_data_long
str(temp_data_long)

# subset ear and tympanic temp and liveweight datasets
ear_temperature_dt <- subset(temp_data_long, ear_temp == 1)
tympanic_temperature_dt <- subset(temp_data_long, tympanic_temp == 1)
liveweight_temperature_dt <- subset(temp_data_long, liveweight == 1)

# Check some of the data
with(tympanic_temperature_dt, plot(temperature ~ tag_id))

for (i in unique(tympanic_temperature_dt$pen)) {
  with(subset(tympanic_temperature_dt, pen == i), plot(temperature ~ date + tag_id, main = paste0("Pen ", i)))
}
# pen with potentially wrong temps
# 24, 20, 9, 1

wrong_temps_idx <- 
  which(tympanic_temperature_dt$pen %in% c(1, 9, 20, 24) & tympanic_temperature_dt$temperature > 45)

tympanic_temperature_dt[wrong_temps_idx, temperature := NA_real_]

tympanic_temperature_dt[wrong_temps_idx, ]


## Health scores
# health scores
health_scores <- read.csv("data/batch3_Karen_health_scores.csv") |>
  janitor::clean_names() |>
  as.data.table()

str(health_scores)

focus_pigs <- subset(health_scores, focus_pig == 1)
View(focus_pigs)

is.(subset(focus_pigs, condition == "dead")$body_condition)
