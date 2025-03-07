# Load libraries
library("dplyr")
library("lubridate")
library("stringr")
library("ggplot2")

# Laod funcs
source("R/utils.R")

# Load data with last records
# Second work sheet of the .xlsx file
batch3_focus_last_record <- 
  readxl::read_excel("data/3PS_batch3_100focusPigs_temps.xlsx", 
                     sheet = "Focus pigs last record", 
                     col_types = c("numeric", "text", "text"))



# Convert to timestamp and split date, time data
batch3_focus_last_record <-
  batch3_focus_last_record |>
  dplyr::mutate(
    TIMESTAMP_UTC = ymd_hms(gsub("\\.000\\+0800", "", LAST_RECORD_TIMESTAMP), 
                            tz = "UTC"),
    TIMESTAMP_AWST = with_tz(TIMESTAMP_UTC, "Australia/Perth"),
    DATE_AWST = as.Date(TIMESTAMP_AWST),
    TIME_AWST = format(TIMESTAMP_AWST, "%H:%M:%S")
  ) |>
  janitor::clean_names()

# Remove original timestamp
batch3_focus_last_record$last_record_timestamp <- NULL
str(batch3_focus_last_record)

# Now, import temperature data
batch3_focus_tag_temperature <- 
  readxl::read_excel("data/3PS_batch3_100focusPigs_temps.xlsx",
                     sheet = "3PS_batch3_100focusPigs_temps")

# Tidy up timestamp and location ID data
# Split the LOCATION_ID column into two new columns
batch3_focus_tag_temperature <- 
  batch3_focus_tag_temperature |>
  # tidyr::separate(
  #   LOCATION_ID, into = c("LOCATION_CODE", "LOCATION_NAME"), sep = "---"
  #   ) |>
  mutate(
    LOCATION_CODE = ifelse(str_detect(LOCATION_ID, "---"), 
                           str_extract(LOCATION_ID, "^[^-]+"), LOCATION_ID),
    LOCATION_NAME = ifelse(str_detect(LOCATION_ID, "---"), 
                           str_extract(LOCATION_ID, "(?<=---).+$"), NA),
    TIMESTAMP_UTC = ymd_hms(gsub("\\.000\\+0800", "", TIMESTAMP), 
                            tz = "UTC"),
    TIMESTAMP_AWST = with_tz(TIMESTAMP_UTC, "Australia/Perth"),
    DATE_AWST = as.Date(TIMESTAMP_AWST),
    TIME_AWST = format(TIMESTAMP_AWST, "%H:%M:%S")
  ) |> 
  janitor::clean_names()

str(batch3_focus_tag_temperature)


# Fill in NA in orange_tag_number in the temp data with those in the last record
batch3_focus_temp <- 
  batch3_focus_tag_temperature |>
  dplyr::rename(orange_tag = orange_tag_number) |>
  left_join(batch3_focus_last_record, by = "animal_id") |>
  select(-matches("\\.y$")) |>
  rename(timestamp_utc = timestamp_utc.x,
         timestamp_awst =timestamp_awst.x,
         date_awst = date_awst.x,
         time_awst= time_awst.x) |>
  mutate(
    animal_id = factor(animal_id),
    location_code = factor(location_code),
    orange_tag_number = factor(orange_tag_number)
  ) |>
  select(-c(orange_tag, timestamp_utc, location_id, timestamp, location_name))

str(batch3_focus_temp)

# Export clean df
# arrow::write_parquet(batch3_focus_temp,
                     # "data/processed/batch3_focus_pig_temperature.parquet")

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

# Check observations for each pig
obs_counts_temp <- 
  batch3_focus_NR |> 
  group_by(animal_id) |>
  count() |>
  arrange(n) |>
  ungroup()

# Check if pig E79CF8753649 is present in the NR
plot_pig_temp_batch3("E79CF8753649", batch3_focus_NR)

# Check pig with most observations C640371F4CA6
plot_pig_temp_batch3("C640371F4CA6", batch3_focus_NR)

# Check pig with < 100 observations (DD6160433693 and C99F7F04ED2B)
cowplot::plot_grid(
  plotlist = list(plot_pig_temp_batch3("DD6160433693", batch3_focus_NR),
                  plot_pig_temp_batch3("C99F7F04ED2B", batch3_focus_NR),
                  plot_pig_temp_batch3("E79CF8753649", batch3_focus_NR),
                  plot_pig_temp_batch3("C640371F4CA6", batch3_focus_NR))
)

# Calculate stats for all pigs
pig_stats <- 
  calculate_stats(batch3_focus_NR,
                  "temperature",
                  "animal_id")

pig_stats |>
  ggplot(aes(x = sd)) +
  geom_density() +
  # facet_wrap(. ~ animal_id) +
  theme_light(base_size = 14) 

## High SD pigs ----
high_sd_pig_tags <-
  subset(pig_stats, sd >= 1.55) |>
  pull(animal_id) |>
  as.character()

# Filter only high sd pigs
high_sd_pig_df <- subset(batch3_focus_NR, animal_id %in% high_sd_pig_tags) 

# Create plots for all
high_sd_pig_plots <-
  lapply(high_sd_pig_tags,
         plot_pig_temp_batch3,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = high_sd_pig_plots, 
  nrow = 3
)

# Density plots
high_sd_pig_plots_density <-
  lapply(high_sd_pig_tags,
         plot_pig_temp_batch3_density,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = high_sd_pig_plots_density, 
  nrow = 3
)

## Low SD pigs ----
low_sd_pig_tags <-
  subset(pig_stats, sd <= 1.25) |>
  pull(animal_id) |>
  as.character()

# Filter only low sd pigs
low_sd_pig_df <- subset(batch3_focus_NR, animal_id %in% low_sd_pig_tags) 

# Create plots for all
low_sd_pig_plots <-
  lapply(low_sd_pig_tags,
         plot_pig_temp_batch3,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = low_sd_pig_plots, 
  nrow = 4
)

# density plots
low_sd_pig_plots_density <-
  lapply(low_sd_pig_tags,
         plot_pig_temp_batch3_density,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = low_sd_pig_plots_density, 
  nrow = 4
)

## Middle pigs ----
middle_sd_pig_df <- 
  subset(batch3_focus_NR, animal_id %notin% c(low_sd_pig_tags, high_sd_pig_tags)) 

# get tags
middle_sd_pig_tags <-
  middle_sd_pig_df |> 
  pull(animal_id) |>
  unique() |>
  as.character()

# Create plots for all
middle_sd_pig_df <-
  lapply(middle_sd_pig_tags,
         plot_pig_temp_batch3,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = middle_sd_pig_df, 
  nrow = 5
)

# Density plots
middle_sd_pig_plots_density <-
  lapply(middle_sd_pig_tags,
         plot_pig_temp_batch3_density,
         batch3_focus_NR)

cowplot::plot_grid(
  plotlist = middle_sd_pig_plots_density, 
  nrow = 5
)




