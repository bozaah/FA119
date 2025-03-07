#' Add %notin% Function
#' Negates `%in%` for easier (mis)matching.
#' @param x A character string to match.
#' @param table A table containing values to match `x` against.
#' @return A logical vector, indicating if a mismatch was located for any
#'  element of x: thus the values are TRUE or FALSE and never NA.
#' @keywords Internal
#' @noRd
#' 
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' Calculate statistical features for animal temperature data
#' @description This function computes key statistical features from a dataset 
#' of temperature measurements for a single tag ID
#' @param data A data frame or data table containing at least a column 
#' named `temperature`, representing recorded temperature values for an animal.
#' @return A data table with the following calculated features:
#' \describe{
#'   \item{mean_temp}{The mean of the temperature values.}
#'   \item{median_temp}{The median of the temperature values.}
#'   \item{sd_temp}{The standard deviation of the temperature values.}
#'   \item{min_temp}{The minimum temperature value.}
#'   \item{max_temp}{The maximum temperature value.}
#'   \item{range_temp}{The range of temperature values (max - min).}
#'   \item{skewness_temp}{The skewness of the temperature distribution.}
#'   \item{kurtosis_temp}{The kurtosis of the temperature distribution.}
#' }
#'
calculate_features <- function(data) {
  return(data.table(
    mean_temp = mean(data$temperature, na.rm = TRUE),
    median_temp = median(data$temperature, na.rm = TRUE),
    sd_temp = sd(data$temperature, na.rm = TRUE),
    min_temp = min(data$temperature, na.rm = TRUE),
    max_temp = max(data$temperature, na.rm = TRUE),
    range_temp = max(data$temperature, na.rm = TRUE) - min(data$temperature, na.rm = TRUE),
    skewness_temp = e1071::skewness(data$temperature, na.rm = TRUE),
    kurtosis_temp = e1071::kurtosis(data$temperature, na.rm = TRUE)
  ))
}

#' Convert 10-minute intervals to hours
#' @description This function converts time intervals from 10-minute units to hours.
#' @param x A numeric vector representing time intervals in 10-minute units.
#' @return A numeric vector representing the equivalent time intervals in hours.
#' @examples
#' # Convert 10-minute intervals to hours
#' to_hours(6)  # Returns 1 (6 * 10 minutes = 1 hour)
#' to_hours(c(3, 12))  # Returns c(0.5, 2)
#' 
to_hours <- function(x){return(x * 6)}

# Create temperature features from ear tag observations (10 min)
#' Create a set of temperature features for a data frame.
#' @description This function creates a variety of features related to the 
#' temperature from the input dataframe, including differences between 
#' consecutive observations, ranges, and more.
#' @param df A input `data.table` Must contain columns 'timestamp' and 'temperature'.
#' @return A new `data.table` with the created features

create_features <- function(data) {
  # Ensure data is ordered by animal_id and timestamp
  setorder(data, animal_id, timestamp)
  
  # Create temperature differences
  data[, Temp_Diff_3h := temperature - shift(temperature, n = 18, type = "lag"), by = animal_id]  # 18 = 3h * 6 (10-min intervals)
  data[, Temp_Diff_6h := temperature - shift(temperature, n = 36, type = "lag"), by = animal_id] 
  data[, Temp_Diff_12h := temperature - shift(temperature, n = 72, type = "lag"), by = animal_id]  
  data[, Temp_Diff_24h := temperature - shift(temperature, n = 144, type = "lag"), by = animal_id]  
  data[, Temp_Diff_30h := temperature - shift(temperature, n = 180, type = "lag"), by = animal_id] 
  data[, Temp_Diff_48h := temperature - shift(temperature, n = 288, type = "lag"), by = animal_id] 
  
  # Create temperature ratios
  data[, Temp_Ratio_3h := temperature / rollmean(temperature, k = 18, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_6h := temperature / rollmean(temperature, k = 36, fill = NA, align = "right"), by = animal_id]
  data[, Temp_Ratio_12h := temperature / rollmean(temperature, k = 72, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_24h := temperature / rollmean(temperature, k = 144, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_30h := temperature / rollmean(temperature, k = 180, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_48h := temperature / rollmean(temperature, k = 288, fill = NA, align = "right"), by = animal_id]  
  
  # Create lagged temperature features
  data[, Lag_Temp_3h := shift(temperature, n = 18, type = "lag"), by = animal_id]
  data[, Lag_Temp_6h := shift(temperature, n = 36, type = "lag"), by = animal_id]
  data[, Lag_Temp_12h := shift(temperature, n = 72, type = "lag"), by = animal_id]
  data[, Lag_Temp_24h := shift(temperature, n = 144, type = "lag"), by = animal_id]
  data[, Lag_Temp_30h := shift(temperature, n = 180, type = "lag"), by = animal_id]
  data[, Lag_Temp_48h := shift(temperature, n = 288, type = "lag"), by = animal_id]
  
  # Create rolling statistics
  data[, Rolling_Avg_3h := rollmean(temperature, k = 18, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_6h := rollmean(temperature, k = 36, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_12h := rollmean(temperature, k = 72, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_24h := rollmean(temperature, k = 144, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_30h := rollmean(temperature, k = 180, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_48h := rollmean(temperature, k = 288, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_SD_3h := rollapply(temperature, width = 18, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_6h := rollapply(temperature, width = 36, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_12h := rollapply(temperature, width = 72, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_24h := rollapply(temperature, width = 144, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_30h := rollapply(temperature, width = 180, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_48h := rollapply(temperature, width = 288, FUN = sd, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_max_3h := rollapply(temperature, width = 18, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_6h := rollapply(temperature, width = 36, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_12h := rollapply(temperature, width = 72, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_24h := rollapply(temperature, width = 144, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_30h := rollapply(temperature, width = 180, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_48h := rollapply(temperature, width = 288, FUN = max, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_min_3h := rollapply(temperature, width = 18, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_6h := rollapply(temperature, width = 36, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_12h := rollapply(temperature, width = 72, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_24h := rollapply(temperature, width = 144, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_30h := rollapply(temperature, width = 180, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_48h := rollapply(temperature, width = 288, FUN = min, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_range_3h := rollapply(temperature, width = 18, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_6h := rollapply(temperature, width = 36, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_12h := rollapply(temperature, width = 72, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_24h := rollapply(temperature, width = 144, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_30h := rollapply(temperature, width = 180, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_48h := rollapply(temperature, width = 288, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  
  return(data)
}

#' Add theoretical temperature classes to a data table
#' @description This function adds columns to a `data.table` indicating 
#' theoretical temperature categories based on rolling 3-hour averages. It also 
#' classifies observations into broad classes ("watch", "sick", "optimal", or 
#' "normal") based on these derived conditions.
#' @param dt A `data.table` containing a numeric column `Rolling_Avg_3h` 
#' representing the 3-hour rolling average of temperature observations.
#' @return The input `data.table` with additional columns:
#'   \itemize{
#'     \item \code{hypo_crit}: 1 if 32°C < temperature ≤ 35°C, otherwise 0.
#'     \item \code{hypo_severe}: 1 if temperature ≤ 32°C, otherwise 0.
#'     \item \code{fever_crit}: 1 if 40°C ≤ temperature < 42°C, otherwise 0.
#'     \item \code{fever_severe}: 1 if temperature ≥ 42°C, otherwise 0.
#'     \item \code{optimum_temp}: 1 if 38.5°C ≤ temperature ≤ 39.5°C, otherwise 0.
#'     \item \code{class}: A string column indicating the overall classification 
#'       (\code{"watch"}, \code{"sick"}, \code{"optimal"}, or \code{"normal"}).
#'   }
#
add_temperature_classes <- function(dt) {
  # Ensure input is a data.table
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data.table.")
  }
  
  out <- data.table::copy(dt)
  # Add columns for temperature categories
  out[, hypo_crit := fifelse(Rolling_Avg_3h <= 35 & Rolling_Avg_3h > 32, 1, 0)]
  out[, hypo_severe := fifelse(Rolling_Avg_3h <= 32, 1, 0)]
  out[, fever_crit := fifelse(Rolling_Avg_3h >= 40 & Rolling_Avg_3h < 42, 1, 0)]
  out[, fever_severe := fifelse(Rolling_Avg_3h >= 42, 1, 0)]
  out[, optimum_temp := fifelse(Rolling_Avg_3h >= 38.5 & Rolling_Avg_3h <= 39.5, 1, 0)]
  
  # Add class column based on conditions
  out[, class := fifelse(
    fever_crit == 1 | hypo_crit == 1, "watch",
    fifelse(
      fever_severe == 1 | hypo_severe == 1, "sick",
      fifelse(optimum_temp == 1, "optimal", "normal")
    )
  )]
  
  # Return modified data.table
  return(out)
}

#' Calculate temperature features by group or entire dataset
#' This function calculates statistical features for a dataset. It can operate 
#' on the entire dataset or calculate statistics grouped by specified columns.
#' @param dt A `data.table` containing the dataset.
#' @param value_col A string specifying the column name of the numeric variable
#' for which to calculate statistics.
#' @param group_cols An optional vector of column names to group by. If not 
#' provided, statistics are calculated for the entire dataset.
#' @return A `data.table` containing the calculated statistics:
#'   \itemize{
#'     \item \code{min}: Minimum value of the numeric column.
#'     \item \code{max}: Maximum value of the numeric column.
#'     \item \code{median}: Median of the numeric column.
#'     \item \code{avg}: Mean of the numeric column.
#'     \item \code{sd}: Standard deviation of the numeric column.
#'     \item \code{var}: Variance of the numeric column.
#'     \item \code{iqr}: Interquartile range of the numeric column.
#'   }
#'   If `group_cols` are provided, the result includes one row per group.
#'   
calculate_stats_combined <- function(dt, 
                                     value_col, 
                                     group_cols = NULL) {
  
  # Ensure input is a data.table
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data.table.")
  }
  
  if (is.null(value_col) | missing(value_col) | is.na(value_col)) {
    stop("You must provide a variable to `value_col`")
  }
  
  # Define the statistical calculations
  calc_stats <- function(data) {
    return(data.table(
      min = min(data$observed, na.rm = TRUE),
      max = max(data$observed, na.rm = TRUE),
      median = median(data$observed, na.rm = TRUE),
      avg = mean(data$observed, na.rm = TRUE),
      sd = sd(data$observed, na.rm = TRUE),
      var = var(data$observed, na.rm = TRUE),
      iqr = IQR(data$observed, na.rm = TRUE)
    ))
  }
  
  # If group_cols is provided, calculate stats by group
  if (!is.null(group_cols)) {
    return(
      dt[, .(
        min = min(get(value_col), na.rm = TRUE),
        max = max(get(value_col), na.rm = TRUE),
        median = median(get(value_col), na.rm = TRUE),
        avg = mean(get(value_col), na.rm = TRUE),
        sd = sd(get(value_col), na.rm = TRUE),
        var = var(get(value_col), na.rm = TRUE),
        iqr = IQR(get(value_col), na.rm = TRUE)
      ), by = group_cols]
    )
  } else if (is.null(group_cols)) {
    # Otherwise, calculate stats for the entire dataset
    return(calc_stats(dt))
  }
}


plot_pig_temp_batch3 <- function(tag,
                                 df) {
  # get tag for plot title
  id = substr(tag, nchar(tag) - 3, nchar(tag))
  
  subset(df, animal_id == tag) |>
    ggplot(aes(x = timestamp_awst, y = temperature, group = animal_id)) +
    geom_line(linewidth = 1.2, alpha = .8) +
    scale_y_continuous(limits = c(20, 43)) +
    scale_x_datetime(breaks = scales::date_breaks("5 days"),
                     labels = scales::date_format("%d-%b-%y (%a)")) +
    labs(title = paste0("tag ", id), x = "", y = "XioT temp\n") +
    theme_light(base_size = 14) +
    theme(axis.text.x = element_text(angle = 330))
  
}


plot_pig_temp_batch3_density <- function(tag,
                                         df) {
  # get tag for plot title
  id = substr(tag, nchar(tag) - 3, nchar(tag))
  
  health_tags <- c("C1FC0C4CCE86",
                   "C20A449A63EB",
                   "C24D5B17FD18",
                   "C80B8EA4514C",
                   "CB3CF1059D7F",
                   "CB8B523FAB43",
                   "CD3062ABA29D",
                   "CFBDCC8AE5C3",
                   "CFEC1116F692",
                   "D4227C6BD418",
                   "D73EF9925BFE",
                   "D784DAA8B677",
                   "D821AF0832D8",
                   "D88118773E44",
                   "DC21C32F027A",
                   "DD88A0D815D6",
                   "E23E9AADF65E",
                   "E354659732CF",
                   "E5BE5A43227B",
                   "E72BD397F951",
                   "E73B53CF8538",
                   "EF54D1A4E3CB",
                   "F1D082A91E93",
                   "F26E5CB58E1D",
                   "F55BCE24ABF8")
  
  ggplot(subset(df, animal_id %in% health_tags), aes(x = temperature)) +
    geom_density(aes(y = after_stat(density)), colour = "firebrick", fill = "firebrick", linewidth = .7, alpha = .4) +  # Density of the whole data
    geom_density(data = subset(df, animal_id == tag), aes(y = after_stat(density)), linewidth = .9, alpha = .8) +  # Density of the group 
    scale_x_continuous(limits = c(28, 45.5)) +
    labs(title = paste0("tag ", id), y = "", x = "XioT temp\n") +
    theme_light(base_size = 14) 
}


create_features_ts <- function(data) {
  # Ensure data is ordered by animal_id and timestamp
  setorder(data, animal_id, timestamp)
  
  # Create temperature differences
  data[, Temp_Diff_3h := observed - shift(observed, n = 18, type = "lag"), by = animal_id]  # 18 = 3h * 6 (10-min intervals)
  data[, Temp_Diff_6h := observed - shift(observed, n = 36, type = "lag"), by = animal_id] 
  data[, Temp_Diff_12h := observed - shift(observed, n = 72, type = "lag"), by = animal_id]  
  data[, Temp_Diff_24h := observed - shift(observed, n = 144, type = "lag"), by = animal_id]  
  data[, Temp_Diff_30h := observed - shift(observed, n = 180, type = "lag"), by = animal_id] 
  data[, Temp_Diff_48h := observed - shift(observed, n = 288, type = "lag"), by = animal_id] 
  
  # Create observed ratios
  data[, Temp_Ratio_3h := observed / rollmean(observed, k = 18, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_6h := observed / rollmean(observed, k = 36, fill = NA, align = "right"), by = animal_id]
  data[, Temp_Ratio_12h := observed / rollmean(observed, k = 72, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_24h := observed / rollmean(observed, k = 144, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_30h := observed / rollmean(observed, k = 180, fill = NA, align = "right"), by = animal_id]  
  data[, Temp_Ratio_48h := observed / rollmean(observed, k = 288, fill = NA, align = "right"), by = animal_id]  
  
  # Create lagged observed features
  data[, Lag_Temp_3h := shift(observed, n = 18, type = "lag"), by = animal_id]
  data[, Lag_Temp_6h := shift(observed, n = 36, type = "lag"), by = animal_id]
  data[, Lag_Temp_12h := shift(observed, n = 72, type = "lag"), by = animal_id]
  data[, Lag_Temp_24h := shift(observed, n = 144, type = "lag"), by = animal_id]
  data[, Lag_Temp_30h := shift(observed, n = 180, type = "lag"), by = animal_id]
  data[, Lag_Temp_48h := shift(observed, n = 288, type = "lag"), by = animal_id]
  
  # Create rolling statistics
  data[, Rolling_Avg_3h := rollmean(observed, k = 18, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_6h := rollmean(observed, k = 36, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_12h := rollmean(observed, k = 72, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_24h := rollmean(observed, k = 144, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_30h := rollmean(observed, k = 180, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_48h := rollmean(observed, k = 288, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_SD_3h := rollapply(observed, width = 18, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_6h := rollapply(observed, width = 36, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_12h := rollapply(observed, width = 72, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_24h := rollapply(observed, width = 144, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_30h := rollapply(observed, width = 180, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_48h := rollapply(observed, width = 288, FUN = sd, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_max_3h := rollapply(observed, width = 18, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_6h := rollapply(observed, width = 36, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_12h := rollapply(observed, width = 72, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_24h := rollapply(observed, width = 144, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_30h := rollapply(observed, width = 180, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_48h := rollapply(observed, width = 288, FUN = max, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_min_3h := rollapply(observed, width = 18, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_6h := rollapply(observed, width = 36, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_12h := rollapply(observed, width = 72, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_24h := rollapply(observed, width = 144, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_30h := rollapply(observed, width = 180, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_48h := rollapply(observed, width = 288, FUN = min, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_range_3h := rollapply(observed, width = 18, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_6h := rollapply(observed, width = 36, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_12h := rollapply(observed, width = 72, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_24h := rollapply(observed, width = 144, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_30h := rollapply(observed, width = 180, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_48h := rollapply(observed, width = 288, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  
  return(data)
}

#' One-Hot encode a categorical column
#' @description This function performs one-hot encoding on a specified 
#' categorical column in a `data.table`.
#' @param dt A `data.table` containing the dataset.
#' @param col A string specifying the name of the column to one-hot encode.
#' @param drop_col A `boolean` specifying if the original column should be 
#' dropped, defaults to `TRUE`.
#' @return The input `data.table` with additional one-hot encoded columns for 
#' each level of the specified column.
#' 
one_hot_encode <- function(dt, col, drop_col = TRUE) {
  # Ensure input is a data.table
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data.table.")
  }
  
  # Get unique levels of the column
  levels <- unique(dt[[col]])
  
  # Create one-hot encoded columns
  for (level in levels) {
    dt[, paste0(col, "_", level) := as.integer(get(col) == level)]
  }
  
  # Optionally drop the original column 
  if (isFALSE(drop_col)) {
    return(dt)
  } else {
    return(dt[, (col) := NULL])
  }
}

#' Plot variable importance
#' This function creates a bar plot to visualize variable importance for a model.
#' @param importance A named numeric vector where names are variable names and 
#' values are importance scores.
#' @param top_n The number of top features to display (default: 20).
#' @return A ggplot2 object showing variable importance.
#'
plot_var_importance <- function(importance,
                                top_n = 20) {
  
  # Convert named vector to data.table
  importance_dt <- data.table(
    variable = names(importance),
    importance = as.numeric(importance)
  )
  
  # Filter top N features
  importance_dt <- importance_dt[order(-importance)][1:top_n]
  
  # Create the ggplot
  ggplot(importance_dt, aes(x = reorder(variable, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "grey70", alpha = 0.7) +
    scale_fill_viridis_d(option = "D", name = "") +
    coord_flip() +
    labs(
      title = "Variable importance",
      x = "Features\n",
      y = "\nImportance"
    ) +
    theme_light(base_size = 16) 
}

#' Train and evaluate machine learning models (mlr3)
#' @description This function trains a machine learning model using a specified
#' learner and evaluates its performance on a test set using accuracy as the 
#' evaluation metric.
#' @param learner An `mlr3::Learner` model object that supports the `train` 
#' and `predict` 
#' methods.
#' @param training A training dataset formatted as expected by the learner.
#' @param testing A testing dataset used for evaluation after training.
#' @return A list containing:
#' \describe{
#'   \item{learner}{The trained model object.}
#'   \item{test_accuracy}{The accuracy score of the model on the test set.}
#'   \item{prediction_test}{The prediction object containing detailed results.}
#' }
#'
train_and_evaluate <- function(learner, 
                               training,
                               testing) {
  # Train the model
  learner$train(training)
  
  # Predict on the original test set from the task
  prediction_test <- learner$predict(testing)
  
  # Calculate the accuracy on the original test set
  test_accuracy <- prediction_test$score(msr("classif.fbeta"))
  
  # Return the trained learner and the test accuracy
  return(list(learner = learner, 
              test_accuracy = test_accuracy, 
              prediction_test = prediction_test))
}


#' Calculate evaluation metrics for classification models
#' @description This function computes various performance metrics for a 
#' classification model's predictions.
#' @param predictions An object containing the predicted results from a 
#' classification model.
#' @return A data frame with calculated evaluation metrics:
#' \describe{
#'   \item{AUC}{Area Under the Curve (AUC).}
#'   \item{Recall}{Recall or sensitivity of the model.}
#'   \item{Sensitivity}{Same as recall, proportion of true positives identified.}
#'   \item{Specificity}{Proportion of true negatives identified.}
#'   \item{False_Positive_Rate}{False positive rate (FPR).}
#'   \item{False_Negative_Rate}{False negative rate (FNR).}
#'   \item{True_Positive_Rate}{True positive rate (TPR).}
#'   \item{True_Negative_Rate}{True negative rate (TNR).}
#'   \item{Classif_error}{Overall classification error.}
#'   \item{Precision}{Precision of the model (positive predictive value).}
#' }
#'
calculate_metrics <- function(predictions) {
  # Calculate each metric
  f1_score <- predictions$score(msr("classif.fbeta"))
  accuracy <- predictions$score(msr("classif.acc"))
  auc <- predictions$score(msr("classif.auc"))
  recall <- predictions$score(msr("classif.recall"))
  sensitivity <- predictions$score(msr("classif.sensitivity"))
  specificity <- predictions$score(msr("classif.specificity"))
  # false_positive_rate <- predictions$score(msr("classif.fpr"))
  # false_negative_rate <- predictions$score(msr("classif.fnr"))
  # true_positive_rate <- predictions$score(msr("classif.tpr"))
  # true_negative_rate <- predictions$score(msr("classif.tnr"))
  classification_error <- predictions$score(msr("classif.ce"))
  precision <- predictions$score(msr("classif.precision"))
  # confusion_mat <- predictions$confusion
  
  # Return a data frame with all metrics
  return(data.frame(
    F1_score = f1_score,
    Accuracy = accuracy,
    AUC = auc,
    Recall = recall,
    Sensitivity = sensitivity,
    Specificity = specificity,
    # False_Positive_Rate = false_positive_rate,
    # False_Negative_Rate = false_negative_rate,
    # True_Positive_Rate = true_positive_rate,
    # True_Negative_Rate = true_negative_rate,
    Classif_error = classification_error,
    Precision = precision
    # Confusion = confusion_mat
  ))
}

# Create temperature features from ear tag observations (10 min) for inference
#' Create a set of temperature features for a data frame.
#' @description This function creates a variety of features related to the 
#' temperature from the input dataframe, including differences between 
#' consecutive observations, ranges, and more.
#' @param df A input `data.table` Must contain columns 'timestamp' and 'observed'.
#' @return A new `data.table` with the created features

create_features_inference <- function(data) {
  # Ensure data is ordered by animal_id and timestamp
  setorder(data, animal_id, timestamp_awst)
  
  # Create rolling statistics
  data[, Rolling_Avg_24h := rollmean(observed, k = 144, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_30h := rollmean(observed, k = 180, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_Avg_48h := rollmean(observed, k = 288, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_SD_24h := rollapply(observed, width = 144, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_30h := rollapply(observed, width = 180, FUN = sd, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_SD_48h := rollapply(observed, width = 288, FUN = sd, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_max_12h := rollapply(observed, width = 72, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_24h := rollapply(observed, width = 144, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_30h := rollapply(observed, width = 180, FUN = max, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_max_48h := rollapply(observed, width = 288, FUN = max, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_min_24h := rollapply(observed, width = 144, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_30h := rollapply(observed, width = 180, FUN = min, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_min_48h := rollapply(observed, width = 288, FUN = min, fill = NA, align = "right"), by = animal_id]
  
  data[, Rolling_range_24h := rollapply(observed, width = 144, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_30h := rollapply(observed, width = 180, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  data[, Rolling_range_48h := rollapply(observed, width = 288, FUN = function(x) {max(x) - min(x)}, fill = NA, align = "right"), by = animal_id]
  
  return(data)
}


#' Create a Status Summary Plot
#' @description This function reads a CSV file containing status data, 
#' summarises it, and creates a bar plot displaying the counts of different 
#' statuses over time. Optionally,
#' it can save the plot as an image.
#' @param csv_file_path Character. The file path of the CSV containing the 
#' status data.
#' @param export_plot Logical. If `TRUE`, the generated plot is saved as a PNG 
#' file in the "images" directory.
#' @return A `ggplot` object representing the status summary plot.
#' @export

create_status_summary_plot <- function(csv_file_path, 
                                       location_id = NULL,
                                       export_plot = FALSE,
                                       save_width = NULL,
                                       save_height = 5) {
  # Read the CSV file
  dt_out <- data.table::fread(csv_file_path)
  
  # add save time to file name
  timestamp_export <- format(Sys.time(), "%d-%b-%y-%X")
  
  # Summarize the data
  status_summary_dt <- dt_out[, .N, by = .(date, final_status)][order(-N)]
  data.table::setnames(status_summary_dt, c("date", "final_status", "Count"))
  
  # Calculate total counts per day
  date_summary <- status_summary_dt[final_status %notin% c("ok"),
                                    .(total_count = sum(Count)),
                                    by = c("date")]
  
  if (is.null(location_id)) {
    location_id <- ""
  } 
  
  # plot_df <- 
    # subset(status_summary_dt, 
           # final_status %notin% c("ok") & 
             # date %notin% max(status_summary_dt$date, na.rm = T))
  
  # Create the plot
  plot <- ggplot(status_summary_dt,
                 aes(x = reorder(final_status, -Count), y = Count, group = date)) +
    geom_bar(stat = "identity", aes(fill = final_status), show.legend = FALSE) +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.75),
              color = "white", size = 4) +
    
    geom_text(data = date_summary,
              aes(label = paste("Total:", total_count), x = 1.5, y = Inf),
              hjust = 0.5, vjust = 1.1, size = 4, color = "black", inherit.aes = FALSE) +
    
    scale_y_continuous(limits = c(0, max(date_summary$total_count) * 1.1),
                       breaks = function(limits) seq(0, limits[2], by = ifelse(limits[2] > 20, 20, 10))) +
    
    
    labs(title = "ML approach output",
         subtitle = paste("3PS weaner trial 3 (", location_id, ")"), 
         x = "\nLabel",
         y = "Count\n") +
    theme_minimal(base_size = 14) +
    facet_wrap(. ~ date, ncol = length(date_summary$date)) +
    scale_fill_manual(name = "Final label", values = c("alert" = "goldenrod", "sick" = "firebrick"))
  
  # Export the plot if requested
  if (export_plot) {
    # Ensure directory exists
    output_dir <- "images"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    file_path <- file.path(output_dir, 
                           paste0("status_summary_plot_",
                                  timestamp_export,
                                  ".png")
    )
    
    ggsave(filename = file_path, plot = plot, width = save_width, height = save_height, dpi = 300, bg = "white")
    message(paste("Plot saved to:", file_path))
  }
  
  # Return the plot
  return(plot)
}
