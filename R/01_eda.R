# Load libs
library(arrow)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(mgcv)

# Modelling
library(tidymodels)
library(forcats)
library(vip)
library(xgboost)
library(ranger)
# library(keras)
# library(reticulate)
# reticulate::use_condaenv(condaenv = "base")

# source functions
source("R/utils.R")

# Load raw data ----
# Get all file names
all_files <- list.files("data/NR"); all_files

# Grab only ones with average temperature
temperature_files <- grep("_avgTemp10mins_", all_files, value = T)

# Behaviour index
behaviour_index_file <- grep("_hourlyActivityIndex_", all_files, value = TRUE)

# Mortality files
sick_files <- grep("_Sickpen_Mortality.", all_files, value = TRUE)

# Create dataset combining all metadata information ----
# Match sick with pen data
sick_complete_df <- 
  sick_df |>
  left_join(pen_gender_df, by = "tag_id")

sick_complete_df$sex <- NULL
names(sick_complete_df)[4] <- "sex"
names(sick_complete_df)[5] <- "pen_number_start"

sick_complete_df$pen_number <- 19
sick_complete_df$pen_number <- factor(sick_complete_df$pen_number)
str(sick_complete_df)

# Each tag should have X obs (10 mins over time period)
obs_counts_temp <- 
  tag_temp_df |> 
  dplyr::mutate(tag_id = factor(tag_id)) |>
  group_by(tag_id) |>
  count() |>
  arrange(-n) |>
  ungroup()

max(obs_counts_temp$n) # 3994
hist(obs_counts_temp$n)

# Match temp counts with pen metadata
tag_temp_metadata_df <- 
  obs_counts_temp |>
  left_join(pen_gender_df, by = "tag_id")

names(tag_temp_metadata_df)[2] <- "n_obs_temp"
which(duplicated(tag_temp_metadata_df)) # 810 (001D)

# Remove duplicated row?
tag_temp_metadata_df <- tag_temp_metadata_df[-c(810), ]

# Counts for behaviour index ----
obs_counts_idx <- 
  behaviour_idx_df |> 
  dplyr::mutate(tag_id = factor(tag_id)) |>
  group_by(tag_id) |>
  count() |>
  arrange(-n) |>
  ungroup()

max(obs_counts_idx$n) # 613
hist(obs_counts_idx$n)

# Match temp counts with pen metadata
tag_idx_metadata_df <- 
  obs_counts_idx |>
  left_join(pen_gender_df, by = "tag_id")

names(tag_idx_metadata_df)[2] <- "n_obs_idx"

# Remove a duplicated row?
which(duplicated(tag_idx_metadata_df)) # 7 (001D)
tag_idx_metadata_df <- tag_idx_metadata_df[-c(7), ]

# Join all metadata together
meta_df <- 
  tag_temp_metadata_df |>
  left_join(tag_idx_metadata_df, by = "tag_id") 

# Remove COL.y 
meta_df$gender.y <- NULL
meta_df$pen_number.y <- NULL

# Edit names
names(meta_df) <- c("tag_id", "n_obs_temp", "sex", "pen_number", "n_obs_idx")

str(meta_df)
summary(meta_df)

# merge metadata with sick df
batch2_metadata_df <- merge(meta_df, 
                            sick_complete_df, 
                            by = "tag_id", 
                            all = T)

# Create new cols based on the pen_number and the mortality
batch2_metadata_df$sick_pen <- fifelse(is.na(batch2_metadata_df$pen_number.y), 0, 1)
batch2_metadata_df$dead <- fifelse(is.na(batch2_metadata_df$mortality_date), 0, 1)

# Remove duplicated columns on join
batch2_metadata_df$mortality_date <- NULL
batch2_metadata_df$sex.y <- NULL
batch2_metadata_df$pen_number_start <- NULL
batch2_metadata_df$pen_number.y <- NULL

# Renam
names(batch2_metadata_df)[3:4] <- c("x", "pen_number")

# Check data
str(batch2_metadata_df)
summary(batch2_metadata_df)

# export data
# arrow::write_parquet(batch2_metadata_df, "data/processed/batch2_metadata.parquet")


# Load datasets ----
tag_temp_df <- arrow::read_parquet("data/processed/batch2_tag_temp.parquet")
behaviour_idx_df <- arrow::read_parquet("data/processed/batch2_behaviour_index.parquet")
sick_df <- arrow::read_parquet("data/processed/batch2_sick_record.parquet")
pen_gender_df <- arrow::read_parquet("data/processed/batch2_pen_gender_ids.parquet")
batch2_metadata_df <- arrow::read_parquet("data/processed/batch2_metadata.parquet")

# Create model df ----
# Temperature data
str(tag_temp_df)

# ID to factor, extract julian day and arrange/order by the timestamp
tag_temp_df <- 
  tag_temp_df |>
  mutate(doy = lubridate::yday(date_time),
         tag_id = factor(tag_id)
         # hypo_crit = factor(hypo_crit),
         # hypo_severe = factor(hypo_severe),
         # fever_crit = factor(fever_crit),
         # fever_severe = factor(fever_severe)
  ) |>
  arrange(date_time)

# Merge temperature data with batch2 metadata df
model_df <-
  tag_temp_df |>
  left_join(batch2_metadata_df[, c(1,3,4,8)], by = "tag_id")

# Dead to factor
model_df$dead <- factor(model_df$dead)

# Check daada
str(model_df)
summary(model_df)

# Check tag 0002 (almost complete) 
day_night_labels <- c("0" = "Night", "1" = "Day")
mortality_labels <- c("0" = "dead", "1" = "alive")

ggplot(subset(model_df, tag_id == "0002") , aes(x = date_time, y = tag_temp), group = tag_id) +
  geom_line(linewidth = 1.2, alpha = .8) +
  # scale_color_manual(values = c(rep("grey", 6), "firebrick", "orange", rep("firebrick", 2))) +
  scale_x_datetime(breaks = scales::date_breaks("5 day"),
                   labels = scales::date_format("%A %H:%m")) +
  scale_y_continuous(limits = c(10, 70), breaks = seq(25, 50, 5)) +
  facet_wrap(. ~ day_night, labeller = labeller(day_night = day_night_labels)) +
  theme_light(base_size = 16) +
  theme(axis.text.x = element_text(angle = 330))


ggplot(model_df, aes(x = tag_temp), group = dead) +
  ggridges::geom_density_line() +
  facet_wrap(day_night ~ dead, labeller = labeller(dead = mortality_labels)) +
  theme_light(base_size = 16) 
  

# Select a few tag_ids (between 500 and 2000 obs for temp)
tags_missing_obs <- 
  as.character(
    obs_counts_temp[
      which(obs_counts_temp$n < 2000 & obs_counts_temp$n > 500), 
    ]$tag_id
  )

length(tags_missing_obs) # 39 tags

# Check temperature data
ggplot(subset(model_df, tag_id %in% tags_missing_obs), 
       aes(x = tag_temp, y = tag_id), group = tag_id) +
  ggridges::geom_density_ridges() +
  scale_x_continuous(name = "\nTag temperature (ºC)", 
                     limits = c(20, 50), 
                     breaks = seq(25, 50, 5)) +
  scale_y_discrete(name = "XioT tag ID\n") +
  theme_minimal(base_size = 16)

ggsave("images/missing_tags_density.png",
       width = 4, 
       height = 8, 
       units = "in", 
       bg = "white",
       dpi = 360)

# Split day night
ggplot(subset(model_df, tag_id %in% tags_missing_obs), 
       aes(x = tag_temp, y = tag_id), group = dead) +
  ggridges::geom_density_ridges() +
  scale_x_continuous(name = "\nTag temperature (ºC)", 
                     limits = c(20, 50), 
                     breaks = seq(25, 50, 5)) +
  scale_y_discrete(name = "XioT tag ID\n") +
  facet_wrap(. ~ day_night, labeller = labeller(day_night = day_night_labels)) +
  theme_minimal(base_size = 16)

ggsave("images/missing_tags_density_daynight.png",
       width = 6, 
       height = 9, 
       units = "in", 
       bg = "white",
       dpi = 360)

# split compromised pigs
ggplot(subset(model_df, tag_id %in% tags_missing_obs), 
       aes(x = tag_temp, y = tag_id), group = dead) +
  ggridges::geom_density_ridges() +
  scale_x_continuous(name = "\nTag temperature (ºC)", 
                     limits = c(20, 50), 
                     breaks = seq(25, 50, 5)) +
  scale_y_discrete(name = "XioT tag ID\n") +
  facet_wrap(. ~ dead, labeller = labeller(dead = mortality_labels)) +
  theme_minimal(base_size = 16)

ggsave("images/missing_tags_density_compromised.png",
       width = 6, 
       height = 9, 
       units = "in", 
       bg = "white",
       dpi = 360)


# Some tag temperatures seem to high or too low
subset(model_df, tag_temp > 48)
subset(model_df, tag_temp < 25)

# 268 observations where tag temperature is greater than 48C - Remove 
# observations. Now on the lower end what is a realistic threshold? Below 30C 
# seems extreme already.. but still about 100k observations below 30C. Below
# 25C (should pigs be confirmed dead if body temp at this temp?) there are about
# 60k observations.

# Join model_df to the behaviour index data ----
str(behaviour_idx_df)

# Tag id to factor
behaviour_idx_df$tag_id <- factor(behaviour_idx_df$tag_id)

# Join datasets
model_df <-
  model_df |>
  left_join(behaviour_idx_df[, c(1:3)], by = c("tag_id", "date_time"))

# Export
# arrow::write_parquet(model_df, "data/processed/batch2_model_df_NAs.parquet")

# Check start and end datetime
max(unique(model_df$date_time))
min(unique(model_df$date_time))

# Filter data only with non missing data activity data
model_df_behaviour_complete <- model_df[which(!is.na(model_df$activity)), ]

# arrow::write_parquet(model_df_behaviour_complete, 
                     # "data/processed/batch2_model_df_behave_complete.parquet")

max(unique(model_df_behaviour_complete$date_time))
min(unique(model_df_behaviour_complete$date_time))

# Filter data for NA in pen_number and sex (same rows)
model_df_tiny <- model_df_behaviour_complete[which(!is.na(model_df_behaviour_complete$sex)), ]

# Export
# arrow::write_parquet(model_df_tiny, 
                     # "data/processed/batch2_model_df_noNAs.parquet")


# Load data
model_df_tiny <- arrow::read_parquet("data/processed/batch2_model_df_noNAs.parquet")
str(model_df_tiny)

# set up model formula for GAM model
model_df_tiny$datetime_num <- as.numeric(model_df_tiny$date_time)
model_df_tiny$day <- as.factor(format(model_df_tiny$date_time, "%A"))

# Add lagged features for tag temperature and activity index
model_df_tiny <-
  model_df_tiny |>
  mutate(
    temp_lag1 = lag(tag_temp, 6),
    temp_lag2 = lag(tag_temp, 12),
    activity_lag1 = lag(activity, 6),
    activity_lag2 = lag(activity, 12)
  ) |>
  na.omit()

# Split data into train/test
splits <- rsample::initial_split(model_df_tiny, 
                                 strata = dead, 
                                 prop = .8)
train_df <- rsample::training(splits)
test_df <- rsample::testing(splits)

# Run gam model ----
# Define gam formulae
my_formula <- (dead ~ s(tag_temp, k = 5) +
                 s(activity, k = 5) +
                 s(temp_lag1) + 
                 s(temp_lag2) +
                 s(activity_lag1) +
                 s(activity_lag2) +
                 day_night +
                 sex + 
                 s(hour, bs = "cc") +
                 s(pen_number, day, bs = "re")
)

fit <- bam(my_formula, 
           family = binomial("logit"), 
           data = train_df)
gc()

# Export model
# saveRDS(fit, "outputs/gam_model.rds")

# Check
gam.check(fit)
plot(fit)

# Get residuals
residuals_gam <- resid(fit)

# Histogram
hist(residuals_gam, 
     main = "Histogram of Residuals",
     xlab = "Residuals")

# Shapiro-Wilk (sensitive to larger sample sizes)
shapiro.test(residuals_gam)

# Kolmogorov-Smirnov test
ks.test(residuals_gam, "pnorm", mean(residuals_gam), sd(residuals_gam))

# Q-Q Plot
qqnorm(residuals_gam)
qqline(residuals_gam, col = "firebrick")

# Density plot with overlay of normal distribution
density_gam_plot <- density(residuals_gam)
plot(density_gam_plot, main = "Density Plot of Residuals")
curve(
  dnorm(x, 
        mean = mean(residuals_gam), 
        sd = sd(residuals_gam)
  ),
  add = TRUE, 
  col = "firebrick"
)

# Predict
gam_preds <- predict(fit, test_df, "response")
out_gam <- data.table::setDT(
  data.frame(cbind(as.character(test_df$date_time), as.character(test_df$tag_id), test_df$dead, gam_preds)))
names(out_gam) <- c("dates", "tag_id", "true", "pred")


# Run mixed-effects model ----
library(lme4)
# control_params <- lmeControl(opt = "optim", maxIter = 1000)
control_params <- glmerControl(
  optimizer = "bobyqa",  # Optimization algorithm
  optCtrl = list(maxfun = 1e3)  # Maximum number of function evaluations
)

fit_mixed <- glmer(
  dead ~ tag_temp + activity + temp_lag1 + temp_lag2 + activity_lag1 + 
    activity_lag2 + day_night + sex + (1 | tag_id/pen_number),
  data = train_df,
  family = binomial(link = "logit"),
  control = control_params
)

# Export model
# saveRDS(fit_mixed, "output/glmer_model.rds")

fit_mixed
summary(fit_mixed)
plot(fit_mixed)
autoplot(fit_mixed)


# Run brms ----
# options(mc.cores = parallel::detectCores())
# options(brms.backend = "cmdstanr")
# 
# library(brms)
# library(cmdstanr)
# 
# ctrl <- list(adapt_delta = 0.9)
# 
# bayes_fit <- 
#   brms::brm(my_formula, 
#             data = train_df,
#             seed = 108,
#             control = ctrl,
#             family = brms::bernoulli(link = "logit"),
#             iter = 8000,
#             warmup = 2000,
#             cores = parallel::detectCores())

# Run xgboost model ----
# library(parsnip)
# library(dials)
# library(workflows)
# library(tune)
# library(yardstick)

model_formula <- (dead ~ 
                    tag_temp +
                    activity+
                    temp_lag1 + 
                    temp_lag2 +
                    activity_lag1 +
                    activity_lag2 +
                    day_night +
                    sex)

# Set cross-validation
set.seed(108)
df_folds <- rsample::vfold_cv(train_df, v = 5)
df_folds


# xgb_grid <- grid_latin_hypercube(
#   trees(),
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), train_df),
#   learn_rate(),
#   size = 50
# )

# Set xgboost specs
# xgb_spec <- boost_tree(
#   trees = tune(), 
#   tree_depth = tune(), 
#   min_n = tune(), 
#   loss_reduction = tune(),                     
#   sample_size = tune(), 
#   mtry = tune(),         
#   learn_rate = tune(),                       
# ) |> 

# Find best hyperparameters
# doParallel::registerDoParallel()
# set.seed(108)
# system.time(
#   xgb_res <- tune_grid(
#     xgb_workflow,
#     resamples = df_folds,
#     grid = xgb_grid,
#     metrics = metric_set(accuracy, roc_auc)
#   )
# )

log_spec <- # your model specification
  logistic_reg() |> # model type
  set_engine(engine = "glm") |> # model engine
  set_mode("classification") # model mode

xgb_spec <- 
  boost_tree() |> 
  set_engine("xgboost", 
             objective = "binary:logistic",
             nthread = parallel::detectCores(),
             eval_metric = "error") |> 
  set_mode("classification") 

rf_spec <- 
  rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

nnet_spec <-
  mlp() %>%
  set_mode("classification") |>
  set_engine("keras", verbose=0) 

# Set workflow 
log_workflow <- workflow() |>
  add_formula(model_formula) |>
  add_model(log_spec)

xgb_workflow <- workflow() |>
  add_formula(model_formula) |>
  add_model(xgb_spec)

ranger_workflow <- workflow() |>
  add_formula(model_formula) |>
  add_model(rf_spec)

keras_workflow <- workflow() |>
  add_formula(model_formula) |>
  add_model(nnet_spec)

# Fit resamples
log_res <- 
  log_workflow |>
  fit_resamples(
    resamples = df_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 

xgb_res <- 
  xgb_workflow |>
  fit_resamples(
    resamples = df_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 


rf_res <-
  ranger_workflow |>
  fit_resamples(
    resamples = df_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 


# nnet_res <- 
#   keras_workflow |>
#   fit_resamples(
#     resamples = df_folds, 
#     metrics = metric_set(
#       recall, precision, f_meas, 
#       accuracy, kap,
#       roc_auc, sens, spec),
#     control = control_resamples(save_pred = FALSE)
#   ) 

# Compare models
log_metrics <- 
  log_res |>
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Logistic Regression") # add the name of the model to every row

log_pred <- 
  log_res %>%
  collect_predictions()

log_pred |>
  conf_mat(dead, .pred_class) 

log_pred |>
  ggplot() +
  geom_density(aes(x = .pred_1, 
                   fill = dead), 
               alpha = 0.5)

# xgb
xgb_metrics <- 
  xgb_res |>
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

xgb_pred <- 
  xgb_res %>%
  collect_predictions()


xgb_pred |>
  conf_mat(dead, .pred_class) |>
  autoplot(type = "heatmap")

xgb_pred |>
  ggplot() +
  geom_density(aes(x = .pred_1, 
                   fill = dead), 
               alpha = 0.5)

# RF
rf_metrics <- 
  rf_res |>
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "RandomForest")

rf_pred <- 
  rf_res %>%
  collect_predictions()


rf_pred |>
  conf_mat(dead, .pred_class) |>
  autoplot(type = "heatmap")

rf_pred |>
  ggplot() +
  geom_density(aes(x = .pred_0, 
                   fill = dead), 
               alpha = 0.5)

# nnet_metrics <- 
#   nnet_res |>
#   collect_metrics(summarise = TRUE) %>%
#   mutate(model = "Neural Net")
# 
# nnet_pred <- 
#   nnet_res %>%
#   collect_predictions()

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics
  # knn_metrics,
  # nnet_metrics
) 

# change data structure
model_comp <- 
  model_compare |>
  select(model, .metric, mean, std_err) |>
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean F1-Score for every model
model_comp |>
  arrange(mean_f_meas) |>
  mutate(model = fct_reorder(model, mean_f_meas)) |># order results
  ggplot(aes(model, mean_f_meas, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_f_meas, 2), y = mean_f_meas + 0.08),
    vjust = 1
  )

# show mean area under the curve (auc) per model
# model_comp |>
#   arrange(mean_roc_auc) |>
#   mutate(model = fct_reorder(model, mean_roc_auc)) %>%
#   ggplot(aes(model, mean_roc_auc, fill=model)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_brewer(palette = "Blues") + 
#   geom_text(
#     size = 3,
#     aes(label = round(mean_roc_auc, 2), y = mean_roc_auc + 0.08),
#     vjust = 1
#   )


# Evaluation on test set
last_fit_log <- last_fit(log_workflow, 
                        split = splits,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                          roc_auc, sens, spec)
)

last_fit_log |>
  pluck(".workflow", 1) |>  
  pull_workflow_fit() |>
  vip(num_features = 10)

last_fit_xgb <- last_fit(xgb_workflow, 
                         split = splits,
                         metrics = metric_set(
                           recall, precision, f_meas, 
                           accuracy, kap,
                           roc_auc, sens, spec)
)

last_fit_xgb |>
  pluck(".workflow", 1) |>  
  pull_workflow_fit() |>
  vip(num_features = 10)


last_fit_rf <- last_fit(ranger_workflow, 
                        split = splits,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                           roc_auc, sens, spec)
)

last_fit_rf |>
  pluck(".workflow", 1) |>  
  pull_workflow_fit() |>
  vip(num_features = 10)


# last_fit_nnet <- last_fit(keras_workflow, 
#                          split = splits,
#                          metrics = metric_set(
#                            recall, precision, f_meas, 
#                            accuracy, kap,
#                            roc_auc, sens, spec)
# )
# 
# library(baguette)
# 
# last_fit_nnet |>
#   pluck(".workflow", 1) |>  
#   pull_workflow_fit() |>
#   vip(num_features = 10)
# 
# last_fit_nnet |>
#   extract_fit_engine() |>
#   nnet_imp_garson()



# Get results from CV validations
results <- collect_metrics(xgb_res, summarize = T)
results |> 
  arrange(mean) |> 
  top_n(25)

# Which one is the best
show_best(xgb_res, "accuracy")
show_best(xgb_res, "roc_auc")

# Select the best combination of hyper-params and set final model fit
best_mae <- select_best(xgb_res, "mae")

final_xgb <- finalize_workflow(
  xgb_workflow,
  best_mae
)

final_xgb

# Check variable importance
library(vip)
final_xgb |>
  fit(data = train_df) |>
  pull_workflow_fit() |>
  vip(geom = "col") + theme_light(base_size = 16)

# Final fit
xgb_fit <-
  final_xgb |>
  fit(data = train_df) 

# Check model performance
final_res <- last_fit(final_xgb, splits)
xgb_metrics <- collect_metrics(final_res)


# Get analysis interval (ie min and max timestamp)
# Using temperature data for now
# tag_temp_interval <- lubridate::interval(min(model_df$date_time), max(model_df$date_time))

