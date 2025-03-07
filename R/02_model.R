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

# Load datasets ----
tag_temp_df <- arrow::read_parquet("data/processed/batch2_tag_temp.parquet")
behaviour_idx_df <- arrow::read_parquet("data/processed/batch2_behaviour_index.parquet")
sick_df <- arrow::read_parquet("data/processed/batch2_sick_record.parquet")
pen_gender_df <- arrow::read_parquet("data/processed/batch2_pen_gender_ids.parquet")
batch2_metadata_df <- arrow::read_parquet("data/processed/batch2_metadata.parquet")

# Load model data without missing values
model_df_tiny <- arrow::read_parquet("data/processed/batch2_model_df_noNAs.parquet")
str(model_df_tiny)

# set up model formula for GAM model
model_df_tiny$datetime_num <- as.numeric(model_df_tiny$date_time)
model_df_tiny$day <- as.factor(format(model_df_tiny$date_time, "%A"))

# Calculate the 3-hour rolling average for avg_temperature and behavior_index
model_df_tiny[, avg_temp_roll := frollmean(tag_temp, 3), by = tag_id]
model_df_tiny[, behavior_index_roll := frollmean(activity, 3), by = tag_id]

# Calculate the 3-hour rolling variance for avg_temperature and behavior_index
model_df_tiny[, avg_temp_roll_var := frollapply(avg_temp_roll, 3, stats::var), by = tag_id]
model_df_tiny[, behavior_index_roll_var := frollapply(behavior_index_roll, 3, stats::var), by = tag_id]

# Add lagged features for tag temperature and activity index
model_df <-
  model_df_tiny |>
  mutate(
    date = lubridate::date(date_time),
    day_night = factor(day_night),
    temp_lag1 = lag(avg_temp_roll, 2),
    temp_lag2 = lag(avg_temp_roll, 4),
    temp_lag3 = lag(avg_temp_roll, 6),
    temp_lag4 = lag(avg_temp_roll, 8),
    temp_lag5 = lag(avg_temp_roll, 10),
    temp_lag6 = lag(avg_temp_roll, 12),
    temp_lag7 = lag(avg_temp_roll, 14),
    temp_lag8 = lag(avg_temp_roll, 16),
    activity_lag1 = lag(behavior_index_roll, 2),
    activity_lag2 = lag(behavior_index_roll, 4),
    activity_lag3 = lag(behavior_index_roll, 6),
    activity_lag4 = lag(behavior_index_roll, 8),
    activity_lag5 = lag(behavior_index_roll, 10),
    activity_lag6 = lag(behavior_index_roll, 12),
    activity_lag7 = lag(behavior_index_roll, 14),
    activity_lag8 = lag(behavior_index_roll, 16)
  ) |>
  select(-c(hypo_crit, hypo_severe, fever_crit, fever_severe, optimum_temp,
            tag_temp, activity)) |>
  na.omit()

# Create a unique day index
unique_dates <- unique(model_df[, .(date)])
unique_dates[, day_index := .I]

# Merge the day index back to the original data.table
model_df <- merge(model_df, unique_dates, by = "date", all.x = TRUE)


# Split data into train/test
splits <- rsample::initial_split(model_df, 
                                 strata = dead, 
                                 prop = .8)
train_df <- rsample::training(splits)
test_df <- rsample::testing(splits)

# Run gam model ----
# Define gam formulae
my_formula <- (dead ~ s(avg_temp_roll, k = 8) +
                 s(behavior_index_roll, k = 8) +
                 s(temp_lag1) + 
                 s(temp_lag2) +
                 s(activity_lag1) +
                 s(activity_lag2) +
                 day_night +
                 sex + 
                 s(hour, bs = "cc") +
                 s(pen_number, day_index, bs = "re")
)

fit <- bam(my_formula, 
           family = binomial("logit"), 
           data = train_df)

# Export model
saveRDS(fit, "outputs/gam_model.rds")
gc()

# Plots
plot(gam_model, pages = 1)

par(mfrow = c(2, 2))
gam.check(gam_model)

# Enhanced Plots with gratia
library(gratia)
library(pdp)

draw(gam_model)
appraise(gam_model)

# Partial dependence plots with pdp
partial(gam_model, pred.var = "x1", plot = TRUE)
partial(gam_model, pred.var = "x2", plot = TRUE)



# Define ML models ----
# Formula
model_formula <- (dead ~ 
                    avg_temp_roll +
                    behavior_index_roll+
                    avg_temp_roll_var +
                    behavior_index_roll_var +
                    temp_lag1 + 
                    temp_lag2 +
                    temp_lag3 + 
                    temp_lag4 +
                    temp_lag5 + 
                    temp_lag6 +
                    temp_lag7 + 
                    temp_lag8 +
                    activity_lag1 +
                    activity_lag2 +
                    activity_lag3 +
                    activity_lag4 +
                    activity_lag5 +
                    activity_lag6 +
                    activity_lag7 +
                    activity_lag8 +
                    day_night +
                    sex +
                    day_index)

fit_glm <- glm(model_formula, data = train_df, family = "binomial")
plot(fit_glm)
summary(fit_glm)

# Mixed effects model
library(lme4)
# control_params <- lmeControl(opt = "optim", maxIter = 1000)
control_params <- glmerControl(
  optimizer = "bobyqa",  # Optimization algorithm
  optCtrl = list(maxfun = 500)  # Maximum number of function evaluations
)

fit_mixed <- glmer(
  model_formula,
  data = train_df,
  family = binomial(link = "logit"),
  control = control_params
)

# Set cross-validation
set.seed(108)
df_folds <- rsample::vfold_cv(train_df, v = 5)
df_folds

doParallel::registerDoParallel()

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

gam_spec <- 
  gen_additive_mod() |>
  set_engine("mgcv") |>
  set_mode("classification")

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

# nnet_spec <-
#   mlp() %>%
#   set_mode("classification") |>
#   set_engine("keras", verbose=0) 

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

# keras_workflow <- workflow() |>
#   add_formula(model_formula) |>
#   add_model(nnet_spec)

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

log_pred |>
  group_by(id) |># id contains our folds
  roc_curve(dead, .pred_0) |>
  autoplot()


# Adjust the threshold
# threshold <- 0.8
# pred_classes <- log_pred %>%
  # mutate(pred_class2 = factor(if_else(.pred_0 > threshold, 0, 1)))

# Combine predictions with true labels
# results <- bind_cols(test_data, pred_classes)

# Evaluate the confusion matrix
# conf_mat(pred_classes, truth = dead, estimate = pred_class2)
# autoplot(conf_mat(pred_classes, truth = dead, estimate = pred_class2), "heatmap")

# xgb
xgb_metrics <- 
  xgb_res |>
  collect_metrics(summarise = TRUE) |>
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

xgb_pred |>
  group_by(id) |># id contains our folds
  roc_curve(dead, .pred_0) |>
  autoplot()


# RF
rf_metrics <- 
  rf_res |>
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "RandomForest")

rf_pred <- 
  rf_res %>%
  collect_predictions()

rf_pred |>
  conf_mat(dead, .pred_class)

rf_pred |>
  conf_mat(dead, .pred_class) |>
  autoplot(type = "heatmap")

rf_pred |>
  ggplot() +
  geom_density(aes(x = .pred_1, 
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

last_fit_xgb |>
  collect_metrics()


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

