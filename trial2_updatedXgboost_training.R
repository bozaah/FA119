# Load libraries
library(data.table)
library(ggplot2)
# library(ggtext)
library(patchwork)
library(zoo)
library(here)
library(anomalize)
library(future)

# ML libs
library(mlr3)
library(mlr3verse)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3viz)
library(paradox)


# Source functions
source(here::here("R/utils.R"))

# define measures to score/evalutate
eval_metrics <- c(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.recall"),
  msr("classif.sensitivity"),
  msr("classif.specificity"),
  msr("classif.fpr"),
  msr("classif.fnr"),
  msr("classif.tpr"),
  msr("classif.tnr"),
  msr("classif.ce"),
  msr("classif.precision")
)

# Define columns used for training
train_cols <- 
  c("week_age", "trend", "Rolling_Avg_24h", "Rolling_Avg_30h", "Rolling_Avg_48h", "Rolling_SD_24h",
    "Rolling_SD_30h", "Rolling_SD_48h", "Rolling_max_12h", "Rolling_max_24h", "Rolling_max_30h", 
    "Rolling_max_48h", "Rolling_min_24h", "Rolling_min_30h", "Rolling_min_48h", "Rolling_range_24h",
    "Rolling_range_30h", "Rolling_range_48h")

# Load data
options(arrow.unsafe_metadata = TRUE)
model_dt_full <- arrow::read_parquet("data/3PS_trial_2_model_df_FULL_jan24.parquet") |> setDT() 
setorder(model_dt_full, timestamp, animal_id)
model_dt_full[, week_age := lubridate::week(date)]

# filter dt only to train cols
model_dt <- model_dt_full[, ..train_cols][, target := model_dt_full$target]
model_dt_clean <- na.omit(model_dt)

fa119_task <- 
  TaskClassif$new(id = "fa119", backend = model_dt_clean, target = "target")

split <- partition(fa119_task, ratio = 0.8)

# Create separate tasks for train and test
train_task <- fa119_task$clone()$filter(split$train)
test_task <- fa119_task$clone()$filter(split$test)

task_split <- tsk(id = "fa119") 

######## RANGER
#### Fine tune ranger model
future::plan("multisession")

# Set tunning space
lts_ranger <- lts("classif.ranger.default")

# Set tuning params
tnr_random <- tnr("random_search")
rsmp_holdout <- rsmp("holdout", ratio = 0.8)
trm_evals <- trm("evals", n_evals = 20)
msr_fpr <- msr("classif.fpr")

# Set model params to tune
lrn_ranger <- lrn("classif.ranger", predict_type = "prob", importance = "impurity")
lrn_ranger$param_set$set_values(.values = lts_ranger$values)

# set threads for parallel compute
set_threads(lrn_ranger, parallel::detectCores())

# Tuning without nested resampling
instance <- ti(
  task = train_task,
  learner = lrn_ranger,
  resampling = rsmp_holdout,
  measures = msr_auc,
  terminator = trm_evals
)

# Optimise
tuner_ranger <- tnr("random_search", batch_size = 10)
tuner_ranger$optimize(instance)

# autoplot(instance, type = "performance")
# autoplot(instance, type = "parallel")
# autoplot(instance, type = "incumbent")
# autoplot(instance, type = "pairs")

# Extract metric
insample <- instance$result_y
instance$result_learner_param_vals

# INFO  [21:14:54.833] [bbotk] Result:
# INFO  [21:14:54.834] [bbotk]  mtry.ratio num.trees replace sample.fraction learner_param_vals  x_domain classif.auc
# INFO  [21:14:54.834] [bbotk]       <num>     <int>  <lgcl>           <num>             <list>    <list>       <num>
# INFO  [21:14:54.834] [bbotk]   0.4170116      1524   FALSE       0.3976391          <list[6]> <list[4]>   0.9999999

# Tuning with nested resampling
at <- auto_tuner(
  tuner = tnr_random,
  learner = lrn_ranger,
  resampling = rsmp_holdout,
  measure = msr_auc,
  terminator = trm_evals
)

# nested sample
rsmp_cv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)
out_of_sample_ranger <- resample(train_task, at, rsmp_cv5)
outsample_ranger <- out_of_sample_ranger$score(measure = eval_metrics)

# INFO  [08:16:54.847] [bbotk] Result:
# INFO  [08:16:54.848] [bbotk]  mtry.ratio num.trees replace sample.fraction learner_param_vals  x_domain classif.auc
# INFO  [08:16:54.848] [bbotk]       <num>     <int>  <lgcl>           <num>             <list>    <list>       <num>
# INFO  [08:16:54.848] [bbotk]    0.694769      1586    TRUE       0.6476078          <list[6]> <list[4]>           1


###
best_params_ranger <- list(
  num.threads = 6,
  verbose = TRUE,
  mtry.ratio = 0.694769,
  replace = TRUE,
  sample.fraction = 0.6476078,
  num.trees = 1586
)

# Tuned model
lrn_ranger_tuned <- lrn("classif.ranger", 
                        predict_type = "prob", 
                        importance = "impurity",
                        oob.error = TRUE)

lrn_ranger_tuned$param_set$set_values(.values = best_params_ranger)

lrn_ranger_tuned$train(train_task)

generalisation_ranger <- lrn_ranger_tuned$predict(test_task)$score(measure = eval_metrics)

# Close parallel multisession
# future::plan(sequential)

# Extract prediction and check metrics
# lrn_xgboost_tuned$train(train_task)
prediction_train_ranger <- lrn_ranger_tuned$predict(train_task)
autoplot(prediction_train_ranger, type = "stacked") + theme_light(base_size = 16)

prediction_tuned_ranger <- lrn_ranger_tuned$predict(test_task)
lrn_ranger_tuned$importance()
plot_var_importance(lrn_ranger_tuned$importance()) + ggtitle("Variable importance - tuned model")

# oob_error
lrn_ranger_tuned$oob_error()

##### XGBOOST
# select the multisession backend to use
future::plan("multisession")

# Set tunning space
lts_xgb <- lts("classif.xgboost.default")

# Set tuning params
tnr_random <- tnr("random_search")
rsmp_holdout <- rsmp("holdout", ratio = 0.8)
trm_evals <- trm("evals", n_evals = 20)
# msr_f1 <- msr("classif.fbeta")
msr_auc <- msr("classif.auc")

# Set model params to tune
lrn_xgb <- lrn("classif.xgboost", predict_type = "prob")
lrn_xgb$param_set$set_values(.values = lts_xgb$values)

# set threads for parallel compute
set_threads(lrn_xgb, parallel::detectCores())

# Tuning without nested resampling
instance <- ti(
  task = train_task,
  learner = lrn_xgb,
  resampling = rsmp_holdout,
  measures = msr_auc,
  terminator = trm_evals
)

# Optimise
tuner <- tnr("random_search", batch_size = 10)
tuner$optimize(instance)

# Extract metric
insample <- instance$result_y

# INFO  [18:45:23.403] [bbotk] Result:
# INFO  [18:45:23.405] [bbotk]      alpha colsample_bylevel colsample_bytree       eta    lambda max_depth nrounds subsample learner_param_vals  x_domain classif.auc
# INFO  [18:45:23.405] [bbotk]      <num>             <num>            <num>     <num>     <num>     <int>   <int>     <num>             <list>    <list>       <num>
# INFO  [18:45:23.405] [bbotk]  -5.629396         0.5570495        0.5666036 -8.090602 -3.453862        17     246 0.8226233         <list[10]> <list[8]>           1

# Tuning with nested resampling
at_xgb <- auto_tuner(
  tuner = tnr_random,
  learner = lrn_xgb,
  resampling = rsmp_holdout,
  measure = msr_auc,
  terminator = trm_evals
)

# nested sample
rsmp_cv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)
out_of_sample <- resample(train_task, at, rsmp_cv5)
outsample <- out_of_sample$score(measure = eval_metrics)

# INFO  [19:06:33.563] [bbotk] Finished optimizing after 20 evaluation(s)
# INFO  [19:06:33.563] [bbotk] Result:
# INFO  [19:06:33.565] [bbotk]      alpha colsample_bylevel colsample_bytree       eta    lambda max_depth nrounds subsample learner_param_vals  x_domain classif.auc
# INFO  [19:06:33.565] [bbotk]      <num>             <num>            <num>     <num>     <num>     <int>   <int>     <num>             <list>    <list>       <num>
# INFO  [19:06:33.565] [bbotk]  -6.180859         0.5111613        0.6133854 -4.070764 -3.125043         8    3436  0.608528         <list[10]> <list[8]>           1


# best xgboost params
best_params <- list(
  nthread = 8,
  verbose = 1,
  nrounds = 420,
  eta = 0.070764,
  max_depth = 8,
  colsample_bytree = 0.6133854,
  colsample_bylevel = 0.5111613,
  lambda = 0.125043,
  alpha = 0.180859,
  subsample =  0.608528
)
# best_params <- list(
#   nthread = 8,
#   verbose = 1,
#   # early_stopping_set = "none",
#   nrounds = 530,
#   eta = 0.3882343,
#   max_depth = 9,
#   colsample_bytree = 0.5261703,
#   colsample_bylevel = 0.8223902,
#   lambda = 4.977289,
#   # alpha = -2.040999,
#   alpha = 0.6524879,
#   subsample =  0.93331
# )

##################################################################
# Tuned model
lrn_xgboost_tuned <- lrn("classif.xgboost", predict_type = "prob")
lrn_xgboost_tuned$param_set$set_values(.values = best_params)

lrn_xgboost_tuned$train(train_task)

generalisation <- lrn_xgboost_tuned$predict(test_task)$score(measure = eval_metrics)

# Extract prediction and check metrics
prediction_train <- lrn_xgboost_tuned$predict(train_task)
autoplot(prediction_train, type = "stacked") + theme_light(base_size = 16)

prediction_tuned <- lrn_xgboost_tuned$predict(test_task)
lrn_xgboost_tuned$importance()
plot_var_importance(lrn_xgboost_tuned$importance()) + ggtitle("Variable importance - tuned model")

prediction_tuned$confusion
autoplot(prediction_tuned, type = "stacked") + theme_light(base_size = 16)

# Check thresholds
autoplot(prediction_tuned, type = "threshold", measure = msr("classif.fbeta")) +
  autoplot(prediction_tuned, type = "threshold", measure = msr("classif.acc")) + 
  autoplot(prediction_tuned, type = "threshold", measure = msr("classif.fpr")) + 
  ggtitle("Threshold check - tuned xgboost")


## ensemble models
# Define the learners
# Create a pipeline that combines the predictions of the two models
future::plan("multisession")

ensemble_pipeline <- 
  po("learner_cv", lrn_xgboost_tuned) %>>%
  po("learner_cv", lrn_ranger_tuned) %>>%
  po("featureunion") %>>%
  po("learner", lrn("classif.rpart", predict_type = "prob"))

# Create a Graph from the pipeline
ensemble_graph <- as_graph(ensemble_pipeline)

# Train the ensemble model with resampling
# future::plan("multisession")

rsmp_cv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)

resampling_result <- resample(
  task = train_task,
  learner = ensemble_graph,
  resampling = rsmp_cv5, 
  store_models = TRUE 
)

future::plan("sequential")

# Evaluate the resampling results
resampling_result$score(eval_metrics) 

# Access individual predictions and models
predictions <- resampling_result$predictions()
models <- resampling_result$learners

# Train the ensemble model
ensemble_graph$train(train_task)

# Evaluate the predictions
predictions_ensemble <- ensemble_graph$predict(test_task)$classif.rpart.output
performance_ensemble <- predictions_ensemble$score(measure = eval_metrics)
autoplot(predictions_ensemble, type = "stacked") + theme_light(base_size = 16)

saveRDS(ensemble_graph, file = "outputs/ensemble_trained_on_trial2_v3.rds")
