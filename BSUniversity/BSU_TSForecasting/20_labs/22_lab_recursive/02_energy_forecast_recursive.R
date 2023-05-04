# BUSINESS SCIENCE LEARNING LABS ----
# LAB 54: AUTOREGRESSIVE FORECASTING ----
# ENERGY FORECASTING WITH MODELTIME RECURSIVE ----
# **** ----

# REFERENCES ----
# US Net Power Generation Monthly
# https://www.eia.gov/electricity/data/browser/

# LIBRARIES ----

# Use Development version of modeltime:
# remotes::install_github("business-science/modeltime")

# Machine Learning
library(modeltime)
library(tidymodels)

# Time Series
library(timetk)
library(lubridate)

# Tidyverse & Misc
library(tidyverse)
library(skimr)
library(janitor)


# LOAD DATA ----

data <- read_csv("data/Net_generation_for_all_sectors.csv")

data

# DATA REVIEW ----

# * Data Description ----
data_description_tbl <- data %>%
    clean_names() %>%
    select(description:source_key) %>%
    slice(-(1:2)) %>%
    separate(description, into = c("location", "fuel"), sep = " : ")

data_description_tbl

# * Data Pivoted ----
data_pivoted_tbl <- data %>%
    select(-description, -units) %>%
    mutate(across(.cols = -(`source key`), as.numeric)) %>%
    pivot_longer(cols = -`source key`, names_to = "date") %>%
    drop_na() %>%
    mutate(date = my(date)) %>%
    clean_names()

# * Checks ----

# Found something weird
data_pivoted_tbl %>%
    group_by(source_key) %>%
    tk_summary_diagnostics() %>%
    ungroup() %>%
    slice(10) %>%
    glimpse()

# Duplicates detected!
data_pivoted_tbl %>% filter(source_key == "ELEC.GEN.SUN-US-99.M") %>% plot_time_series(date, value)

data_pivoted_tbl %>% filter(source_key == "ELEC.GEN.SUN-US-99.M") %>% distinct()

# Fix duplicates with distinct
data_pivoted_tbl <- data_pivoted_tbl %>% distinct()

# Now we're good
data_pivoted_tbl %>%
    group_by(source_key) %>%
    tk_summary_diagnostics()

# * Visualize ----

data_pivoted_tbl %>%
    left_join(data_description_tbl %>% select(source_key, fuel)) %>%
    group_by(fuel) %>%
    plot_time_series(
        .date_var = date,
        .value = value,
        .facet_ncol = 3,
        .smooth = F,
        .title = "US Power Generation (Thousand MegaWatt-Hours)"
    )

data_pivoted_tbl %>%
    left_join(data_description_tbl %>% select(source_key, fuel)) %>%
    filter(fuel %in% unique(fuel)[1:3]) %>%
    group_by(fuel) %>%
    plot_acf_diagnostics(
        .date_var = date,
        .value = value,
    )

# TIME SERIES PREPARATION ----


FORECAST_HORIZON <- 24

# * Data Extended ----

data_extended_tbl <- data_pivoted_tbl %>%

    # Extend
    group_by(source_key) %>%
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>%

    tk_augment_lags(value, .lags = FORECAST_HORIZON, .names = "long_lag") %>%
    tk_augment_slidify(
        .value   = long_lag,
        .f       = ~mean(.x, na.rm=TRUE),
        .period  = c(0.5*FORECAST_HORIZON, FORECAST_HORIZON, FORECAST_HORIZON*2),
        .align   = "center",
        .partial = TRUE
    ) %>%

    ungroup()

data_extended_tbl

# * Transformer Function ----

transformer_function <- function(data) {
    data %>%
        group_by(source_key) %>%
        tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
        ungroup()
}

data_lagged_tbl <- data_extended_tbl %>% transformer_function()

data_prepared_tbl <- data_lagged_tbl %>% drop_na()

data_future_tbl <- data_lagged_tbl %>% filter(is.na(value))

# MODELING ----

# * Resamples ----

resamples <- time_series_cv(
    data_prepared_tbl,
    cumulative  = TRUE,
    assess      = FORECAST_HORIZON,
    slice_limit = 1
)

train_tbl <- training(resamples$splits[[1]]) %>%
    group_by(source_key) %>%
    mutate(value = ts_clean_vec(value, period = 12)) %>%
    ungroup()

test_tbl  <- testing(resamples$splits[[1]])

# * Recipes ----

# ** Lag Recipe ----

recipe_spec_lag <- recipe(value ~ ., data = train_tbl) %>%
    step_dummy(all_nominal()) %>%
    step_rm(date) %>%
    step_zv(all_predictors())

recipe_spec_lag %>% prep() %>% juice() %>% glimpse()

# ** Calendar + Long-Term Rolling Recipe ----

recipe_spec_calendar <- recipe(
        value ~ .,
        data = train_tbl %>% select(-contains("lag"), contains("roll"))
    ) %>%
    step_timeseries_signature(date) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_normalize(date_index.num, starts_with("date_year")) %>%
    step_rm(date) %>%
    step_zv(all_predictors())

recipe_spec_calendar %>% prep() %>% juice() %>% glimpse()

# ** Hybrid Short-Term Lag + Calendar Recipe ----

recipe_spec_hybrid <- recipe(value ~ ., data = train_tbl) %>%
    step_timeseries_signature(date) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_normalize(date_index.num, starts_with("date_year")) %>%
    step_rm(date) %>%
    step_zv(all_predictors())

recipe_spec_hybrid %>% prep() %>% juice() %>% glimpse()



# A. GLMNET ----

# * Elastic Net Penalized Regression ----
model_spec_glmnet <- linear_reg(
    penalty = 200,
    mixture = 0.99
) %>%
    set_engine("glmnet")

# * Workflows ----

wflw_fit_glmnet_lag <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_lag) %>%
    fit(train_tbl) %>%
    recursive(
        id         = "source_key",
        transform  = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

wflw_fit_glmnet_calendar <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_calendar) %>%
    fit(train_tbl)

wflw_fit_glmnet_hybrid <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_hybrid) %>%
    fit(train_tbl) %>%
    recursive(
        id = "source_key",
        transform = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

# * Calibration Results ----
calibration_glmnet_tbl <- modeltime_table(
    wflw_fit_glmnet_lag,
    wflw_fit_glmnet_calendar,
    wflw_fit_glmnet_hybrid
) %>%
    modeltime_calibrate(test_tbl)


calibration_glmnet_tbl %>% modeltime_accuracy()

test_forecast_glmnet_tbl <- calibration_glmnet_tbl %>%
    modeltime_forecast(
        new_data = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data = TRUE
    )

test_forecast_glmnet_tbl %>%
    group_by(source_key) %>%
    plot_modeltime_forecast(.facet_ncol = 3, .conf_interval_show = FALSE)

# B. XGBOOST ----

# * XGBoost Model ----
model_spec_xgboost <- boost_tree(
    mode       = "regression",
    learn_rate = 0.75,
    min_n      = 1,
    tree_depth = 12,
    loss_reduction = 0.001
) %>%
    set_engine("xgboost")

# * Workflows ----

wflw_fit_xgboost_lag <- workflow() %>%
    add_model(model_spec_xgboost) %>%
    add_recipe(recipe_spec_lag) %>%
    fit(train_tbl) %>%
    recursive(
        id         = "source_key",
        transform  = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

wflw_fit_xgboost_calendar <- workflow() %>%
    add_model(model_spec_xgboost) %>%
    add_recipe(recipe_spec_calendar) %>%
    fit(train_tbl)

wflw_fit_xgboost_hybrid <- workflow() %>%
    add_model(model_spec_xgboost) %>%
    add_recipe(recipe_spec_hybrid) %>%
    fit(train_tbl) %>%
    recursive(
        id = "source_key",
        transform = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

# * Calibration Results ----
calibration_xgboost_tbl <- modeltime_table(
    wflw_fit_xgboost_lag,
    wflw_fit_xgboost_calendar,
    wflw_fit_xgboost_hybrid
) %>%
    modeltime_calibrate(test_tbl)


calibration_xgboost_tbl %>% modeltime_accuracy()

test_forecast_xgboost_tbl <- calibration_xgboost_tbl %>%
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    )

test_forecast_xgboost_tbl %>%
    group_by(source_key) %>%
    plot_modeltime_forecast(.facet_ncol = 3, .conf_interval_show = FALSE)

# C. SVM ----

# * Model ----
model_spec_svm <- svm_rbf(
    mode = "regression",
    margin = 0.001
) %>%
    set_engine("kernlab")

# * Workflows ----

wflw_fit_svm_lag <- workflow() %>%
    add_model(model_spec_svm) %>%
    add_recipe(recipe_spec_lag) %>%
    fit(train_tbl) %>%
    recursive(
        id         = "source_key",
        transform  = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

wflw_fit_svm_calendar <- workflow() %>%
    add_model(model_spec_svm) %>%
    add_recipe(recipe_spec_calendar) %>%
    fit(train_tbl)

wflw_fit_svm_hybrid <- workflow() %>%
    add_model(model_spec_svm) %>%
    add_recipe(recipe_spec_hybrid) %>%
    fit(train_tbl) %>%
    recursive(
        id = "source_key",
        transform = transformer_function,
        train_tail = panel_tail(train_tbl, source_key, FORECAST_HORIZON)
    )

# * Calibration Results ----
calibration_svm_tbl <- modeltime_table(
    wflw_fit_svm_lag,
    wflw_fit_svm_calendar,
    wflw_fit_svm_hybrid
) %>%
    modeltime_calibrate(test_tbl)


calibration_svm_tbl %>% modeltime_accuracy()

test_forecast_svm_tbl <- calibration_svm_tbl %>%
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    )

test_forecast_svm_tbl %>%
    group_by(source_key) %>%
    plot_modeltime_forecast(.facet_ncol = 3, .conf_interval_show = FALSE)


# SELECT BEST ----

calibration_tbl <- modeltime_table(
    wflw_fit_xgboost_calendar,
    wflw_fit_svm_calendar,
    wflw_fit_glmnet_lag
) %>%
    modeltime_calibrate(test_tbl)

# * Global Accuracy ----
calibration_tbl %>% modeltime_accuracy()

# * Accuracy by Identifier ----
test_forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(test_tbl, keep_data = TRUE)

accuracy_by_identifier_tbl <- test_forecast_tbl %>%
    select(source_key, .model_id, .model_desc, .index, .value,  value) %>%
    group_by(source_key, .model_id, .model_desc) %>%
    summarize_accuracy_metrics(
        truth      = value,
        estimate   = .value,
        metric_set = default_forecast_accuracy_metric_set()
    ) %>%
    ungroup()

best_rmse_by_indentifier_tbl <- accuracy_by_identifier_tbl %>%
    group_by(source_key) %>%
    slice_min(rmse, n = 1) %>%
    ungroup()

# REFITTED FORECAST ----

refitted_tbl <- calibration_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl %>%
            group_by(source_key) %>%
            mutate(value = ts_clean_vec(value, period = 12)) %>%
            ungroup()
    )

future_forecast_tbl <- refitted_tbl %>%
    modeltime_forecast(
        new_data    = data_future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    )

future_forecast_tbl %>%
    group_by(source_key) %>%
    plot_modeltime_forecast(.facet_ncol = 3, .conf_interval_show = FALSE)

# FILTER BEST ----

actual_tbl <- future_forecast_tbl %>% filter(.model_desc == "ACTUAL")

future_forecast_best_tbl <- future_forecast_tbl %>%
    right_join(
        best_rmse_by_indentifier_tbl %>% select(source_key, .model_id, .model_desc),
        by = c(".model_id", ".model_desc", "source_key")
    )

bind_rows(
    actual_tbl,
    future_forecast_best_tbl
) %>%
    left_join(data_description_tbl %>% select(source_key, fuel)) %>%
    group_by(fuel) %>%
    plot_modeltime_forecast(.facet_ncol = 3, .conf_interval_show = FALSE)



