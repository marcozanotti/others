# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: DEEP LEARNING - RETICULATE & GLUONTS ----

# GOAL: Forecast Weekly Revenue (Simple Case: Single Time Series)

# OBJECTIVES ----
# - Work with GluonTS via Reticulate
# - Understand the GluonTS List Dataset
# - Make a GluonTS Probabilistic Forecast via Reticulated Python
# - Showcase Modeltime GluonTS in R


# 1.0 LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.gluonts)

# Python 
library(reticulate)

# Plotting 
library(plotly)

# Core 
library(tidyverse)
library(timetk)


# 2.0 PYTHON & RETICULATE ----

# Check Environment - Should be 'r-gluonts'

py_discover_config()


# * Import Key Packages ----

gluonts <- import("gluonts", convert = FALSE)
py      <- import_builtins(convert = FALSE)



# * Using Python & R Together ----

base::abs()

py$abs(-5) %>% class()

py$abs(-5) %>% py_to_r() %>% class()

r_to_py(5) %>% class()


my_dict <- py$dict("key1" = "value 1", "key2" = "value 2") 

my_dict %>% class()

my_dict["key1"] %>% class()

my_dict["key1"] %>% py_to_r() %>% class()

# 3.0 DATA ----

# * Transactions Data ----

transactions_tbl <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl

transactions_tbl %>% plot_time_series(purchased_at, revenue)


# 4.0 DATA PROCESSING ----

full_data_tbl <- transactions_tbl %>%
    mutate(id = "total_revenue") %>%
    
    group_by(id) %>%
    future_frame(
        .length_out = 12,
        .bind_data  = TRUE
    ) %>%
    ungroup()


data_prepared_tbl <- full_data_tbl %>%
    filter(!is.na(revenue))

future_tbl <- full_data_tbl %>%
    filter(is.na(revenue))


# 5.0 GLUONTS LIST DATASET ----

# * Creating a gluonts ListDataset ----

?to_gluon_list_dataset()

data_prepared_list_dataset <- data_prepared_tbl %>%
    to_gluon_list_dataset(
        date_var  = purchased_at,
        value_var = revenue, 
        id_var    = id,
        freq      = "W"
    )

data_prepared_list_dataset


# * Examining a ListDataset ----

data_prepared_list_dataset %>% class()

data_prepared_list_dataset$list_data

data_prepared_list_dataset$list_data %>% py_to_r()

data_prepared_list_dataset$list_data %>% class()

data_prepared_list_dataset$list_data[0]


# * Converting to Pandas ----

to_pandas <- gluonts$dataset$util$to_pandas

data_prepared_list_dataset$list_data[0] %>% to_pandas() %>% class()

data_prepared_list_dataset$list_data[0] %>% to_pandas() %>% py_to_r() %>% as.numeric()

# 6.0 DeepAR Estimator ----
# - Documentation: https://ts.gluon.ai/api/gluonts/gluonts.model.deepar.html

# * Connect to Model & Trainer

DeepAREstimator <- gluonts$model$deepar$DeepAREstimator

Trainer         <- gluonts$trainer$Trainer


# * Model Specification 
DeepAR_spec_1 <- DeepAREstimator(
    freq              = "W", 
    prediction_length = 12,
    trainer           = Trainer(
        epochs = 5
    )
)

DeepAR_spec_1


# * Fitting the GluonTS Model

DeepAR_fit_1 <- DeepAR_spec_1$train(training_data = data_prepared_list_dataset)

DeepAR_fit_1 %>% class()


# 7.0 PREDICTION ----

# * Prediction Object ----

prediction <- DeepAR_fit_1$predict(data_prepared_list_dataset)

prediction %>% class()

first_prediction <- reticulate::iter_next(prediction)

first_prediction %>% class()



# * Probabilistic Forecasting  -----

first_prediction$mean %>% class()

first_prediction$mean %>% py_to_r() %>% as.numeric() 

first_prediction$mean_ts %>% class()


first_prediction$median

first_prediction$quantile(0.5)

first_prediction$quantile(0.75)

first_prediction$quantile(0.25)



# 8.0 MATPLOTLIB PROBABILISTIC VISUALIZATION ----

matplotlib <- import("matplotlib", convert = FALSE)

plt <- matplotlib$pyplot

plt$style$available %>% py_to_r()

plt$style$use("seaborn-dark-palette")

to_pandas(data_prepared_list_dataset$list_data[0])$plot()

first_prediction$plot(
    prediction_intervals = c(50, 90), 
    color = "g"
)

plt$show()

plt$close()

# 9.0 GGPLOT & PLOTLY PROBABILISTIC FORECAST VISUALIZATION ----

future_predictions_deepar_1_tbl <- future_tbl %>%
    mutate(
        revenue = first_prediction$mean %>% py_to_r() %>% as.numeric(),
        type    = "prediction",
        ci_lo   = first_prediction$quantile(0.25) %>% py_to_r() %>% as.numeric(),
        ci_hi   = first_prediction$quantile(0.75) %>% py_to_r() %>% as.numeric(),
        ci_lo2  = first_prediction$quantile(0.05) %>% py_to_r() %>% as.numeric(),
        ci_hi2  = first_prediction$quantile(0.95) %>% py_to_r() %>% as.numeric()
    )

g <- data_prepared_tbl %>%
    mutate(type = "actual") %>%
    bind_rows(future_predictions_deepar_1_tbl) %>%
    # Visualization
    plot_time_series(
        purchased_at, revenue,
        .color_var   = type, 
        .smooth      = FALSE,
        .interactive = FALSE
    ) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, fill = "gray50") +
    geom_ribbon(aes(ymin = ci_lo2, ymax = ci_hi2), alpha = 0.2, fill = "gray50")

plotly::ggplotly(g)

# 10.0 MODELTIME ----

?deep_ar

data_prepared_tbl

model_fit_deepar_1 <- deep_ar(
    id = "id",
    freq = "W",
    prediction_length = 12,
    epochs = 15
) %>%
    set_engine("gluonts_deepar") %>%
    fit(revenue ~ ., data = data_prepared_tbl)

model_fit_arima_1 <- arima_reg(seasonal_period = 4) %>%
    set_engine("auto_arima") %>%
    fit(revenue ~ purchased_at, data = data_prepared_tbl)

modeltime_table(
    model_fit_deepar_1,
    model_fit_arima_1
) %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()


# 11.0 SAVING / LOADING MODELS ----

# * GluonTS ----

fs::dir_create("00_models/deep_ar_1_revenue_weekly")

pathlib <- import("pathlib", convert = FALSE)

model_path <- pathlib$Path("00_models/deep_ar_1_revenue_weekly/") 

DeepAR_fit_1$serialize(model_path)

model_reloaded <- gluonts$model$predictor$Predictor$deserialize(path = model_path)

model_reloaded$predict(dataset = data_prepared_list_dataset)

# * Modeltime ----

modeltime_table(
    model_fit_deepar_1,
    model_fit_deepar_1
)

model_fit_deepar_1 %>%
    save_gluonts_model("00_models/deep_ar_1_revenue_weekly_mdl")

model_reloaded_2 <- load_gluonts_model(path = "00_models/deep_ar_1_revenue_weekly_mdl/")

predict(model_reloaded_2, new_data = future_tbl)


# 12.0 BONUS - Deep Factor Estimator ----

# * Model Specification 
# gluonts.model.deep_factor.DeepFactorEstimator

DeepFactorEstimator <- gluonts$model$deep_factor$DeepFactorEstimator

Trainer <- gluonts$trainer$Trainer

DeepFactor_spec_1 <- DeepFactorEstimator(
    freq = "W",
    prediction_length = 12,
    trainer = Trainer(epochs = 10)
)

DeepFactor_spec_1


# * Fitting the GluonTS DeepFactor Model ----

DeepFactor_fit_1 <- DeepFactor_spec_1$train(data_prepared_list_dataset)

DeepFactor_fit_1

predictor <- DeepFactor_fit_1$predict(dataset = data_prepared_list_dataset)

predictions <- iter_next(predictor)

# * Visualize  ----

matplotlib <- import("matplotlib", convert = FALSE)

plt <- matplotlib$pyplot

plt$style$use("fivethirtyeight")

to_pandas(data_prepared_list_dataset$list_data[0])$plot()

predictions$plot(prediction_intervals = c(50, 90))

plt$show()

plt$close()

# CONCLUSIONS ----

# PROS & CONS:
# - GLUONTS 
#   - [PRO] Wide range of algorithms
#   - [PRO] One round of training and probabilities are incorporated (for some models)
#   - [CON] More complex to work with ListDataset Structure
# - MODELTIME 
#   - [PRO] Simplifies creating ListDataset() objects
#   - [PRO] Can compare w/ other forecast algorithms (shown next)
#   - [PRO] Good for scaling up predictions (shown next)
#   - [CON] Requires 2 rounds of training to get confidence intervals
#   - [CON] Not all GluonTS Algorithms incorporated (more coming)

