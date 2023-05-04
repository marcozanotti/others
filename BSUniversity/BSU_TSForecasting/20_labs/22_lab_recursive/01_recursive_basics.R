# BUSINESS SCIENCE LEARNING LABS ----
# LAB 54: AUTOREGRESSIVE FORECASTING ----
# RECURSIVE BASICS ----
# **** ----

# LIBRARIES ----

# Use Development version of modeltime:
# remotes::install_github("business-science/modeltime")

library(modeltime)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(timetk)


# MULTIPLE TIME SERIES (PANEL DATA) -----

m4_monthly

m4_monthly %>%
    group_by(id) %>%
    plot_time_series(date, value)

FORECAST_HORIZON <- 24

m4_extended <- m4_monthly %>%
    group_by(id) %>%
    future_frame(
        .length_out = FORECAST_HORIZON,
        .bind_data  = TRUE
    ) %>%
    ungroup()

# TRANSFORM FUNCTION ----
# - NOTE - We create lags by group
lag_transformer_grouped <- function(data){
    data %>%
        group_by(id) %>%
        tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
        ungroup()
}

m4_lags <- m4_extended %>%
    lag_transformer_grouped()

train_data <- m4_lags %>%
    drop_na()

future_data <- m4_lags %>%
    filter(is.na(value))

# Modeling Autoregressive Panel Data ----

# * Normal Linear Regression ----
model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ ., data = train_data)

# * Recursive Linear Regression ----
model_fit_lm_recursive <- model_fit_lm %>%
    recursive(

        # We add an id = "id" to specify the groups
        id         = "id",

        # Supply the transformation function used to generate the lags
        transform  = lag_transformer_grouped,

        # We use panel_tail() to grab tail by groups
        train_tail = panel_tail(train_data, id, FORECAST_HORIZON)
    )

# MODELTIME WORKFLOW FOR PANEL DATA ----

modeltime_table(
    model_fit_lm,
    model_fit_lm_recursive
) %>%
    modeltime_forecast(
        new_data    = future_data,
        actual_data = m4_monthly,
        keep_data   = TRUE
    ) %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .interactive = TRUE,
        .conf_interval_show = FALSE
    )

# Notice LM gives us an error while the recursive makes the forecast.
# This happens because recursive() tells the NA values to be filled
#   in use the lag transformer function.
