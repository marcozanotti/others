# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: DEEP LEARNING - GLUONTS & RETICULATE SETUP ----

# OBJECTIVES ----
# - Install GluonTS
# - Work with Python Environments via Reticulate

# REQUIREMENTS ----
# - Install latest Modeltime / Timetk Suite: 

# devtools::install_github("business-science/modeltime")
# devtools::install_github("business-science/modeltime.gluonts")
# devtools::install_github("business-science/modeltime.ensemble")
# devtools::install_github("business-science/modeltime.resample")
# devtools::install_github("business-science/timetk")



# 1.0 SETUP ----
# - Get the default 'r-gluonts' conda environment set up

# * Step 1: Load the Library ----
library(modeltime.gluonts)


# * Step 2: Install the Python Environment ----
install_gluonts()

# * Step 3: Restart R Session ----
library(modeltime.gluonts)

reticulate::py_discover_config()

is_gluonts_activated()

get_python_env()

# * Troubleshooting ----

# 1. No conda?

reticulate::conda_version()

reticulate::conda_install()
reticulate::install_miniconda()

# 2. Windows - Need Visual Studio C++ for Python
"https://github.com/business-science/modeltime.gluonts/issues/4"



# 2.0 TEST ----
# - Load modeltime.gluonts
# - Activates the 'r-gluonts' (default) environment

# Test a GluonTS DeepAR Model

library(modeltime.gluonts)
library(tidyverse)
library(tidymodels)
library(timetk)

# Data

df <- tibble(
    grp  = "A",
    date = tk_make_timeseries("2011", "2015", by = "month"),
    y    = 1:60
)

splits <- df %>%
    time_series_split(assess = 12, cumulative = TRUE)


model_fit_deepar <- deep_ar(
    id    = "grp",
    freq  = "M",
    prediction_length = 12,
    lookback_length   = 36
) %>%
    set_engine("gluonts_deepar") %>%
    fit(y ~ ., data = training(splits))

modeltime_table(
    model_fit_deepar
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = df
    ) %>%
    plot_modeltime_forecast()


# 3.0 RETICULATE NAVIGATION ----

library(reticulate)

# * Which Environments are Available? ----

conda_list()

virtualenv_list()



# * Which Environment am I Using? ----

py_discover_config()



# * What's in My Environment? ----

conda_path <- reticulate::conda_binary()

system2(conda_path, args = "list -n r-gluonts")

# 4.0 SETTING UP A CUSTOM PYTHON ENVIRONMENT ----

# Py Install

?reticulate::py_install()

py_install(
    packages = c(
        "mxnet==1.6",
        "gluonts==0.6.3",
        "numpy==1.16.6",
        "pandas==1.0.5",
        "scikit-learn==0.23.2",
        "matplotlib==3.3.2",
        "seaborn==0.11.0",
        "pathlib==1.0.1"
    ),
    envname = "my_gluonts_env",
    method  = "conda",
    python_version = "3.6",
    pip     = TRUE
)

# Find the Environment Python

reticulate::conda_list()


# 5.0 ACTIVATING A CUSTOM PYTHON ENV ----

# * Step 1. Requires a Session Restart ----

reticulate::py_discover_config()



# * Step 2. Get the Path to your Python Environment ----

library(tidyverse)

env_path <- reticulate::conda_list() %>%
    filter(name == "my_gluonts_env") %>%
    pull(python)

# * Step 3. Set an R environment variable ----

Sys.setenv(GLUONTS_PYTHON = env_path)

Sys.getenv("GLUONTS_PYTHON")


# Step 4. Load Modeltime - It will override the default using the new python environment
library(modeltime.gluonts)

reticulate::py_discover_config()

# Step 5. Common Gotchas:
# - If you've already activated an environment or run library(modeltime), you need to restart R
#   and set the Sys.setenv(GLUONTS_PYTHON) with the path to your python and run this first


# 6.0 RE-ACTIVATING THE DEFAULT ENV ----
# - Requires session restart

reticulate::py_discover_config()

reticulate::use_condaenv("r-gluonts", required = TRUE)

library(modeltime.gluonts)



