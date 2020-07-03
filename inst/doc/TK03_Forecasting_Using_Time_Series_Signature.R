## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    fig.width = 8, 
    fig.height = 4.5,
    fig.align = 'center',
    out.width='95%', 
    dpi = 100
)

# devtools::load_all() # Travis CI fails on load_all()

## ---- message = FALSE---------------------------------------------------------
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

# Used to convert plots from interactive to static
interactive = FALSE

## -----------------------------------------------------------------------------
# Read data
bike_transactions_tbl <- bike_sharing_daily %>%
  select(dteday, cnt) %>%
  set_names(c("date", "value")) 

bike_transactions_tbl

## -----------------------------------------------------------------------------
bike_transactions_tbl %>%
  plot_time_series(date, value, .interactive = interactive)

## -----------------------------------------------------------------------------
splits <- bike_transactions_tbl %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

## -----------------------------------------------------------------------------
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = interactive)

## -----------------------------------------------------------------------------
# Add time series signature
recipe_spec_timeseries <- recipe(value ~ ., data = training(splits)) %>%
    step_timeseries_signature(date) 

## -----------------------------------------------------------------------------
bake(prep(recipe_spec_timeseries), new_data = training(splits))

## -----------------------------------------------------------------------------
recipe_spec_final <- recipe_spec_timeseries %>%
    step_fourier(date, period = 365, K = 5) %>%
    step_rm(date) %>%
    step_rm(contains("iso"), contains("minute"), contains("hour"),
            contains("am.pm"), contains("xts")) %>%
    step_normalize(contains("index.num"), date_year) %>%
    step_dummy(contains("lbl"), one_hot = TRUE) 

juice(prep(recipe_spec_final))

## -----------------------------------------------------------------------------
model_spec_lm <- linear_reg(mode = "regression") %>%
    set_engine("lm")

## -----------------------------------------------------------------------------
workflow_lm <- workflow() %>%
    add_recipe(recipe_spec_final) %>%
    add_model(model_spec_lm)

workflow_lm

## -----------------------------------------------------------------------------
workflow_fit_lm <- workflow_lm %>% fit(data = training(splits))

## ---- paged.print = F---------------------------------------------------------
model_table <- modeltime_table(
  workflow_fit_lm
) 

model_table

## ---- paged.print = F---------------------------------------------------------
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

## -----------------------------------------------------------------------------
calibration_table %>%
  modeltime_forecast(actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = interactive)

## -----------------------------------------------------------------------------
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = interactive)

## -----------------------------------------------------------------------------
calibration_table %>%
  modeltime_refit(bike_transactions_tbl) %>%
  modeltime_forecast(h = "12 months", actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = interactive)

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("time_series_course.jpg")

