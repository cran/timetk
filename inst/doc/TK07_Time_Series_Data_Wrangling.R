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

## -----------------------------------------------------------------------------
library(tidyverse)
library(tidyquant) 
library(timetk)

## -----------------------------------------------------------------------------
FANG

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE)

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  summarise_by_time(
    date, .by = "quarter",
    volume = SUM(volume)
  ) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE, .y_intercept = 0)

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  summarise_by_time(
    date, .by = "month",
    adjusted = FIRST(adjusted)
  ) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  filter_by_time(date, "2013-09", "2013") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

## ---- message=TRUE------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "auto") # Guesses .by = "day"

## -----------------------------------------------------------------------------
FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "hour") %>%
  mutate_at(vars(open:adjusted), .funs = ts_impute_vec, period = 1) %>%
  filter_by_time(date, "start", FIRST(date) %+time% "1 month") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE) 

## -----------------------------------------------------------------------------
# Make the rolling function
roll_avg_30 <- slidify(.f = AVERAGE, .period = 30, .align = "center", .partial = TRUE)

# Apply the rolling function
FANG %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  # Apply Sliding Function
  mutate(rolling_avg_30 = roll_avg_30(adjusted)) %>%
  pivot_longer(cols = c(adjusted, rolling_avg_30)) %>%
  plot_time_series(date, value, .color_var = name,
                   .facet_ncol = 2, .smooth = FALSE, 
                   .interactive = FALSE)

## -----------------------------------------------------------------------------
FANG %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  # Apply roll apply Function
  mutate(rolling_avg_30 = slidify_vec(adjusted,  ~ AVERAGE(.), 
                                      .period = 30, .partial = TRUE))

## -----------------------------------------------------------------------------
# Rolling regressions are easy to implement using `.unlist = FALSE`
lm_roll <- slidify(~ lm(..1 ~ ..2 + ..3), .period = 90, 
                   .unlist = FALSE, .align = "right")


FANG %>%
  select(symbol, date, adjusted, volume) %>%
  group_by(symbol) %>%
  mutate(numeric_date = as.numeric(date)) %>%
  # Apply rolling regression
  mutate(rolling_lm = lm_roll(adjusted, volume, numeric_date)) %>%
  filter(!is.na(rolling_lm))

