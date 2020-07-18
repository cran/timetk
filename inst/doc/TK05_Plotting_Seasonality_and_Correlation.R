## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.width = 8, 
  fig.height = 4.5,
  fig.align = 'center',
  out.width='95%', 
  dpi = 100,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

## ---- fig.height=6------------------------------------------------------------
m4_hourly %>%
    group_by(id) %>%
    plot_acf_diagnostics(
        date, value,               # ACF & PACF
        .lags = "7 days",          # 7-Days of hourly lags
        .interactive = interactive
    )

## ---- fig.height=8------------------------------------------------------------
walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
    group_by(id) %>%
    plot_acf_diagnostics(
        Date, Weekly_Sales,        # ACF & PACF
        .ccf_vars    = c(Temperature, Fuel_Price),   # CCFs
        .lags        = "3 months",    # 3 months of weekly lags
        .interactive = interactive
    )

## ---- fig.height=8------------------------------------------------------------
taylor_30_min %>%
    plot_seasonal_diagnostics(date, value, .interactive = interactive)

## ---- fig.height=8------------------------------------------------------------
m4_hourly %>%
    group_by(id) %>%
    plot_seasonal_diagnostics(date, value, .interactive = interactive)

## ---- fig.height=8------------------------------------------------------------
m4_hourly %>%
    group_by(id) %>%
    plot_stl_diagnostics(
        date, value,
        .frequency = "auto", .trend = "auto",
        .feature_set = c("observed", "season", "trend", "remainder"),
        .interactive = interactive)

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("time_series_course.jpg")

