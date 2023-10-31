## ----include = FALSE----------------------------------------------------------
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

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("timetk_version_2.jpg")

## ----setup--------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

## -----------------------------------------------------------------------------
taylor_30_min

## -----------------------------------------------------------------------------
taylor_30_min %>% 
  plot_time_series(date, value, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

## -----------------------------------------------------------------------------
m4_daily %>% group_by(id)

## -----------------------------------------------------------------------------
m4_daily %>%
  group_by(id) %>%
  plot_time_series(date, value, 
                   .facet_ncol = 2, .facet_scales = "free",
                   .interactive = interactive)

## -----------------------------------------------------------------------------
m4_hourly %>% group_by(id)

## -----------------------------------------------------------------------------
m4_hourly %>%
  group_by(id) %>%
  plot_time_series(date, log(value),             # Apply a Log Transformation
                   .color_var = week(date),      # Color applied to Week transformation
                   # Facet formatting
                   .facet_ncol = 2, 
                   .facet_scales = "free", 
                   .interactive = interactive)

## -----------------------------------------------------------------------------
taylor_30_min %>%
  plot_time_series(date, value, 
                   .color_var = month(date, label = TRUE),
                   
                   # Returns static ggplot
                   .interactive = FALSE,  
                   
                   # Customization
                   .title = "Taylor's MegaWatt Data",
                   .x_lab = "Date (30-min intervals)",
                   .y_lab = "Energy Demand (MW)",
                   .color_lab = "Month") +
  scale_y_continuous(labels = scales::label_comma())

## -----------------------------------------------------------------------------
m4_monthly %>%
    group_by(id) %>%
    plot_time_series_boxplot(
        date, value,
        .period      = "1 year",
        .facet_ncol  = 2,
        .interactive = FALSE)

## -----------------------------------------------------------------------------
m4_monthly %>%
    group_by(id) %>%
    plot_time_series_regression(
        .date_var     = date,
        .formula      = log(value) ~ as.numeric(date) + month(date, label = TRUE),
        .facet_ncol   = 2,
        .interactive  = FALSE,
        .show_summary = FALSE
    )

