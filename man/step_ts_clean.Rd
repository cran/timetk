% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recipes-step_ts_clean.R
\name{step_ts_clean}
\alias{step_ts_clean}
\alias{tidy.step_ts_clean}
\title{Clean Outliers and Missing Data for Time Series}
\usage{
step_ts_clean(
  recipe,
  ...,
  period = 1,
  lambda = "auto",
  role = NA,
  trained = FALSE,
  lambdas_trained = NULL,
  skip = FALSE,
  id = rand_id("ts_clean")
)

\method{tidy}{step_ts_clean}(x, ...)
}
\arguments{
\item{recipe}{A \code{recipe} object. The step will be added to the sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[=selections]{selections()}}
for more details. For the \code{tidy} method, these are not
currently used.}

\item{period}{A seasonal period to use during the transformation. If \code{period = 1},
linear interpolation is performed. If \code{period > 1}, a robust STL decomposition is
first performed and a linear interpolation is applied to the seasonally adjusted data.}

\item{lambda}{A box cox transformation parameter. If set to \code{"auto"}, performs
automated lambda selection.}

\item{role}{Not used by this step since no new variables are
created.}

\item{trained}{A logical to indicate if the quantities for preprocessing have been estimated.}

\item{lambdas_trained}{A named numeric vector of lambdas. This is \code{NULL} until computed
by \code{recipes::prep()}. Note that, if the original data are integers, the mean
will be converted to an integer to maintain the same a data type.}

\item{skip}{A logical. Should the step be skipped when the recipe
is baked by \code{bake.recipe()}? While all operations are baked when \code{prep.recipe()} is run,
some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}

\item{x}{A \code{step_ts_clean} object.}
}
\value{
An updated version of \code{recipe} with the new step
added to the sequence of existing steps (if any). For the
\code{tidy} method, a tibble with columns \code{terms} (the
selectors or variables selected) and \code{value} (the
lambda estimate).
}
\description{
\code{step_ts_clean} creates a \emph{specification} of a recipe
step that will clean outliers and impute time series data.
}
\details{
The \code{step_ts_clean()} function is designed specifically to handle time series
using seasonal outlier detection methods implemented in the Forecast R Package.

\strong{Cleaning Outliers}

#' Outliers are replaced with missing values using the following methods:
\enumerate{
\item Non-Seasonal (\code{period = 1}): Uses \code{stats::supsmu()}
\item Seasonal (\code{period > 1}): Uses \code{forecast::mstl()} with \code{robust = TRUE} (robust STL decomposition)
for seasonal series.
}

\strong{Imputation using Linear Interpolation}

Three circumstances cause strictly linear interpolation:
\enumerate{
\item \strong{Period is 1:} With \code{period = 1}, a seasonality cannot be interpreted and therefore linear is used.
\item \strong{Number of Non-Missing Values is less than 2-Periods}: Insufficient values exist to detect seasonality.
\item \strong{Number of Total Values is less than 3-Periods}: Insufficient values exist to detect seasonality.
}

\strong{Seasonal Imputation using Linear Interpolation}

For seasonal series with \code{period > 1}, a robust Seasonal Trend Loess (STL) decomposition is first computed.
Then a linear interpolation is applied to the seasonally adjusted data, and
the seasonal component is added back.

\strong{Box Cox Transformation}

In many circumstances, a Box Cox transformation can help. Especially if the series is multiplicative
meaning the variance grows exponentially. A Box Cox transformation can be automated by setting \code{lambda = "auto"}
or can be specified by setting \verb{lambda = numeric value}.
}
\examples{

library(dplyr)
library(tidyr)
library(recipes)

# Get missing values
FANG_wide <- FANG \%>\%
    select(symbol, date, adjusted) \%>\%
    pivot_wider(names_from = symbol, values_from = adjusted) \%>\%
    pad_by_time()

FANG_wide

# Apply Imputation
recipe_box_cox <- recipe(~ ., data = FANG_wide) \%>\%
    step_ts_clean(FB, AMZN, NFLX, GOOG, period = 252) \%>\%
    prep()

recipe_box_cox \%>\% bake(FANG_wide)

# Lambda parameter used during imputation process
recipe_box_cox \%>\% tidy(1)



}
\references{
\itemize{
\item \href{https://github.com/robjhyndman/forecast}{Forecast R Package}
\item \href{https://otexts.com/fpp2/missing-outliers.html}{Forecasting Principles & Practices: Dealing with missing values and outliers}
}
}
\seealso{
Time Series Analysis:
\itemize{
\item Engineered Features: \code{\link[=step_timeseries_signature]{step_timeseries_signature()}}, \code{\link[=step_holiday_signature]{step_holiday_signature()}}, \code{\link[=step_fourier]{step_fourier()}}
\item Diffs & Lags \code{\link[=step_diff]{step_diff()}}, \code{recipes::step_lag()}
\item Smoothing: \code{\link[=step_slidify]{step_slidify()}}, \code{\link[=step_smooth]{step_smooth()}}
\item Variance Reduction: \code{\link[=step_box_cox]{step_box_cox()}}
\item Imputation: \code{\link[=step_ts_impute]{step_ts_impute()}}, \code{\link[=step_ts_clean]{step_ts_clean()}}
\item Padding: \code{\link[=step_ts_pad]{step_ts_pad()}}
}
}
