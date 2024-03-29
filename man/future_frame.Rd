% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-future_frame.R
\name{future_frame}
\alias{future_frame}
\title{Make future time series from existing}
\usage{
future_frame(
  .data,
  .date_var,
  .length_out,
  .inspect_weekdays = FALSE,
  .inspect_months = FALSE,
  .skip_values = NULL,
  .insert_values = NULL,
  .bind_data = FALSE
)
}
\arguments{
\item{.data}{A data.frame or tibble}

\item{.date_var}{A date or date-time variable.}

\item{.length_out}{Number of future observations. Can be numeric number or a phrase
like "1 year".}

\item{.inspect_weekdays}{Uses a logistic regression algorithm to inspect
whether certain weekdays (e.g. weekends) should be excluded from the future dates.
Default is \code{FALSE}.}

\item{.inspect_months}{Uses a logistic regression algorithm to inspect
whether certain days of months (e.g. last two weeks of year or seasonal days)
should be excluded from the future dates.
Default is \code{FALSE}.}

\item{.skip_values}{A vector of same class as \code{idx} of timeseries
values to skip.}

\item{.insert_values}{A vector of same class as \code{idx} of timeseries
values to insert.}

\item{.bind_data}{Whether or not to perform a row-wise bind of the \code{.data} and
the future data. Default: \code{FALSE}}
}
\value{
A tibble that has been extended with future date, date-time timestamps.
}
\description{
Make future time series from existing
}
\details{
This is a wrapper for \code{\link[=tk_make_future_timeseries]{tk_make_future_timeseries()}} that works on data.frames. It respects \code{dplyr} groups.

\strong{Specifying Length of Future Observations}

The argument \code{.length_out} determines how many future index observations to compute. It can be specified
as:
\itemize{
\item \strong{A numeric value} - the number of future observations to return.
\itemize{
\item The number of observations returned is \emph{always} equal to the value the user inputs.
\item The \strong{end date can vary} based on the number of timestamps chosen.
}
\item \strong{A time-based phrase} - The duration into the future to include (e.g. "6 months" or "30 minutes").
\itemize{
\item The \emph{duration} defines the \emph{end date} for observations.
\item The \strong{end date will not change} and those timestamps that fall within the end date will be returned
(e.g. a quarterly time series will return 4 quarters if \code{.length_out = "1 year"}).
\item The number of observations will vary to fit within the end date.
}
}

\strong{Weekday and Month Inspection}

The \code{.inspect_weekdays} and \code{.inspect_months} arguments apply to "daily" (scale = "day") data
(refer to \code{tk_get_timeseries_summary()} to get the index scale).
\itemize{
\item The \code{.inspect_weekdays} argument is useful in determining missing days of the week
that occur on a weekly frequency such as every week, every other week, and so on.
It's recommended to have at least 60 days to use this option.
\item The \code{.inspect_months} argument is useful in determining missing days of the month, quarter
or year; however, the algorithm can inadvertently select incorrect dates if the pattern
is erratic.
}

\strong{Skipping / Inserting Values}

The \code{.skip_values} and \code{.insert_values} arguments can be used to remove and add
values into the series of future times. The values must be the same format as the \code{idx} class.
\itemize{
\item The \code{.skip_values} argument useful for passing holidays or special index values that should
be excluded from the future time series.
\item The \code{.insert_values} argument is useful for adding values back that the algorithm may have
excluded.
}

\strong{Binding with Data}

Rowwise binding with the original is so common that
I've added an argument \code{.bind_data} to perform a row-wise
bind of the future data and the incoming data.

This \emph{replaces} the need to do:

\if{html}{\out{<div class="sourceCode">}}\preformatted{df \%>\%
   future_frame(.length_out = "6 months") \%>\%
   bind_rows(df, .)
}\if{html}{\out{</div>}}

Now you can just do:

\if{html}{\out{<div class="sourceCode">}}\preformatted{df \%>\%
    future_frame(.length_out = "6 months", .bind_data = TRUE)
}\if{html}{\out{</div>}}
}
\examples{
\donttest{
library(dplyr)

# 30-min interval data
taylor_30_min \%>\%
    future_frame(date, .length_out = "1 week")

# Daily Data (Grouped)
m4_daily \%>\%
    group_by(id) \%>\%
    future_frame(date, .length_out = "6 weeks")

# Specify how many observations to project into the future
m4_daily \%>\%
    group_by(id) \%>\%
    future_frame(date, .length_out = 100)

# Bind with Original Data
m4_daily \%>\%
    group_by(id) \%>\%
    future_frame(date, .length_out = 100, .bind_data = TRUE)

holidays <- tk_make_holiday_sequence(
    start_date = "2017-01-01",
    end_date   = "2017-12-31",
    calendar   = "NYSE")

weekends <- tk_make_weekend_sequence(
    start_date = "2017-01-01",
    end_date   = "2017-12-31"
)

FANG \%>\%
    group_by(symbol) \%>\%
    future_frame(
        .length_out       = "1 year",
        .skip_values      = c(holidays, weekends)
    )
}

}
\seealso{
\itemize{
\item Making Future Time Series: \code{\link[=tk_make_future_timeseries]{tk_make_future_timeseries()}} (Underlying function)
}
}
