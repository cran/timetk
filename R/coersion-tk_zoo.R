#' Coerce time series objects and tibbles with date/date-time columns to xts.
#'
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `ts` class.
#' @param date_var __Applicable to tibbles and data frames only__.
#' Column name to be used to `order.by`.
#' `NULL` by default. If `NULL`, function will find the date or date-time column.
#' @param silent Used to toggle printing of messages and warnings.
#' @param ... Additional parameters to be passed to `xts::xts()`. Refer to `xts::xts()`.
#'
#' @return Returns a `zoo` object.
#'
#' @details `tk_zoo` is a wrapper for `zoo::zoo()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `zoo` class objects. There are three main advantages:
#'
#' 1. Non-numeric columns that are not removed via `select` are dropped and the user is warned.
#' This prevents an error or coercion issue from occurring.
#' 2. The date column is auto-detected if not specified by `date_var`. This takes
#' the effort off the user to assign a date vector during coercion.
#' 3. `ts` objects are automatically coerced if a "timetk index" is present. Refer to [tk_ts()].
#'
#' The `select` argument can be used to select subsets
#' of columns from the incoming data.frame.
#' Only columns containing numeric data are coerced.
#' The `date_var` can be used to specify the column with the date index.
#' If `date_var = NULL`, the date / date-time column is interpreted.
#' Optionally, the `order.by` argument from the underlying `zoo::zoo()` function can be used.
#' The user must pass a vector of dates or date-times if `order.by` is used.
#' _Important Note: The `...` arguments are passed to `xts::xts()`, which
#' enables additional information (e.g. time zone) to be an attribute of the `zoo` object._
#'
#' For non-data.frame object classes (e.g. `xts`, `zoo`, `timeSeries`, etc) the objects are coerced
#' using `zoo::zoo()`.
#'
#' `tk_zoo_` is a nonstandard evaluation method.
#'
#' @seealso [tk_tbl()], [tk_xts()], [tk_zooreg()], [tk_ts()]
#'
#' @examples
#' library(dplyr)
#'
#' ### tibble to zoo: Comparison between tk_zoo() and zoo::zoo()
#' data_tbl <- dplyr::tibble(
#'     date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
#'     x    = rep("chr values", 5),
#'     y    = cumsum(1:5),
#'     z    = cumsum(11:15) * rnorm(1))
#'
#' # zoo: Characters will cause error; order.by must be passed a vector of dates
#' zoo::zoo(data_tbl[,-c(1,2)], order.by = data_tbl$date)
#'
#' # tk_zoo: Character columns dropped with a warning; No need to specify dates (auto detected)
#' tk_zoo(data_tbl)
#'
#' # ts can be coerced back to zoo
#' data_tbl %>%
#'     tk_ts(start = 2016, freq = 365) %>%
#'     tk_zoo()
#'
#'
#' ### Using select and date_var
#' tk_zoo(data_tbl, select = y, date_var = date)
#'
#'
#' ### NSE: Enables programming
#' date_var <- "date"
#' select   <- "y"
#' tk_zoo_(data_tbl, select = select, date_var = date_var)
#'
#' @name tk_zoo
#' @export
tk_zoo <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    select   <- rlang::quo_name(rlang::enquo(select))
    date_var <- rlang::quo_name(rlang::enquo(date_var))

    # Coerce to xts then to zoo
    ret <- tk_xts_(data = data, select = select, date_var = date_var, silent = silent, ...)
    ret <- zoo::zoo(ret)

    return(ret)

}

#' @export
#' @rdname tk_zoo
tk_zoo_ <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    # Coerce to xts then to zoo
    ret <- tk_xts_(data = data, select = select, date_var = date_var, silent = silent, ...)
    ret <- zoo::zoo(ret)

    return(ret)

}
