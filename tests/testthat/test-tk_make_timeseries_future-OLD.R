context("Testing tk_make_future_timeseries")

# tk_make_future_timeseries_signature -----

test_datetime <- c("2016-01-01 00:00:00",
                   "2016-01-01 00:00:03",
                   "2016-01-01 00:00:06") %>%
    ymd_hms()

test_that("tk_make_future_timeseries(datetime) test returns correct format.", {
    # No skip values
    expect_warning(
      test <- tk_make_future_timeseries(test_datetime, n_future = 3)
    )
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:15") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Skip values
    skip <- ymd_hms("2016-01-01 00:00:15")
    expect_warning(test <- tk_make_future_timeseries(test_datetime, n_future = 3, skip_values = skip))
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Skip values not within sequence
    skip <- ymd_hms("2016-01-01 00:00:10")
    expect_warning(expect_message(test <- tk_make_future_timeseries(test_datetime, n_future = 3, skip_values = skip)))
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:15") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Inspect validation of skip_values
    expect_error(expect_warning(test <- tk_make_future_timeseries(test_datetime, n_future = 10, skip_values = 1)))

    # Insert values
    insert <- tail(test_datetime, 1)
    expect_warning(test   <- tk_make_future_timeseries(test_datetime, n_future = 3, insert_values = insert))
    expectation <- c("2016-01-01 00:00:06", "2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:15") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Test Time Zones
    idx <- c("2015-04-05 00:00:00",
             "2015-04-05 01:00:00",
             "2015-04-05 02:00:00") %>%
      ymd_hms(tz = 'Africa/Bujumbura')

    expect_warning(test <- idx %>% tk_make_future_timeseries(n_future = 3))
    expectation <- c("2015-04-05 03:00:00",
                     "2015-04-05 04:00:00",
                     "2015-04-05 05:00:00") %>%
      ymd_hms(tz = 'Africa/Bujumbura')
    expect_equal(test, expectation)

})

test_date <- FANG %>%
    dplyr::filter(symbol == "FB") %>%
    tk_index()

test_that("tk_make_future_timeseries(date) test returns correct format.", {

    # DAILY SCALE

    # No skip values, inspect_weekdays = FALSE
    expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = NULL, inspect_weekdays = FALSE))
    expectation <- c("2016-12-31", "2017-01-01", "2017-01-02") %>%
        ymd()
    expect_equal(test, expectation)

    # No skip values, inspect_weekdays = TRUE
    expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = NULL, inspect_weekdays = TRUE))
    expectation <- c("2017-01-02") %>%
        ymd()
    expect_equal(test, expectation)

    # Skip values, inspect_weekdays = TRUE
    holidays <- c("2017-01-03", "2017-01-04") %>% lubridate::ymd()
    expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 8, skip_values = holidays, inspect_weekdays = TRUE))
    expectation <- c("2017-01-02", "2017-01-05", "2017-01-06") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = T: Test when skip values are not within future index
    skip <- ymd(c("2018-01-01", "2016-12-31"))
    expect_warning(expect_message(test <- tk_make_future_timeseries(test_date, n_future = 4, skip_values = skip, inspect_weekdays = T)))
    expectation <- c("2017-01-02", "2017-01-03") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = F: Test when skip values are not within future index
    skip <- ymd(c("2018-01-01", "2016-12-31"))
    expect_warning(expect_message(test <- tk_make_future_timeseries(test_date, n_future = 4, skip_values = skip, inspect_weekdays = F)))
    expectation <- c("2017-01-01", "2017-01-02", "2017-01-03") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = T: n_future missing
    expect_error(test <- tk_make_future_timeseries(test_date))

    # Inspect validation of skip_values
    expect_error(expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 10, skip_values = 1)))

    # Insert dates
    insert <- ymd(c("2017-01-03", "2017-01-01"))
    expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 3, inspect_weekdays = TRUE, insert_values = insert))
    expectation <- c("2017-01-01", "2017-01-02", "2017-01-03") %>%
        ymd()
    expect_equal(test, expectation)


    # WEEKLY SCALE

    # No skip
    test_date   <- c("2017-01-01", "2017-01-08") %>% lubridate::ymd()
    expectation <- c("2017-01-15", "2017-01-22") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2)),
                 expectation)

    # With skip
    test_date   <- c("2017-01-01", "2017-01-08") %>% lubridate::ymd()
    skip_values  <- c("2017-01-22") %>% lubridate::ymd()
    expectation <- c("2017-01-15") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, skip_values = skip_values)),
                 expectation)

    # With insert
    test_date   <- c("2017-01-01", "2017-01-08") %>% lubridate::ymd()
    insert      <- c("2017-01-29") %>% lubridate::ymd()
    expectation <- c("2017-01-15", "2017-01-22", "2017-01-29") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, insert_values = insert)),
                 expectation)

    # With insert / Message
    test_date   <- c("2017-01-01", "2017-01-08") %>% lubridate::ymd()
    insert      <- c("2017-01-22") %>% lubridate::ymd()
    expect_message(
      expect_warning(
        tk_make_future_timeseries(test_date, n_future = 2, insert_values = insert)
      )
    )

    # MONTHLY SCALE

    # No skip
    test_date   <- c("2017-01-01", "2017-02-01") %>% lubridate::ymd()
    expectation <- c("2017-03-01", "2017-04-01") %>% lubridate::ymd()
    expect_equal(
      expect_warning(tk_make_future_timeseries(test_date, n_future = 2)),
      expectation
    )

    # With skip
    test_date   <- c("2017-01-01", "2017-02-01") %>% lubridate::ymd()
    skip_values  <- c("2017-03-01") %>% lubridate::ymd()
    expectation <- c("2017-04-01") %>% lubridate::ymd()
    expect_equal(
      expect_warning(tk_make_future_timeseries(test_date, n_future = 2, skip_values = skip_values)),
      expectation
    )

    # With insert
    test_date   <- c("2017-01-01", "2017-02-01") %>% lubridate::ymd()
    insert      <- c("2017-06-01") %>% lubridate::ymd()
    expectation <- c("2017-03-01", "2017-04-01", "2017-06-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, insert_values = insert)),
                 expectation)

    # QUARTERLY SCALE

    # No skip
    test_date   <- c("2017-01-01", "2017-04-01") %>% lubridate::ymd()
    expectation <- c("2017-07-01", "2017-10-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2)),
                 expectation)

    # With skip
    test_date   <- c("2017-01-01", "2017-04-01") %>% lubridate::ymd()
    skip_values  <- c("2017-10-01") %>% lubridate::ymd()
    expectation <- c("2017-07-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, skip_values = skip_values)),
                 expectation)

    # With insert
    test_date   <- c("2017-01-01", "2017-04-01") %>% lubridate::ymd()
    insert      <- c("2018-01-01") %>% lubridate::ymd()
    expectation <- c("2017-07-01", "2017-10-01", "2018-01-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, insert_values = insert)),
                 expectation)

    # YEARLY SCALE

    # No skip
    test_date   <- c("2017-06-01", "2018-06-01") %>% lubridate::ymd()
    expectation <- c("2019-06-01", "2020-06-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2)),
                 expectation)

    # With skip
    test_date   <- c("2017-04-01", "2018-04-01") %>% lubridate::ymd()
    skip_values  <- c("2019-04-01") %>% lubridate::ymd()
    expectation <- c("2020-04-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, skip_values = skip_values)),
                 expectation)

    # 1.5 YEARLY SCALE

    # No skip
    test_date   <- c("2017-07-01", "2019-01-01") %>% lubridate::ymd()
    expectation <- c("2020-07-01", "2022-01-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2)),
                 expectation)

    # With skip
    skip_values  <- c("2022-01-01") %>% lubridate::ymd()
    test_date   <- c("2017-07-01", "2019-01-01") %>% lubridate::ymd()
    expectation <- c("2020-07-01") %>% lubridate::ymd()
    expect_equal(expect_warning(tk_make_future_timeseries(test_date, n_future = 2, skip_values = skip_values)),
                 expectation)


})

test_yearmon <- c("2016-01",
                  "2016-02",
                  "2016-03") %>%
    zoo::as.yearmon()

test_that("tk_make_future_timeseries(yearmon) test returns correct format.", {
    # No skip values
    expect_warning(test <- tk_make_future_timeseries(test_yearmon, n_future = 3))
    expectation <- c("2016-04", "2016-05", "2016-06") %>%
        zoo::as.yearmon()
    expect_equal(test, expectation)

    # Skip values
    skip <- zoo::as.yearmon("2016-05")
    expect_warning(test <- tk_make_future_timeseries(test_yearmon, n_future = 3, skip_values = skip))
    expectation <- c("2016-04", "2016-06")  %>%
        zoo::as.yearmon()
    expect_equal(test, expectation)

    # Test when skip values are not within future index
    skip <- zoo::as.yearmon(c("2016-10", "2016-11"))
    expect_message(expect_warning(tk_make_future_timeseries(test_yearmon, n_future = 3, skip_values = skip)))

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_yearmon, n_future = 10, skip_values = 1))
    expect_equal(test, NA)

    # insert values
    insert <- zoo::as.yearmon(c("2017-10", "2017-11"))
    expect_warning(test <- tk_make_future_timeseries(test_yearmon, n_future = 3, insert_values = insert))
    expectation <- c("2016-04", "2016-05", "2016-06", " 2017-10", "2017-11")  %>%
        zoo::as.yearmon()
    expect_equal(test, expectation)

})

test_yearqtr <- c("2016 Q1",
                  "2016 Q2",
                  "2016 Q3",
                  "2016 Q4") %>%
    zoo::as.yearqtr()

test_that("tk_make_future_timeseries(yearqtr) test returns correct format.", {
    # No skip values
    expect_warning(test <- tk_make_future_timeseries(test_yearqtr, n_future = 4))
    expectation <- c("2017 Q1", "2017 Q2", "2017 Q3", "2017 Q4") %>%
        zoo::as.yearqtr()
    expect_equal(test, expectation)

    # Skip values
    skip <- zoo::as.yearqtr("2017 Q1")
    expect_warning(test <- tk_make_future_timeseries(test_yearqtr, n_future = 4, skip_values = skip))
    expectation <- c("2017 Q2", "2017 Q3", "2017 Q4")  %>%
        zoo::as.yearqtr()
    expect_equal(test, expectation)

    # Test when skip values are not within future index
    skip <- zoo::as.yearqtr(c("2017 Q1", "2018 Q2"))
    expect_warning(expect_message(test <- tk_make_future_timeseries(test_yearqtr, n_future = 4, skip_values = skip)))
    expectation <- c("2017 Q2", "2017 Q3", "2017 Q4")  %>%
        zoo::as.yearqtr()
    expect_equal(test, expectation)

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_yearqtr, n_future = 10, skip_values = 1))
    expect_equal(test, NA)
})

# Test prediction algorithm -----

idx_every_two <-
    c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05",
      "2017-01-06",
      "2017-01-09", "2017-01-10", "2017-01-11", "2017-01-12",
      # "2017-01-13",
      "2017-01-16", "2017-01-17", "2017-01-18", "2017-01-19",
      "2017-01-20",
      "2017-01-23", "2017-01-24", "2017-01-25", "2017-01-26",
      # "2017-01-27",
      "2017-01-30", "2017-01-31", "2017-02-01", "2017-02-02",
      "2017-02-03",
      "2017-02-06", "2017-02-07", "2017-02-08", "2017-02-09",
      # "2017-02-10",
      "2017-02-13", "2017-02-14", "2017-02-15", "2017-02-16",
      "2017-02-17",
      "2017-02-20", "2017-02-21", "2017-02-22", "2017-02-23",
      # "2017-02-24",
      "2017-02-27", "2017-02-28", "2017-03-01", "2017-03-02",
      "2017-03-03") %>%
    ymd()

expect_every_two <-
    c("2017-03-06", "2017-03-07", "2017-03-08", "2017-03-09",
      # "2017-03-10",
      "2017-03-13", "2017-03-14", "2017-03-15", "2017-03-16",
      "2017-03-17",
      "2017-03-20", "2017-03-21", "2017-03-22", "2017-03-23",
      # "2017-03-24",
      "2017-03-27", "2017-03-28", "2017-03-29", "2017-03-30",
      "2017-03-31"
    ) %>%
    ymd()

test_that("tk_make_future_timeseries(predict_every_two) test returns correct format.", {
    expect_warning(test <- tk_make_future_timeseries(idx_every_two, n_future = 7 * 4, inspect_weekdays = TRUE))
    expect_equal(test, expect_every_two)
})

idx_every_three <-
    c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05",
      "2017-01-06",
      "2017-01-09", "2017-01-10", "2017-01-11", "2017-01-12",
      "2017-01-13",
      "2017-01-16", "2017-01-17", "2017-01-18", "2017-01-19",
      # "2017-01-20",
      "2017-01-23", "2017-01-24", "2017-01-25", "2017-01-26",
      "2017-01-27",
      "2017-01-30", "2017-01-31", "2017-02-01", "2017-02-02",
      "2017-02-03",
      "2017-02-06", "2017-02-07", "2017-02-08", "2017-02-09",
      # "2017-02-10",
      "2017-02-13", "2017-02-14", "2017-02-15", "2017-02-16",
      "2017-02-17",
      "2017-02-20", "2017-02-21", "2017-02-22", "2017-02-23",
      "2017-02-24",
      "2017-02-27", "2017-02-28", "2017-03-01", "2017-03-02"
      # "2017-03-03"
      ) %>%
    ymd()

expect_every_three <-
    c("2017-03-06", "2017-03-07", "2017-03-08", "2017-03-09",
      "2017-03-10",
      "2017-03-13", "2017-03-14", "2017-03-15", "2017-03-16",
      "2017-03-17",
      "2017-03-20", "2017-03-21", "2017-03-22", "2017-03-23",
      # "2017-03-24",
      "2017-03-27", "2017-03-28", "2017-03-29", "2017-03-30",
      "2017-03-31"
    ) %>%
    ymd()

test_that("tk_make_future_timeseries(predict_every_three) test returns correct format.", {
    expect_warning(test <- tk_make_future_timeseries(idx_every_three, n_future = 4 * 7 + 1, inspect_weekdays = TRUE))
    expect_equal(test, expect_every_three)
})

idx_every_four <-
       c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05",
         "2017-01-06",
         "2017-01-09", "2017-01-10", "2017-01-11", "2017-01-12",
         "2017-01-13",
         "2017-01-16", "2017-01-17", "2017-01-18", "2017-01-19",
         "2017-01-20",
         "2017-01-23", "2017-01-24", "2017-01-25", "2017-01-26",
         # "2017-01-27",
         "2017-01-30", "2017-01-31", "2017-02-01", "2017-02-02",
         "2017-02-03",
         "2017-02-06", "2017-02-07", "2017-02-08", "2017-02-09",
         "2017-02-10",
         "2017-02-13", "2017-02-14", "2017-02-15", "2017-02-16",
         "2017-02-17",
         "2017-02-20", "2017-02-21", "2017-02-22", "2017-02-23",
         # "2017-02-24",
         "2017-02-27", "2017-02-28", "2017-03-01", "2017-03-02",
         "2017-03-03") %>%
    ymd()

expect_every_four <-
    c("2017-03-06", "2017-03-07", "2017-03-08", "2017-03-09",
      "2017-03-10",
      "2017-03-13", "2017-03-14", "2017-03-15", "2017-03-16",
      "2017-03-17",
      "2017-03-20", "2017-03-21", "2017-03-22", "2017-03-23",
      # "2017-03-24",
      "2017-03-27", "2017-03-28", "2017-03-29", "2017-03-30",
      "2017-03-31"
      ) %>%
    ymd()

test_that("tk_make_future_timeseries(predict_every_four) test returns correct format.", {
    expect_warning(test <- tk_make_future_timeseries(idx_every_four, n_future = 4 * 7, inspect_weekdays = TRUE))
    expect_equal(test, expect_every_four)
})

idx_random <-
    c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05",
      "2017-01-06",
      "2017-01-09", "2017-01-10", "2017-01-11", "2017-01-12",
      "2017-01-13",
      "2017-01-16", "2017-01-17", "2017-01-18", "2017-01-19",
      "2017-01-20",
      "2017-01-23", "2017-01-24", "2017-01-25", "2017-01-26",
      # "2017-01-27",
      "2017-01-30", "2017-01-31", "2017-02-01", "2017-02-02",
      "2017-02-03",
      "2017-02-06", "2017-02-07", "2017-02-08", "2017-02-09",
      "2017-02-10",
      "2017-02-13", "2017-02-14", "2017-02-15", "2017-02-16",
      "2017-02-17",
      "2017-02-20", "2017-02-21", "2017-02-22", "2017-02-23",
      "2017-02-24",
      "2017-02-27", "2017-02-28", "2017-03-01", "2017-03-02",
      "2017-03-03",
      "2017-03-06", "2017-03-07", "2017-03-08", "2017-03-09",
      "2017-03-10",
      "2017-03-13", "2017-03-14", "2017-03-15", "2017-03-16",
      "2017-03-17",
      "2017-03-20", "2017-03-21", "2017-03-22", "2017-03-23",
      "2017-03-24") %>%
    ymd()

expect_random <-
    c("2017-03-27", "2017-03-28", "2017-03-29", "2017-03-30",
      "2017-03-31",
      "2017-04-03", "2017-04-04", "2017-04-05", "2017-04-06",
      "2017-04-07",
      "2017-04-10", "2017-04-11", "2017-04-12", "2017-04-13",
      "2017-04-14",
      "2017-04-17", "2017-04-18", "2017-04-19", "2017-04-20",
      "2017-04-21"
    ) %>%
    ymd()

test_that("tk_make_future_timeseries(predict_random) test returns correct format.", {
    expect_warning(test <- tk_make_future_timeseries(idx_random, n_future = 4 * 7, inspect_weekdays = TRUE))
    expect_equal(test, expect_random)
})

