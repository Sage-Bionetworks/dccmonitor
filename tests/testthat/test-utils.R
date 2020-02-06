context("utils.R")

test_that("get_most_recent_time gets the most recent time", {
  times1 <- c(NULL)
  times2 <- c(
    as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  )
  times3 <- c(
    as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  )
  times4 <- c(
    as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-15 01:30:01", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  )
  times5 <- c(
    as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-15 01:15:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-15 15:10:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-15 16:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  )
  times6 <- c(
    as.POSIXct(strptime("2019-11-23 22:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-11-15 22:34:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-12-04 10:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    as.POSIXct(strptime("2019-11-03 8:45:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  )
  res1 <- get_most_recent_time(times1)
  res2 <- get_most_recent_time(unclass(times2))
  res3 <- get_most_recent_time(unclass(times3))
  res4 <- get_most_recent_time(unclass(times4))
  res5 <- get_most_recent_time(unclass(times5))
  res6 <- get_most_recent_time(unclass(times6))
  expect_equal(res1, NULL)
  expect_equal(
    res2,
    unclass(as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))[1]
  )
  expect_equal(
    res3,
    unclass(as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))[1]
  )
  expect_equal(
    res4,
    unclass(as.POSIXct(strptime("2019-12-15 01:30:01", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))[1]
  )
  expect_equal(
    res5,
    unclass(as.POSIXct(strptime("2019-12-15 16:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))[1]
  )
  expect_equal(
    res6,
    unclass(as.POSIXct(strptime("2019-12-04 10:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))[1]
  )
})
