context("study-table.R")

test_that("get_study_table_latest only filters metadata files", {
  fileview <- tibble::tribble(
    ~study, ~metadataType, ~modifiedOn,
    # nolint start
    "a", NA, as.POSIXct(strptime("2019-11-23 19:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", NA, as.POSIXct(strptime("2019-11-23 22:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", "m", as.POSIXct(strptime("2019-11-23 10:02:10", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", "m", as.POSIXct(strptime("2019-11-23 10:03:01", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", "i", as.POSIXct(strptime("2019-11-23 22:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", "b", as.POSIXct(strptime("2019-11-22 08:10:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "a", "b", as.POSIXct(strptime("2019-11-21 07:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "b", "m", as.POSIXct(strptime("2019-11-20 02:10:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "b", NA, as.POSIXct(strptime("2019-11-21 23:05:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "b", "m", as.POSIXct(strptime("2019-11-23 22:35:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    "b", "m", as.POSIXct(strptime("2019-11-21 06:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    # nolint end
  )
  expected_a <- fileview[c(1, 2, 4, 5, 6), ]
  expected_b <- fileview[c(9, 10), ]
  res_a <- filter_study_table_latest(fileview, "a")
  res_b <- filter_study_table_latest(fileview, "b")
  expect_equal(res_a, expected_a)
  expect_equal(res_b, expected_b)
})
