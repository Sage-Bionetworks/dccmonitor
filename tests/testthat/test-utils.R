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
    unclass(as.POSIXct(strptime("2019-12-15 01:30:00", "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
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

test_that("get_study_table_latest only filters metadata files", {
  fileview <- tibble::tribble(
    ~study, ~metadataType, ~modifiedOn,
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
  )
  expected_a <- fileview[c(1, 2, 4, 5, 6), ]
  expected_b <- fileview[c(9, 10), ]
  res_a <- filter_study_table_latest(fileview, "a")
  res_b <- filter_study_table_latest(fileview, "b")
  expect_equal(res_a, expected_a)
  expect_equal(res_b, expected_b)
})

test_that("get_template returns NULL if template not in config", {
  res1 <- get_template(metadata_type = "individual")
  res2 <- get_template(metadata_type = "assay")
  expect_null(res1)
  expect_null(res2)
})

test_that("get_template returns NA if metadata_type is NA", {
  res1 <- get_template(metadata_type = NA)
  expect_equal(res1, NA)
})

test_that("get_template returns correct default template", {
  res1 <- get_template(
    metadata_type = "individual",
    species = "human"
  )
  res2 <- get_template(
    metadata_type = "individual",
    species = "drosophila"
  )
  res3 <- get_template(
    metadata_type = "biospecimen",
    species = "drosophila"
  )
  res4 <- get_template(
    metadata_type = "biospecimen",
    species = "human"
  )
  res5 <- get_template(
    metadata_type = "assay",
    assay = "rnaSeq"
  )
  res6 <- get_template(
    metadata_type = "manifest"
  )
  expect_equal(res1, "syn12973254")
  expect_equal(res2, "syn12973253")
  expect_equal(res3, "syn20673251")
  expect_equal(res4, "syn12973252")
  expect_equal(res5, "syn12973256")
  expect_equal(res6, "syn20820080")
})
