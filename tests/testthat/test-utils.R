context("utils.R")

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
