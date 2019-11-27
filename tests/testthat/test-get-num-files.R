context("get-num-files.R")

manifest <- tibble::tribble(
  ~path, ~specimenID,
  "a/a", "a",
  "a/b", "b",
  "a/c", "c"
)

view1 <- tibble::tribble(
  ~metadataType, ~file_data,
  "manifest", manifest,
  "assay", NA,
  "biospecimen", NA,
  "individual", NA
)

view2 <- tibble::tribble(
  ~metadataType, ~file_data,
  "assay", NA,
  NA, NA,
  "manifest", manifest,
  "biospecimen", NA
)

view3 <- tibble::tribble(
  ~metadataType, ~file_data,
  "assay", NA,
  NA, NA,
  NA, NA,
  "manifest", manifest,
)

view4 <- tibble::tribble(
  ~metadataType, ~file_data,
  NA, NA,
)

test_that("num_meta_files returns correct number", {
  res1 <- num_meta_files(view1)
  res2 <- num_meta_files(view2)
  res3 <- num_meta_files(view3)
  res4 <- num_meta_files(view4)
  expect_equal(res1, 4)
  expect_equal(res2, 3)
  expect_equal(res3, 2)
  expect_equal(res4, 0)
})

test_that("num_manifest_files returns correct number", {
  res1 <- num_manifest_files(view1)
  res2 <- num_manifest_files(view4)
  expect_equal(res1, 3)
  expect_equal(res2, 0)
})

test_that("num_doc_files returns correct number", {
  res1 <- num_doc_files(view1)
  res2 <- num_doc_files(view2)
  res3 <- num_doc_files(view3)
  res4 <- num_doc_files(view4)
  expect_equal(res1, 0)
  expect_equal(res2, 1)
  expect_equal(res3, 2)
  expect_equal(res4, 1)
})
