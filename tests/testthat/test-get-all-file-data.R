context("get-all-file-data.R")

data <- tibble::tribble(
  ~metadataType, ~species, ~assay,
  "manifest", NA, NA,
  "individual", "human", NA,
  "biospecimen", "human", NA,
  "assay", "human", "rnaSeq",
  NA, NA, NA
)

test_that("get_all_file_templates gets templates for all valid rows", {
  res <- get_all_file_templates(data)
  expect_equal(res$template[1], "syn20820080")
  expect_equal(res$template[2], "syn12973254")
  expect_equal(res$template[3], "syn12973252")
  expect_equal(res$template[4], "syn12973256")
  expect_true(is.na(res$template[5]))
})
