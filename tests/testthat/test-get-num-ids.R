context("get-num-ids.R")

assay <- tibble::tribble(
  ~specimenID,
  "sa",
  "sb",
  "sc"
)
biospecimen <- tibble::tribble(
  ~specimenID, ~individualID,
  "sa", "ia",
  "sb", "ib",
  "sc", "ic",
  "sd", "id"
)
individual <- tibble::tribble(
  ~individualID,
  "ia",
  "ib",
  "ic",
  "id"
)
manifest <- tibble::tribble(
  ~specimenID, ~individualID,
  "sa", "ia",
  "sb", "ib",
  "sc", "ic",
  "sd", "id",
  "se", "ie"
)
manifest_duplicates <- tibble::tribble(
  ~specimenID, ~individualID,
  "sa", "ia",
  "sb", "ib",
  "sc", "ic",
  "sa", "ia",
  "sb", "ib",
  "sc", "ic"
)
study_view <- tibble::tribble(
  ~metadataType, ~file_data,
  "manifest", manifest,
  "individual", individual,
  "biospecimen", biospecimen,
  "assay", assay
)
study_view_duplicates <- tibble::tribble(
  ~metadataType, ~file_data,
  "manifest", manifest_duplicates,
  "individual", individual,
  "biospecimen", biospecimen,
  "assay", assay
)
study_view_missing <- tibble::tribble(
  ~metadataType, ~file_data,
  "individual", individual,
  "biospecimen", biospecimen,
  "assay", assay
)

test_that("num_individuals returns correct number", {
  res1 <- num_individuals(study_view)
  res2 <- num_individuals(study_view_duplicates)
  res3 <- num_individuals(study_view_missing)
  expect_equal(res1, 5)
  expect_equal(res2, 4)
  expect_equal(res2, 4)
})

test_that("num_individuals returns 0 if files not present", {
  view <- tibble::tribble(
    ~metadataType, ~file_data,
    "assay", assay
  )
  res1 <- num_individuals(view)
  expect_equal(res1, 0)
})

test_that("num_individuals doesn't break if $individualID missing", {
  manifest_no_ind <- tibble::tribble(
    ~specimenID,
    "sa",
    "sb",
    "sc",
    "sd",
    "se"
  )
  view <- tibble::tribble(
    ~metadataType, ~file_data,
    "manifest", manifest_no_ind,
    "individual", individual,
    "biospecimen", biospecimen,
    "assay", assay
  )
  res1 <- num_individuals(view)
  expect_equal(res1, 4)
})

test_that("num_specimens returns correct number", {
  res1 <- num_specimens(study_view)
  res2 <- num_specimens(study_view_duplicates)
  res3 <- num_specimens(study_view_missing)
  expect_equal(res1, 5)
  expect_equal(res2, 4)
  expect_equal(res3, 4)
})

test_that("num_specimens returns 0 if files not present", {
  view <- tibble::tribble(
    ~metadataType, ~file_data,
    "individual", individual,
  )
  res1 <- num_specimens(view)
  expect_equal(res1, 0)
})

test_that("num_specimens doesn't break if $specimenID missing", {
  manifest_no_sid <- tibble::tribble(
    ~individualID,
    "ia",
    "ib",
    "ic",
    "id"
  )
  view <- tibble::tribble(
    ~metadataType, ~file_data,
    "manifest", manifest_no_sid,
    "individual", individual,
    "biospecimen", biospecimen,
    "assay", assay
  )
  res1 <- num_specimens(view)
  expect_equal(res1, 4)
})
