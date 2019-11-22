context("get-num-ids.R")

study_view <- tibble::tribble(
  ~metadataType,
  "manifest",
  "individual",
  "biospecimen",
  "assay"
)

study_view_missing <- tibble::tribble(
  ~metadataType,
  "manifest",
  "assay"
)

test_that("get_file_indices returns named list for metadataTypes present", {
  res1 <- get_file_indices(study_view, "manifest")
  res2 <- get_file_indices(study_view, "assay")
  res3 <- get_file_indices(study_view, "biospecimen")
  res4 <- get_file_indices(study_view, "individual")
  res5 <- get_file_indices(study_view, c("manifest", "biospecimen"))
  res6 <- get_file_indices(study_view, c("individual", "assay"))
  res7 <- get_file_indices(
    study_view,
    c("manifest", "biospecimen", "assay", "individual")
  )
  expect_equal(res1, list(manifest = 1))
  expect_equal(res2, list(assay = 4))
  expect_equal(res3, list(biospecimen = 3))
  expect_equal(res4, list(individual = 2))
  expect_equal(res5, list(manifest = 1, biospecimen = 3))
  expect_equal(res6, list(individual = 2, assay = 4))
  expect_equal(res7, list(
    manifest = 1,
    biospecimen = 3,
    assay = 4,
    individual = 2)
  )
})

test_that("get_file_indices doesn't return indices for missing metadataTypes", {
  res1 <- get_file_indices(study_view_missing, "biospecimen")
  res2 <- get_file_indices(study_view_missing, "individual")
  res3 <- get_file_indices(study_view_missing, c("individual", "biospecimen"))
  res4 <- get_file_indices(study_view_missing, c("manifest", "biospecimen"))
  res5 <- get_file_indices(study_view_missing, c("individual", "assay"))
  res6 <- get_file_indices(
    study_view_missing,
    c("manifest", "biospecimen", "assay", "individual")
  )
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
  expect_equal(res4, list(manifest = 1))
  expect_equal(res5, list(assay = 2))
  expect_equal(res6, list(manifest = 1, assay = 2))
})

test_that("get_file_indices_vector returns vector for present types", {
  res1 <- get_file_indices_vector(
    study_view_missing,
    c("manifest", "biospecimen")
  )
  res2 <- get_file_indices_vector(
    study_view_missing,
    c("individual", "assay")
  )
  res3 <- get_file_indices_vector(
    study_view_missing,
    c("manifest", "biospecimen", "assay", "individual")
  )
  expect_equal(res1, 1)
  expect_equal(res2, 2)
  expect_equal(res3, c(1, 2))
})

test_that("get_file_indices_vector returns NULL if missing all types", {
  res1 <- get_file_indices_vector(study_view_missing, "biospecimen")
  res2 <- get_file_indices_vector(study_view_missing, "individual")
  res3 <- get_file_indices_vector(
    study_view_missing,
    c("individual", "biospecimen")
  )
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
})

test_that("get_file_indices_named returns named list for present types", {
  res1 <- get_file_indices_named(
    study_view_missing,
    c("manifest", "biospecimen")
  )
  res2 <- get_file_indices_named(
    study_view_missing,
    c("individual", "assay")
  )
  res3 <- get_file_indices_named(
    study_view_missing,
    c("manifest", "biospecimen", "assay", "individual")
  )
  expect_equal(res1, list(manifest = 1))
  expect_equal(res2, list(assay = 2))
  expect_equal(res3, list(manifest = 1, assay = 2))
})

test_that("get_file_indices_vector returns NULL if missing all types", {
  res1 <- get_file_indices_named(study_view_missing, "biospecimen")
  res2 <- get_file_indices_named(study_view_missing, "individual")
  res3 <- get_file_indices_named(
    study_view_missing,
    c("individual", "biospecimen")
  )
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
})
