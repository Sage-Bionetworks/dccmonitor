context("combine-all-metadata.R")

# Example data
biosp1 <- tibble::tibble(
  specimenID = c("a1", "b1", "c1"),
  otherBiosp = c("1", "2", "3")
)
biosp2 <- tibble::tibble(
  specimenID = c("a1", "b1", NA),
  individualID = c("a", "b", "c"),
  otherBiosp = c("1", "2", "3")
)
biosp3 <- tibble::tibble(
  specimenID = c("a1", "b1", NA),
  individualID = c("a", "b", "c"),
  otherBiosp = c("1", NA, "3")
)
biosp4 <- tibble::tibble(
  specimenID = c("a1", "b1", "c1"),
  individualID = c("a", "b", "c"),
  otherBiosp = c("1", "2", "3")
)
indiv1 <- tibble::tibble(
  individualID = c("a", "b", "c"),
  otherIndiv = c("1", "2", "3")
)
indiv2 <- tibble::tibble(
  individualID = c("a", NA, "c"),
  otherIndiv = c("1", "2", "3")
)
indiv3 <- tibble::tibble(
  individualID = c("a", NA, "c"),
  otherIndiv = c("1", "2", NA)
)
assay1 <- tibble::tibble(
  specimenID = c("a1", "b1", "c1"),
  otherAssay = c("1", "2", "3")
)
assay2 <- tibble::tibble(
  specimenID = c("a1", "b1", NA),
  otherAssay = c("1", "2", "3")
)
assay3 <- tibble::tibble(
  specimenID = c("a1", "b1", NA),
  otherAssay = c("1", NA, "3")
)
manifest1 <- tibble::tibble(
  individualID = c("a", "b", "c"),
  other = c("1", "2", "3"),
  path = c("indiv.csv", NA, NA)
)
manifest2 <- tibble::tibble(
  individualID = c("a", "b", "c"),
  specimenID = c("a1", "b1", "c1"),
  other = c("1", "2", "3"),
  path = c("assay.csv", "biosp.csv", "indiv.csv")
)
manifest3 <- tibble::tibble(
  individualID = c(NA, "b", "c", NA, NA, NA),
  specimenID = c("a1", NA, "c1", NA, NA, NA),
  other = c("1", "2", "3", NA, NA, NA),
  path = c(NA, NA, NA, "assay.csv", "biosp.csv", "indiv.csv")
)

test_that("combine_all_metadata returns expected join with manifest and no missing data", { # nolint

  # Only manifest and individual files with no missing IDs outide of metadata
  view1 <- tibble::tibble(
    metadataType = c("individual", "manifest"),
    file_data = c(list(indiv1), list(manifest1))
  )
  res1 <- combine_all_metadata(view1)
  expected1 <- tibble::tibble(
    individualID = c("a", "b", "c"),
    other = c("1", "2", "3"),
    path = c("indiv.csv", NA, NA),
    otherIndiv = c("1", "2", "3")
  )
  expect_equal(res1, expected1)

  # All file types with no missing IDs
  view2 <- tibble::tibble(
    metadataType = c("biospecimen", "manifest", "individual", "assay"),
    file_data = c(list(biosp1), list(manifest2), list(indiv1), list(assay1))
  )
  res2 <- combine_all_metadata(view2)
  expected2 <- tibble::tibble(
    individualID = c("a", "b", "c"),
    specimenID = c("a1", "b1", "c1"),
    other = c("1", "2", "3"),
    path = c("assay.csv", "biosp.csv", "indiv.csv"),
    otherBiosp = c("1", "2", "3"),
    otherAssay = c("1", "2", "3"),
    otherIndiv = c("1", "2", "3")
  )
  expect_equal(res2, expected2)
})

test_that("combine_all_metadata returns expected join with manifest and missing IDs", { # nolint

  # All file types with missing IDs in manifest
  view <- tibble::tibble(
    metadataType = c("biospecimen", "manifest", "individual", "assay"),
    file_data = c(list(biosp2), list(manifest3), list(indiv2), list(assay2))
  )
  res <- combine_all_metadata(view)
  expected <- tibble::tibble(
    individualID = c(NA, "b", "c", NA, NA, NA),
    specimenID = c("a1", NA, "c1", NA, NA, NA),
    other = c("1", "2", "3", NA, NA, NA),
    path = c(NA, NA, NA, "assay.csv", "biosp.csv", "indiv.csv"),
    otherBiosp = as.character(c(NA, NA, NA, NA, NA, NA)),
    otherAssay = c("1", "3", NA, "3", "3", "3"),
    otherIndiv = c("2", NA, "3", "2", "2", "2")
  )
  expect_equal(res, expected)
})

test_that("combine_all_metadata returns expected join with manifest and missing data", { # nolint

  # Only manifest and individual files with missing IDs and "other" data
  view1 <- tibble::tibble(
    metadataType = c("individual", "manifest"),
    file_data = c(list(indiv3), list(manifest1))
  )
  res1 <- combine_all_metadata(view1)
  expected1 <- tibble::tibble(
    individualID = c("a", "b", "c"),
    other = c("1", "2", "3"),
    path = c("indiv.csv", NA, NA),
    otherIndiv = c("1", NA, NA)
  )
  expect_equal(res1, expected1)

  # All file types with missing IDs and "other" data
  view2 <- tibble::tibble(
    metadataType = c("biospecimen", "manifest", "individual", "assay"),
    file_data = c(list(biosp3), list(manifest2), list(indiv3), list(assay3))
  )
  res2 <- combine_all_metadata(view2)
  expected2 <- tibble::tibble(
    individualID = c("a", "b", "c"),
    specimenID = c("a1", "b1", "c1"),
    other = c("1", "2", "3"),
    path = c("assay.csv", "biosp.csv", "indiv.csv"),
    otherBiosp = c("1", NA, NA),
    otherAssay = c("1", NA, NA),
    otherIndiv = c("1", NA, NA)
  )
  expect_equal(res2, expected2)
})

test_that("combine_all_metadata returns expected data join without manifest", {
  # Only individual files with no missing IDs
  view1 <- tibble::tibble(
    metadataType = "individual",
    file_data = list(indiv1)
  )
  res1 <- combine_all_metadata(view1)
  expect_equal(res1, indiv1)

  # individual and biospecimen files with no missing IDs
  view2 <- tibble::tibble(
    metadataType = c("biospecimen", "individual"),
    file_data = c(list(biosp4), list(indiv1))
  )
  res2 <- combine_all_metadata(view2)
  expected2 <- tibble::tibble(
    specimenID = c("a1", "b1", "c1"),
    individualID = c("a", "b", "c"),
    otherBiosp = c("1", "2", "3"),
    otherIndiv = c("1", "2", "3")
  )
  expect_equal(res2, expected2)

  # Biospecimen and assay files with no  missing IDs
  view3 <- tibble::tibble(
    metadataType = c("biospecimen", "assay"),
    file_data = c(list(biosp4), list(assay1))
  )
  res3 <- combine_all_metadata(view3)
  expected3 <- tibble::tibble(
    specimenID = c("a1", "b1", "c1"),
    individualID = c("a", "b", "c"),
    otherBiosp = c("1", "2", "3"),
    otherAssay = c("1", "2", "3")
  )
  expect_equal(res3, expected3)
})

test_that("combine_all_metadata returns NULL if no metadata files", {
  view1 <- tibble::tibble(
    metadataType = NA,
    file_data = NA
  )
  res1 <- combine_all_metadata(view1)
  res2 <- combine_all_metadata(NULL)
  expect_null(res1)
  expect_null(res2)
})

test_that("combine_all_metadata works if IDs are different types", {
  indiv <- tibble::tibble(
    individualID = c(1, 2, 3),
    otherIndivCol = c("a", "b", "c")
  )
  manifest <- tibble::tibble(
    individualID = c("1", "2", "3"),
    specimenID = c("4", "5", "6")
  )
  biosp <- tibble::tibble(
    specimenID = c(1, 2, 3)
  )
  view <- tibble::tibble(
    metadataType = c("manifest", "individual", "biospecimen"),
    file_data = c(list(manifest), list(indiv), list(biosp))
  )
  res <- combine_all_metadata(view)
  expected <- tibble::tibble(
    individualID = c("1", "2", "3"),
    specimenID = c("4", "5", "6"),
    otherIndivCol = c("a", "b", "c")
  )
  expect_equal(res, expected)
})
