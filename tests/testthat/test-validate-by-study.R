context("validate-by-study.R")

manifest <- tibble::tribble(
  ~path, ~parent, ~individualID, ~specimenID,
  "/mypath/assay.csv", "syn499234", NA, NA,
  "/mypath/biosp.csv", "syn499234", NA, NA,
  "/mypath/indiv.csv", "syn499234", NA, NA,
  "/mypath/a1.csv", "syn499234", "a", "a1",
  "/mypath/b1.csv", "syn499234", "b", "b1",
  "/mypath/c1.csv", "syn499234", "c", "c1",
  "/mypath/d1.csv", "syn499234", "d", "d1"
)

assay <- tibble::tribble(
  ~specimenID, ~fileFormat,
  "a1", "fastq",
  "b1", "fastq",
  "c1", "fastq",
  "d1", "fastq"
)

individual <- tibble::tribble(
  ~individualID, ~fileFormat, ~species,
  "a", "fastq", "Human",
  "b", "fastq", "Human",
  "c", "fastq", "Human",
  "d", "fastq", "Human"
)

biospecimen <- tibble::tribble(
  ~individualID, ~specimenID, ~fileFormat,
  "a", "a1", "fastq",
  "b", "b1", "fastq",
  "c", "c1", "fastq",
  "d", "d1", "fastq"
)

manifest_template <- "syn20820080"
assay_template <- "syn12973256"
indiv_template <- "syn12973254"
biosp_template <- "syn12973252"

# Download annotation definitions
syn <- attempt_instantiate()
attempt_login(syn)
annotations <- tibble::tribble(
  ~key, ~value, ~columnType,
  "assay", "rnaSeq", "STRING",
  "fileFormat", "fastq", "STRING",
  "fileFormat", "txt", "STRING",
  "fileFormat", "csv", "STRING",
  "species", "Human", "STRING"
)

# ----- validate_study()

test_that("validate_study() returns a list", {
  skip_if_not(logged_in(syn = syn))
  view <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "file1", "human", NA, "individual", indiv_template, individual,
    "mystudy", "file2", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy", "file3", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy", "file4", NA, NA, "manifest", manifest_template, manifest
  )
  res <- validate_study(view, annotations, syn)
  expect_true(inherits(res, "list"))
})

test_that("validate_study() doesn't have check_all error if missing metadata", {
  skip_if_not(logged_in(syn = syn))
  view1 <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "file1", "human", "rnaSeq", "assay", assay_template, assay
  )
  view2 <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "file1", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy", "file2", "human", NA, "biospecimen", biosp_template, biospecimen
  )
  view3 <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "file1", "human", NA, "individual", indiv_template, individual,
    "mystudy", "file2", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy", "file3", NA, NA, "manifest", manifest_template, manifest
  )

  res1 <- validate_study(view1, annotations, syn)
  res2 <- validate_study(view2, annotations, syn)
  res3 <- validate_study(view3, annotations, syn)

  # Should get lists back for all, not errors
  expect_true(inherits(res1, "list"))
  expect_true(inherits(res2, "list"))
  expect_true(inherits(res3, "list"))
})

test_that("validate_study() returns NULL if no metadata", {
  view <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data
  )
  res <- validate_study(view, annotations, syn)

  expect_null(res)
})

# ----- validate_all_studies()
test_that("validate_all_studies() gets results for all studies", {
  skip_if_not(logged_in(syn = syn))
  view1 <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy1", "f1", "human", NA, "individual", indiv_template, individual,
    "mystudy1", "f2", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy2", "file3", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy2", "file4", NA, NA, "manifest", manifest_template, manifest
  )
  view2 <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy1", "f1", "human", NA, "individual", indiv_template, individual,
    "mystudy1", "f2", "human", NA, "biospecimen", biosp_template, biospecimen
  )
  res1 <- validate_all_studies(view1, annotations, syn)
  res2 <- validate_all_studies(view2, annotations, syn)

  # Should be a list with another list per study
  expect_true(inherits(res1, "list"))
  expect_true(all(purrr::map_lgl(res1, function(x) {
    inherits(x, "list")
  })))
  expect_equal(names(res1), c("mystudy1", "mystudy2"))
  expect_true(inherits(res2, "list"))
  expect_true(all(purrr::map_lgl(res2, function(x) {
    inherits(x, "list")
  })))
  expect_equal(names(res2), "mystudy1")
})

test_that("validate_all_studies() gets results for all studies", {
  view <- tibble::tribble(
    ~study, ~name, ~species, ~assay, ~metadataType, ~template, ~file_data
  )
  res <- validate_all_studies(view, annotations, syn)
  expect_null(res)
})
