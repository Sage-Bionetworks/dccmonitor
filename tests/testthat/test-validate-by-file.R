context("validate-by-file.R")

# nolint start
manifest <- tibble::tribble(
  ~path, ~parent, ~individualID, ~specimenID, ~isMultiSpecimen, ~isModelSystem, ~fileFormat, ~consortium, ~study, ~grant, ~resourceType, ~dataType, ~dataSubtype, ~metadataType, ~analysisType, ~assay,
  "/mypath/assay.csv", "syn499234", NA, NA, "False", "False", "csv", "AMP-AD", "ACT", "R21MH102791", NA, NA, NA, "assay", NA, "rnaSeq",
  "/mypath/biosp.csv", "syn499234", NA, NA, "False", "False", "csv", "AMP-AD", "ACT", "R21MH102791", NA, NA, NA, "biospecimen", NA, NA,
  "/mypath/indiv.csv", "syn499234", NA, NA, "False", "False", "csv", "AMP-AD", "ACT", "R21MH102791", NA, NA, NA, "individual", NA, NA,
  "/mypath/a1.csv", "syn499234", "a", "a1", "False", "False", "raw", "AMP-AD", "ACT", "R21MH102791", "experimentalData", "proteomics", "dataMatrix", NA, "data normalization", NA,
  "/mypath/b1.csv", "syn499234", "b", "b1", "False", "False", "raw", "AMP-AD", "ACT", "R21MH102791", "experimentalData", "proteomics", "dataMatrix", NA, "data normalization", NA,
  "/mypath/c1.csv", "syn499234", "c", "c1", "False", "False", "raw", "AMP-AD", "ACT", "R21MH102791", "experimentalData", "proteomics", "dataMatrix", NA, "data normalization", NA,
  "/mypath/d1.csv", "syn499234", "d", "d1", "False", "False", "raw", "AMP-AD", "ACT", "R21MH102791", "experimentalData", "proteomics", "dataMatrix", NA, "data normalization", NA
)

assay <- tibble::tribble(
  ~specimenID, ~platform, ~RIN, ~rnaBatch, ~libraryBatch, ~sequencingBatch, ~libraryPrep, ~libraryPreparationMethod, ~isStranded, ~readStrandOrigin, ~runType, ~readLength,
  "a1", "HiSeq2000", 4, 2, 4, 5, "miRNAenrichment", "10x", "True", "forward", "singleEnd", 100,
  "b1", "HiSeq2000", 3, 2, 4, 5, "miRNAenrichment", "10x", "True", "forward", "singleEnd", 100,
  "c1", "HiSeq2000", 5, 2, 4, 5, "miRNAenrichment", "10x", "True", "forward", "singleEnd", 100,
  "d1", "HiSeq2000", 3, 2, 4, 5, "miRNAenrichment", "10x", "True", "forward", "singleEnd", 100
)

individual <- tibble::tribble(
  ~individualID, ~individualIdSource, ~species, ~sex, ~race, ~ethnicity, ~yearsEducation, ~ageDeath, ~descriptionDeath, ~yearAutopsy, ~apoeGenotype, ~pmi, ~pH, ~brainWeight, ~diagnosis, ~diagnosisCriteria, ~causeDeath, ~mannerDeath, ~CERAD, ~Braak,
  "a", "NIMH-HBCC", "Human", "male", "white", "hispanic or latino", 12, 98, "a person", 2017, "sure", "T", 3, 4, "Alzheimer Disease", "DSMV", "age", "age", "stuff", "morestuff",
  "b", "NIMH-HBCC", "Human", "female", "american indian", "hispanic or latino", 12, 78, "a person", 2017, "sure", "F", 5, 5, "Alzheimer Disease", "DSMV", "age", "age", "stuff", "morestuff",
  "c", "NIMH-HBCC", "Human", "male", "american indian", "hispanic or latino", 14, 84, "a person", 2017, "sure", "T", 4, 3.5, "Alzheimer Disease", "DSMV", "age", "age", "stuff", "morestuff",
  "d", "NIMH-HBCC", "Human", "female", "pacific islander", "not hispanic or latino", 13, 63, "a person", 2017, "nope", "T", 3, 4, "Bipolar Disorder", "DSMV", "age", "age", "stuff", "morestuff"
)

biospecimen <- tibble::tribble(
  ~individualID, ~specimenID, ~specimenIdSource, ~samplingDate, ~organ, ~tissue, ~BrodmannArea, ~sampleStatus, ~tissueWeight, ~tissueVolume, ~nucleicAcidSource, ~cellType, ~fastingState,
  "a", "a1", "NIMH-HBCC", "3/6/2018", "brain", "middle frontal gyrus", "words", "done", 0.1, 10, "bulk cell", "CD138+", 0,
  "b", "b1", "NIMH-HBCC", "3/6/2018", "brain", "middle frontal gyrus", "words", "done", 0.2, 5, "bulk cell", "CD138+", 0,
  "c", "c1", "NIMH-HBCC", "3/6/2018", "brain", "middle frontal gyrus", "words", "done", 0.05, 3, "bulk cell", "CD138+", 0,
  "d", "d1", "NIMH-HBCC", "3/6/2018", "brain", "middle frontal gyrus", "words", "done", 0.1, 7, "bulk cell", "CD138+", 0
)
# nolint end

manifest_template <- "syn20820080"
assay_template <- "syn12973256"
indiv_template <- "syn12973254"
biosp_template <- "syn12973252"

syn <- attempt_instantiate()
attempt_login(syn)

# Download annotation definitions
annotations <- dccvalidator::get_synapse_annotations(
  synID = config::get("annotations_table"),
  syn = syn
)

# ----- validate_manifest()

test_that("validate_manifest() returns list with check names", {
  res <- validate_manifest(
    manifest,
    manifest_template,
    annotations,
    syn
  )
  expect_type(res, "list")
  expect_named(
    res,
    c(
      "missing_cols",
      "annot_keys",
      "annot_values",
      "empty_cols",
      "complete_cols"
    )
  )
})

test_that("validate_manifest() returns list of correct objects", {
  res1 <- validate_manifest(
    manifest,
    manifest_template,
    annotations,
    syn
  )

  manifest2 <- manifest[, -which(names(manifest) == "assay")]
  res2 <- validate_manifest(
    manifest2,
    manifest_template,
    annotations,
    syn
  )

  expect_true(all(unlist(
    purrr::map(
      res1,
      function(x) {
        inherits(x, "check_pass")
      }
    )
  )))
  expect_equal(
    unlist(purrr::map(
      res2,
      function(x) {
        inherits(x, "check_pass")
      })
    ),
    c(
      missing_cols = FALSE,
      annot_keys = TRUE,
      annot_values = TRUE,
      empty_cols = TRUE,
      complete_cols = TRUE
    )
  )
  expect_true(inherits(res2$missing_cols, "check_fail"))
  expect_equal(res2$missing_cols$data, "assay")
})

# ----- validate_assay_meta()

test_that("validate_assay_meta() returns list with check names", {
  res <- validate_assay_meta(
    assay,
    assay_template,
    annotations,
    syn
  )
  expect_type(res, "list")
  expect_named(
    res,
    c(
      "missing_cols",
      "annot_values",
      "empty_cols",
      "complete_cols"
    )
  )
})

test_that("validate_assay_meta() returns list of correct objects", {
  res1 <- validate_assay_meta(
    assay,
    assay_template,
    annotations,
    syn
  )

  assay2 <- assay
  assay2$isStranded <- TRUE
  res2 <- validate_assay_meta(
    assay2,
    assay_template,
    annotations,
    syn
  )

  expect_true(all(unlist(
    purrr::map(
      res1,
      function(x) {
        inherits(x, "check_pass")
        }
    )
  )))
  expect_equal(
    unlist(purrr::map(
      res2,
      function(x) {
        inherits(x, "check_pass")
      })
    ),
    c(
      missing_cols = TRUE,
      annot_values = FALSE,
      empty_cols = TRUE,
      complete_cols = TRUE
    )
  )
  expect_true(inherits(res2$annot_values, "check_fail"))
  expect_equal(res2$annot_values$data, list(isStranded = TRUE))
})

# ----- validate_individual_meta()

test_that("validate_individual_meta() returns list with check names", {
  res <- validate_individual_meta(
    individual,
    indiv_template,
    annotations,
    syn
  )
  expect_type(res, "list")
  expect_named(
    res,
    c(
      "missing_cols",
      "annot_values",
      "dup_ids",
      "empty_cols",
      "complete_cols"
    )
  )
})

test_that("validate_individual_meta() returns list of correct objects", {
  res1 <- validate_individual_meta(
    individual,
    indiv_template,
    annotations,
    syn
  )

  individual2 <- individual
  individual2 <- rbind(individual2, as.list(individual[4, ]))
  res2 <- validate_individual_meta(
    individual2,
    indiv_template,
    annotations,
    syn
  )

  expect_true(all(unlist(
    purrr::map(
      res1,
      function(x) {
        inherits(x, "check_pass")
      }
    )
  )))
  expect_equal(
    unlist(purrr::map(
      res2,
      function(x) {
        inherits(x, "check_pass")
      })
    ),
    c(
      missing_cols = TRUE,
      annot_values = TRUE,
      dup_ids = FALSE,
      empty_cols = TRUE,
      complete_cols = TRUE
    )
  )
  expect_true(inherits(res2$dup_ids, "check_fail"))
  expect_equal(res2$dup_ids$data, "d")
})

# ----- validate_biospecimen_meta()

test_that("validate_biospecimen_meta() returns list with check names", {
  res <- validate_biospecimen_meta(
    biospecimen,
    biosp_template,
    annotations,
    syn
  )
  expect_type(res, "list")
  expect_named(
    res,
    c(
      "missing_cols",
      "annot_values",
      "dup_ids",
      "empty_cols",
      "complete_cols"
    )
  )
})

test_that("validate_biospecimen_meta() returns list of correct objects", {
  res1 <- validate_biospecimen_meta(
    biospecimen,
    biosp_template,
    annotations,
    syn
  )

  biospecimen2 <- biospecimen
  biospecimen2$specimenID <- NA
  res2 <- validate_biospecimen_meta(
    biospecimen2,
    biosp_template,
    annotations,
    syn
  )

  expect_true(all(unlist(
    purrr::map(
      res1,
      function(x) {
        inherits(x, "check_pass")
      }
    )
  )))
  expect_equal(
    unlist(purrr::map(
      res2,
      function(x) {
        inherits(x, "check_pass")
      })
    ),
    c(
      missing_cols = TRUE,
      annot_values = TRUE,
      dup_ids = TRUE,
      empty_cols = FALSE,
      complete_cols = FALSE
    )
  )
  expect_true(inherits(res2$empty_cols, "check_warn"))
  expect_true(inherits(res2$complete_cols, "check_fail"))
  expect_equal(res2$complete_cols$data, "specimenID")
})
