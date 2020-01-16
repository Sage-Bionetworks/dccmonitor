context("validate-by-study.R")

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

# Download annotation definitions
syn <- attempt_instantiate()
attempt_login(syn)
annotations <- dccvalidator::get_synapse_annotations(
  synID = config::get("annotations_table"),
  syn
)

# ----- validate_study()

test_that("validate_study() manifest logic works correctly", {
  view <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", NA, NA, "manifest", manifest_template, manifest
  )
  res <- validate_study(view, annotations, syn)
  expect_equal(length(res$results[[1]]), 5)
  expect_equal(
    res$results[[1]]$missing_cols$message,
    "All manifest columns present"
  )
})

test_that("validate_study() assay logic works correctly", {
  view1 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", "rnaSeq", "assay", assay_template, assay
  )
  view2 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen
  )
  res1 <- validate_study(view1, annotations, syn)
  res2 <- validate_study(view2, annotations, syn)

  expect_equal(length(res1$results[[1]]), 4)
  expect_equal(
    res1$results[[1]]$missing_cols$message,
    "All assay metadata columns present"
  )

  expect_equal(length(res2$results[[1]]), 5)
  expect_equal(
    res2$results[[1]]$assay_biosp_ids$message,
    "All specimenID values match between biospecimen and assay"
  )
})

test_that("validate_study() biospecimen logic works correctly", {
  view1 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen
  )
  view2 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy", NA, NA, "manifest", manifest_template, manifest
  )
  res1 <- validate_study(view1, annotations, syn)
  res2 <- validate_study(view2, annotations, syn)

  expect_equal(length(res1$results[[1]]), 5)
  expect_equal(
    res1$results[[1]]$missing_cols$message,
    "All biospecimen columns present"
  )

  expect_equal(length(res2$results[[1]]), 6)
  expect_equal(
    res2$results[[1]]$biosp_manifest_ids$message,
    "All specimenID values match between biospecimen and manifest"
  )
})

test_that("validate_study() individual logic works correctly", {
  view1 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "individual", indiv_template, individual
  )
  view2 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "individual", indiv_template, individual,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen
  )
  view3 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "individual", indiv_template, individual,
    "mystudy", NA, NA, "manifest", manifest_template, manifest
  )
  view4 <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "individual", indiv_template, individual,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy", NA, NA, "manifest", manifest_template, manifest
  )
  res1 <- validate_study(view1, annotations, syn)
  res2 <- validate_study(view2, annotations, syn)
  res3 <- validate_study(view3, annotations, syn)
  res4 <- validate_study(view4, annotations, syn)

  expect_equal(length(res1$results[[1]]), 5)
  expect_equal(
    res1$results[[1]]$missing_cols$message,
    "All individual metadata columns present"
  )

  expect_equal(length(res2$results[[1]]), 6)
  expect_equal(
    res2$results[[1]]$indiv_biosp_ids$message,
    "All individualID values match between individual and biospecimen"
  )

  expect_equal(length(res3$results[[1]]), 6)
  expect_equal(
    res3$results[[1]]$indiv_manifest_ids$message,
    "All individualID values match between individual and manifest"
  )

  expect_equal(length(res4$results[[1]]), 7)
  expect_equal(
    res4$results[[1]]$indiv_biosp_ids$message,
    "All individualID values match between individual and biospecimen"
  )
  expect_equal(
    res4$results[[1]]$indiv_manifest_ids$message,
    "All individualID values match between individual and manifest"
  )
})

test_that("validate_study() gets results for all files", {
  view <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy", "human", NA, "individual", indiv_template, individual,
    "mystudy", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy", NA, NA, "manifest", manifest_template, manifest
  )
  res <- validate_study(view, annotations, syn)

  expect_equal(
    unlist(purrr::map(
      res$results,
      function(x) {
        length(x)
      }
    )),
    c(7, 6, 5, 5)
  )

  expect_true(all(
    unlist(purrr::map(
      res$results[[1]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[2]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[3]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[4]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
})

# ----- validate_all_studies()
test_that("validate_all_studies() gets results for all studies", {
  view <- tibble::tribble(
    ~study, ~species, ~assay, ~metadataType, ~template, ~file_data,
    "mystudy1", "human", NA, "individual", indiv_template, individual,
    "mystudy1", "human", NA, "biospecimen", biosp_template, biospecimen,
    "mystudy2", "human", "rnaSeq", "assay", assay_template, assay,
    "mystudy2", NA, NA, "manifest", manifest_template, manifest
  )
  res <- validate_all_studies(view, annotations, syn)

  expect_equal(
    unlist(purrr::map(
      res$results,
      function(x) {
        length(x)
      }
    )),
    c(6, 5, 4, 5)
  )

  expect_true(all(
    unlist(purrr::map(
      res$results[[1]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[2]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[3]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
  expect_true(all(
    unlist(purrr::map(
      res$results[[4]],
      function(x) {
        all(inherits(x, "check_pass"))
      }
    ))
  ))
})
