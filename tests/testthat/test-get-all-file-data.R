context("get-all-file-data.R")

view <- tibble::tribble(
  ~metadataType, ~species, ~assay, ~id,
  "manifest", NA, NA, "syn21249871",
  "individual", "human", NA, "syn21249874",
  "biospecimen", "human", NA, "syn21249873",
  "assay", "human", "proteomics", "syn21249872"
)

syn <- attempt_instantiate()
attempt_login(syn)

test_that("get_all_file_data returns file data as appended list column", {
  skip_if_not(logged_in(syn = syn))
  res <- get_all_file_data(view, syn)
  expect_true(inherits(res$file_data, "list"))
})
