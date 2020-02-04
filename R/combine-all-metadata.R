#' Combine all metadata
#'
#' Combine all metadata into a single dataframe. Note that this function does
#' not handle errors when trying to combine data. For example, if there are
#' no common columns between the metadata files, then the function will throw
#' an error.
#'
#' @export
#' @inheritParams edit_annotations_server
combine_all_metadata <- function(fileview) {
  if (is.null(fileview)) {
    return(NULL)
  }

  file_indices <- get_file_indices_named(
    fileview,
    c("manifest", "biospecimen", "assay", "individual")
  )
  if (length(file_indices) == 0) {
    return(NULL)
  }

  # Get manifest, if present
  if (!is.null(file_indices$manifest)) {
    manifest <- fileview$file_data[file_indices$manifest][[1]]
    # Should have NA in specimenID and/or individualID for rows referring to the
    # metadata files in manifest
    na_ids <- unique(
      c(which(is.na(manifest$specimenID)), which(is.na(manifest$individualID)))
    )
    # Remove NA id rows from manifest
    fileview$file_data[file_indices$manifest][[1]] <- manifest[-na_ids, ]
  }

  # Manifest, if exists, should be first in list and should be joined on
  all_metadata <- fileview$file_data[unlist(file_indices)] %>%
    Reduce(dplyr::left_join, .)

  all_metadata
}
