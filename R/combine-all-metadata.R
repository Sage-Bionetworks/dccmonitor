#' Combine all metadata
#'
#' Combine all metadata into a single dataframe. Note that this function does
#' not handle errors when trying to combine data. For example, if there are
#' no common columns between the metadata files, then the function will throw
#' an error.
#'
#' @export
#' @param fileview The fileview for a specific study.
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

  # Convert all columns to character separately; using pipe results in error
  all_files <- purrr::map(
    fileview$file_data[unlist(file_indices)],
    function(x) {
      dplyr::mutate_all(x, as.character)
    }
  )
  # Manifest, if exists, should be first in list and should be joined on
  all_metadata <- Reduce(dplyr::left_join, all_files)

  all_metadata
}
