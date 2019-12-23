#' @title Get color for documentation infobox
#'
#' @description Gets the color for the documentation
#' infobox based on number of documents.
#'
#' @param num_docs The number of documents.
box_docs_color <- function(num_docs) {
  if (!is.null(num_docs) && num_docs > 0) {
    return("green")
  } else {
    return("red")
  }
}

#' @title Get color for metadata infobox
#'
#' @description Gets the color for the metadata infobox
#' based on the number of metadata files.
#'
#' @param num_meta_files The number of metadata files.
box_meta_color <- function(num_meta_files) {
  if (is.null(num_meta_files) || num_meta_files == 0) {
    return("red")
  } else if (num_meta_files > 0 && num_meta_files < 3) {
    return("orange")
  } else {
    return("green")
  }
}

#' @title Get color for success rate infobox
#'
#' @description Gets the color for the success rate infobox
#' based on the percent success rate.
#'
#' @param success_rate Integer percentage from 0 to 100.
box_success_color <- function(success_rate) {
  if (is.null(success_rate) || success_rate <= 50) {
    return("red")
  } else if (success_rate > 50 && success_rate < 90) {
    return("orange")
  } else {
    return("green")
  }
}
