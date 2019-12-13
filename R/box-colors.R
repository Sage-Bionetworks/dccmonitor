#' Get color for documentation infobox
#'
#' Gets the color for the documentation infobox
#' based on number of documents.
#'
#' @param num_docs The number of documents.
box_docs_color <- function(num_docs) {
  if (num_docs > 0) {
    return("green")
  } else {
    return("red")
  }
}

#' Get color for metadata infobox
#'
#' Gets the color for the metadata infobox
#' based on the number of metadata files.
#'
#' @param num_meta_files The number of metadata files.
box_meta_color <- function(num_meta_files) {
  if (num_meta_files == 0) {
    return("red")
  } else if (num_meta_files > 0 && num_meta_files < 3) {
    return("orange")
  } else {
    return("green")
  }
}

#' Get color for success rate infobox
#'
#' Gets the color for the success rate infobox
#' based on the percent success rate.
#'
#' @param success_rate Percentage from 0 to 100.
box_success_color <- function(success_rate) {
  if (success_rate < 50) {
    return("red")
  } else if (success_rate > 50 && success_rate < 90) {
    return("orange")
  } else {
    return("green")
  }
}
