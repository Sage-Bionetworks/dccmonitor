#' Create a nice, printable version of the file information
#'
#' Uses the fileview to create a table of relevant
#' file information.
#'
#' @param fileview The fileview table. Assumes the following
#'   columns exist:
#'     name, id, currentVersion, metadataType, species,
#'     assay, createdBy, createdOn, modifiedOn
create_info_table <- function(fileview) {
  info_table <- fileview[, c(
    "name",
    "id",
    "currentVersion",
    "metadataType",
    "species",
    "assay",
    "createdBy",
    "createdOn",
    "modifiedOn"
  )]

  # Give user name versus user id
  info_table$createdBy <- purrr::map( # nolint
    info_table$createdBy,
    function(x) {
      get_user_name(x)
    }
  )

  # Fix formatting on dates
  info_table$createdOn <- purrr::map( # nolint
    info_table$createdOn,
    function(x) {
      format_date(x)
    }
  )
  info_table$modifiedOn <- purrr::map( # nolint
    info_table$modifiedOn,
    function(x) {
      format_date(x)
    }
  )

  info_table
}

#' Get the user name
#'
#' Get the user name based on Synapse Id.
#'
#' @param user_id Synapse id as a string or number.
#' @return User name as a string.
get_user_name <- function(user_id) {
  profile <- synapser::synGetUserProfile(as.character(user_id))
  profile$userName
}

#' Format date
#'
#' Format a date to be readable and print nicely.
#'
#' @param date Date in...
#' @return Date as a string.
format_date <- function(date) {
  as.character(date, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
}
