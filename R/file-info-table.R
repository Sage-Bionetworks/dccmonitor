#' @title Create a nice, printable version of the file info
#'
#' @description Uses the fileview to create a table of relevant
#' file information.
#'
#' @param fileview The fileview table. Assumes the following
#'   columns exist:
#'     name, id, currentVersion, metadataType, species,
#'     assay, createdBy, createdOn, modifiedOn.
#' @param syn Synapse client object.
create_info_table <- function(fileview, syn) {
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

  # Rename currentVersion to just version for better printing
  names(info_table)[which(names(info_table) == "currentVersion")] <-
    "version"

  # Give user name versus user id
  info_table$createdBy <- purrr::map( # nolint
    info_table$createdBy,
    function(x) {
      get_user_name(x, syn)
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

#' @title Get the user name
#'
#' @description Get the user name based on Synapse Id.
#'
#' @param user_id Synapse id as a string or number.
#' @param syn Synapse client object.
#' @return User name as a string.
get_user_name <- function(user_id, syn) {
  profile <- syn$getUserProfile(as.character(user_id))
  profile$userName
}

#' @title Format date
#'
#' @description Format a date to be readable and print nicely.
#'
#' @param date POSIX date.
#' @return Date as a string.
format_date <- function(date) {
  as.character(
    as.POSIXlt(as.double(date / 1000), origin = "1970-01-01"),
    format = "%Y-%m-%d %H:%M",
    usetz = TRUE
  )
}
