#' @title Create a nicer version of the file info
#'
#' @description Updates come default Synapse columns to be more useful.
#' The column 'currentVersion' is changed to 'version' to reduce width of column
#' name. The columns 'createdBy' and 'modifiedBy' are changed to user names
#' instead of profile IDs. The columns 'createdOn' and 'modifiedOn' are changed
#' to a date. Note that these columns do not need to exist in the fileview.
#'
#' @param fileview The fileview table.
#' @param syn Synapse client object.
create_info_table <- function(fileview, syn) {
  # Fix some columns based on prior knowledge of how these look/get loaded
  # Only altering default Synapse columns
  if ("currentVersion" %in% names(fileview)) {
    # Rename currentVersion to just version for better printing
    names(fileview)[names(fileview) %in% "currentVersion"] <- "version"
  }
  # Give user name versus user id
  if ("createdBy" %in% names(fileview)) {
    fileview[["createdBy"]] <- unlist(purrr::map(
      fileview[["createdBy"]], function(x) get_user_name(x, syn)
    ))
  }
  if ("modifiedBy" %in% names(fileview)) {
    fileview[["modifiedBy"]] <- unlist(purrr::map(
      fileview[["modifiedBy"]], function(x) get_user_name(x, syn)
    ))
  }

  # Fix formatting on dates
  if ("createdOn" %in% names(fileview)) {
    fileview[["createdOn"]] <- unlist(purrr::map(
      fileview[["createdOn"]], function(x) format_date(x)
    ))
  }
  if ("modifiedOn" %in% names(fileview)) {
    fileview[["modifiedOn"]] <- unlist(purrr::map(
      fileview[["modifiedOn"]], function(x) format_date(x)
    ))
  }

  return(fileview)
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
