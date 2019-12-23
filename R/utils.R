#' @title Get synapse fileview
#'
#' @description Download a synapse fileview and return as a tibble.
#'
#' @param fileview_id The synId for the full metadata Fileview.
#' @param syn Synapse client object.
#' @return Tibble with the Fileview information.
get_all_studies_table <- function(fileview_id, syn) {
  syntable <- syn$tableQuery(sprintf("SELECT * FROM %s", fileview_id))
  fileview <- tibble::as_tibble(syntable$asDataFrame())
  # Some columns end up as lists with NaN; fix these
  fileview$assay <- fix_list(fileview$assay)
  fileview$species <- fix_list(fileview$species)
  fileview$metadataType <- fix_list(fileview$metadataType)
  # File currentVersion should be an integer
  fileview$currentVersion <- as.integer(fileview$currentVersion)
  fileview
}

#' @title Fix character list
#'
#' @description Fix a list that should be all characters and
#'   should have `NA` fields, not `NaN` fields.
#'
#' @param data A list to fix.
#' @return The fixed character vector.
#' @keywords internal
fix_list <- function(data) {
  data <- as.character(data)
  fixed <- purrr::map(
    data,
    function(x) {
      if (x == "NaN") {
        x <- NA
      }
      x
    }
  )
  unlist(fixed)
}

#' @title Get study table with most recent metadata files
#'
#' @description Filter fileview to get study specific table
#' with most recent metadata files. The table will have all
#' documentation files, but will only keep the most
#' recent version of files for the following metadata
#' types: biospecimen, manifest, assay, individual.
#'
#' @param study The study name.
#' @inheritParams get_all_file_templates
filter_study_table_latest <- function(fileview, study) {
  study_table <- fileview[which(fileview$study == study), ]
  metadata_types <- unique(
    study_table$metadataType[!is.na(study_table$metadataType)]
  )
  # Remove all "old" metadata files
  for (type in metadata_types) {
    file_indices <- get_file_indices_vector(study_table, type)
    if (length(file_indices) > 1) {
      times <- study_table$modifiedOn[file_indices]
      times <- times[!is.na(times)]
      most_recent_time <- get_most_recent_time(times)
      to_remove <- intersect(
        file_indices,
        which(study_table$modifiedOn != most_recent_time)
      )
      if (length(to_remove) > 0) {
        study_table <- study_table[-to_remove, ]
      }
    }
  }
  study_table
}

#' @title Get most recent time
#'
#' @description Get the most recent time from within
#' a list of POSIX times in the form of seconds since epoch.
#'
#' @param times Vector of numeric times in seconds since epoch.
#' @return The most recent time in `times`.
get_most_recent_time <- function(times) {
  if (length(times) <= 1) {
    return(times)
  }
  most_recent <- times[1]
  for (date_time in times) {
    if (date_time > most_recent) {
      most_recent <- date_time
    }
  }
  most_recent
}

#' @title Get data from file
#'
#' @description Downloads a synapse file and returns data
#' inside as a tibble.
#'
#' @param id The synId for the file.
#' @param meta_type The metadata type of the file.
#'   "manifest" assumes tab-delimited text file;
#'   all other types currently assumed to be csv file.
#' @param syn Synapse client object.
#' @return Data from file in tibble.
get_data <- function(id, meta_type, syn) {
  data <- NULL
  file_info <- syn$get(id)

  if (meta_type == "manifest") {
    data <- utils::read.table(
      file_info$path,
      sep = "\t",
      header = TRUE,
      na.strings = "",
      stringsAsFactors = FALSE
    )
  } else {
    data <- utils::read.csv(
      file_info$path,
      na.strings = "",
      stringsAsFactors = FALSE
    )
  }

  return(tibble::as_tibble(data))
}

#' @title Gets the template synId
#'
#' @description Gets the template synId from the config file.
#'
#' @param metadata_type The metadata type.
#' @param species The species.
#' @param assay_type The type of assay.
#' @return synId for the template in Synapse, `NULL` if template
#'   not found in config file, or `NA` if `metadata_type` is `NA`.
get_template <- function(metadata_type, species = NA, assay_type = NA) {
  if (is.na(metadata_type)) {
    return(NA)
  }
  template <- switch(
    metadata_type,
    manifest = config::get("templates")$manifest_template,
    individual = {
      if (!is.na(species) && species != "human") {
        config::get("templates")$individual_templates[["animal"]]
      } else {
        config::get("templates")$individual_templates[[species]]
      }
    },
    biospecimen = {
      if (!is.na(species) && species != "drosophila") {
        config::get("templates")$biospecimen_templates[["general"]]
      } else {
        config::get("templates")$biospecimen_templates[[species]]
      }
    },
    assay = config::get("templates")$assay_templates[[assay_type]]
  )
  template
}
