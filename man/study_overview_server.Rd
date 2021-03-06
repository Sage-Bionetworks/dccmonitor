% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod-study-overview.R
\name{study_overview_server}
\alias{study_overview_server}
\title{Server for the study overview module}
\usage{
study_overview_server(
  input,
  output,
  session,
  fileview,
  annotations,
  annots_folder,
  syn,
  synapseclient,
  study
)
}
\arguments{
\item{input}{Shiny input}

\item{output}{Shiny output}

\item{session}{Shiny session}

\item{fileview}{The fileview for a specific study.}

\item{annotations}{A dataframe of annotation definitions.}

\item{annots_folder}{Synapse folder ID to store generated annotation csvs in.}

\item{syn}{Synapse client object.}

\item{synapseclient}{Synapse client.}

\item{study}{Name of the study.}
}
\description{
Server for the study overview module.
}
