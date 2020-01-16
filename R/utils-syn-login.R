## Functions from dccvalidator to use with testing
## Check if we are running on travis
on_travis <- function() {
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    return(TRUE)
    # nocov start
  } else {
    return(FALSE)
  }
  # nocov end
}

## Look up env vars and log in to Synapse
syn_travis_login <- function(syn) {
  ## Credentials are encrypted on travis
  user <- Sys.getenv("SYNAPSE_USER")
  pass <- Sys.getenv("SYNAPSE_PASSWORD")
  syn$login(email = user, password = pass)
}

attempt_instantiate <- function() {
  if (reticulate::py_module_available("synapseclient")) {
    return(synapse$Synapse())
  } else {
    return(NULL)
  }
}

## Attempt to log in using encrypted travis variables if on travis within Sage
## org, or with regular synLogin() if not on travis. If on travis but not within
## Sage, do nothing.
attempt_login <- function(syn, ...) {
  if (on_travis() & !is.null(syn)) {
    try(syn_travis_login(syn), silent = TRUE)
  } else if (reticulate::py_module_available("synapseclient") & !is.null(syn)) {
    syn$login(...)
  } else {
    return(NULL)
  }
}

## Check if we're logged in
logged_in <- function(syn) {
  if (is.null(syn) || is.null(syn$username)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
