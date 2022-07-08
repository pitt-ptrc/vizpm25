#' python 
#'
#' @description import python functions
#'
#' @return python functions
#'
#' @noRd
#' @importFrom reticulate import virtualenv_create use_virtualenv


# global reference to usaddress (will be initialized in .onLoad)
usaddress <- NULL

.onLoad <- function(libname, pkgname) {
  virtualenv_create("testenv3", packages = c("usaddress"))
  use_virtualenv("testenv3", required = TRUE)
  
  # use superassignment to update global reference to usaddress
  usaddress <<- reticulate::import("usaddress", delay_load = TRUE)
}