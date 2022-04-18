#' A simple function for loading packages within a package development environment
#'
#' This function allows a user a load a package while guaranteeing code will install missing packages
#'
#' @param packages a charater/list of packages that all have to be in CRAN
#'
#' @export

load_packages <- function(packages){

  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))

}
