.onAttach <- function(libname, pkgname) {
  # in the future check locale or environment variable to set defaults
  packageStartupMessage("ggspectra: default axis labels updated")
  invisible()
}
