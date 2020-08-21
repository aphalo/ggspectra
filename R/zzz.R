.onAttach <- function(libname, pkgname) {
  # in the future check locale or environment variable
  packageStartupMessage("Labels default to British English")
  invisible()
}
