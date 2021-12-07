#' Validate idfactor
#'
#' @param idfactor character, logical, NULL or NA. If a \code{character} string,
#'   it is validated as an R name, while logical values, NULL and NA are
#'   returned unchanged.
#'
#' @keywords internal
#'
validate_idfactor <- function(idfactor) {
  if (length(idfactor) > 1L) {
    stop("'idfactor' must be of length one or NULL, but is longer")
  }
  if (!is.null(idfactor) & !is.na(idfactor) & !is.logical(idfactor)) {
    idfactor.user <- idfactor
    idfactor <- make.names(idfactor)
    if (idfactor != idfactor.user) {
      message("'idfactor' sanitized from \"", idfactor.user,
              "\" into \"", idfactor, "\"")
    }
  }
  idfactor
}
