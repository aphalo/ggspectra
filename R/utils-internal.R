## internal

#' Check idfactor argument and retrieve if needed
#'
#' @param object generic_spct object.
#' @param idfactor character, NULL or NA The name of the factor identifying
#'   spectra when stored in long form.
#'
#' @keywords internal
#'
check_idfactor_arg <- function(object, idfactor = NULL, default = FALSE) {
  if (length(idfactor) > 1L) {
    warning("Length of 'idfactor' > 1, using: '", idfactor[1], "',")
    idfactor <- idfactor[1]
  }
  if (is.null(idfactor) || is.na(idfactor)) {
    idfactor <- photobiology::getIdFactor(object) # may return NA!
  }
  if (is.na(idfactor) || !is.character(idfactor)) {
    idfactor <- default || photobiology::getMultipleWl(object) > 1L
  }
  idfactor
}

#' Update idfactor name in object
#'
#' Conditionally call \code{\link[photobiology]{setIdFactor}}.
#'
#' @param object generic_spct object.
#' @param idfactor character The name of the factor identifying spectra when
#'   stored in long form.
#'
#' @note There is no check for \code{NA} or \code{NULL} as we assume that
#'   \code{\link{check_idfactor_arg}} has already been called on \code{idfactor}
#'   to check its sanity.
#'
#' @keywords internal
#'
rename_idfactor <- function(object, idfactor) {
  if (is.character(idfactor) && nzchar(idfactor) &&
      photobiology::getIdFactor(object) != idfactor) {
    object <- photobiology::setIdFactor(object, idfactor = idfactor)
  }
  object
}

#' Convert lubridate duration objects to a string if possible
#'
#' @param time.unit lubridate::duration object or character
#'
#' @keywords internal
#'
duration2character <- function(time.unit) {
  if (is.character(time.unit)) return(time.unit)
  if (!lubridate::is.duration(time.unit)) return("unknown")
  if (time.unit == lubridate::duration(1, "seconds")) return("second")
  if (time.unit == lubridate::duration(1, "hours")) return("hour")
  if (time.unit == lubridate::duration(1, "days")) return("day")
  "duration"
}

#' Apply normalization argument
#'
#' Apply the normalization issuing a message.
#'
#' @param x An R object.
#' @param norm numeric or character No longer supported, normalization is always
#'   updated in \code{autoplot()} if present and a unit conversion applied.
#'
#' @keywords internal
#'
apply_normalization <- function(x,
                                norm) {
  if (is.null(norm) || !is.na(norm)) {
    message("Normalization 'norm =", norm, "'applied before plotting.")
    photobiology::normalize(x)
  } else {
    x
  }
}

