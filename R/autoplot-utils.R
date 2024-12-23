## internal

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

## internal

#' Check idfactor argument and retrieve if needed
#'
#' @param object generic_spct object.
#' @param idfactor character or NULL The name of the factor identifying
#'   spectra when stored in long form.
#'
#' @keywords internal
#'
check_idfactor_arg <- function(object, idfactor) {
  if (is.null(idfactor) || is.na(idfactor)) {
    idfactor <- photobiology::getIdFactor(object)
  }
  if (is.na(idfactor) || !is.character(idfactor)) {
    idfactor <- photobiology::getMultipleWl(object) > 1L
  }
  idfactor
}

#' Update idfactor name in object
#'
#' @param object generic_spct object.
#' @param idfactor character The name of the factor identifying spectra when
#'   stored in long form.
#'
#' @keywords internal
#'
rename_idfactor <- function(object, idfactor) {
  if (is.character(idfactor) &&
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

#' Warn if norm argument passed in call
#'
#' @param norm numeric or character No longer supported, normalization is always
#'   updated in \code{autoplot()} if present and a unit conversion applied.
#'
#' @keywords internal
#'
warn_norm_arg <- function(norm) {
  if (!is.na(norm)) {
    warning("On-the-fly normalization no longer supported. Use 'normalize()' instead.")
  }
}


