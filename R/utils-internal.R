## internal

#' Check idfactor argument and retrieve if needed
#'
#' @param object generic_spct object.
#' @param idfactor character, NULL or NA The name of the factor identifying
#'   spectra when stored in long form.
#' @param default logical
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
    # with TRUE the name "spct.idx" is used by photobiology::setIdFactor()
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
#' @note There is no check for \code{NA_character_} as we assume that
#'   \code{\link{check_idfactor_arg}} has already been called on \code{idfactor}
#'   to check its sanity. \code{NULL} and non-character values including
#'   \code{NA} \emph{are ignored silently!}
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
#' @return A character string describing a time base or exposure duration.
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
  if (is.na(norm)) {
    x
  } else {
    if (norm != "update") {
      message("Normalization 'norm = ", norm, "' applied before plotting.",
              sep = ifelse(is.character(norm), "\"", ""))
    }
    photobiology::normalize(x, norm = norm)
  }
}

#' Get a normalization label
#'
#' Handle numeric and non-numeric values, as well as long form spectra
#'
#' @keywords internal
#'
normalization_label <- function(spct, digits = 1L) {
  stopifnot(all(unlist(is_normalised(spct), use.names = FALSE)))
  normalization <- photobiology::getNormalization(spct)
  if (all(is.na(normalization))) {
    return("NA")
  }
  if (photobiology::getMultipleWl(spct) == 1L) {
    # one normalization record
    if (is.numeric(normalization[["norm.wl"]])) {
      return(round(normalization[["norm.wl"]], digits = digits))
    }
    if (is.character(normalization[["norm.type"]])) {
      return(normalization[["norm.type"]])
    } else {
      round(photobiology::getNormalized(spct, .force.numeric = TRUE),
            digits = digits)
    }
  } else {
    # list with one normalization record per spectrum
    # use "norm.wls" if consistent
    norm.wls <- unique(sapply(normalization, `[[`, i = "norm.wl"))
    if (length(norm.wls) == 1L && !is.na(norm.wls)) {
      return(norm.wls)
    }
    # use "norm.type" if consistent
    norm.types <- unique(sapply(normalization, `[[`, i = "norm.type"))
    if (length(norm.types) == 1L && !is.na(norm.types)) {
      return(norm.types)
    } else {
      return("norm")
    }
  }
}
