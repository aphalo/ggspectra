
# Automatic title ---------------------------------------------------------

#' Build a plot title from object metadata (deprecated!)
#'
#' Build a character string to be used as plot title by quering spectral
#' object for its  metadata.
#'
#' @param x An R object.
#' @param default.what A character string.
#' @param ... Additional parameters of derived methods.
#'
#' @export
#'
#' @keywords internal
#'
title_spct <- function(x,
                       default.what = character(0),
                       ...) {
  what <- getWhatMeasured(x)
  if (is.na(what)) {
    what <- default.what
  }
  paste(what, collapse = ", ", ...)
}

# Automatic subtitle ---------------------------------------------------------

#' Build a plot subtitle from object metadata (deprecated!)
#'
#' Build a character string to be used as plot title by quering spectral
#' object for its  metadata.
#'
#' @param x An R object.
#' @param default.when datetime object.
#' @param default.where One-row data.frame with at least variables \code{lat},
#'   and \code{lon}.
#' @param ... Additional parameters of derived methods.
#'
#' @export
#'
#' @keywords internal
#'
subtitle_spct.generic_spct <- function(x,
                                       default.when = character(0),
                                       default.where = character(0),
                                       ...) {
  when <- getWhenMeasured(x)
  if (is.na(when)) {
    when <- default.when
  }
  where <- getWhereMeasured(x)
  if (anyNA(where[["lon"]]) | anyNA(where[["lat"]])) {
    where <- default.where
  } else {
    where <- paste(names(where), where, collapse = ", ", sep = ": ")
  }
  paste(when, where, sep = "; ", ...)
}

