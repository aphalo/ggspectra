
# Automatic title ---------------------------------------------------------

#' Build a plot title from object metadata
#'
#' Build a character string to be used as plot title by quering spectral
#' object for its  metadata.
#'
#' @param x An R object.
#' @param format A character string.
#' @param ... Additional parameters of derived methods.
#'
#' @export
#'
build_title <- function(x) UseMethod("build_title")

#' @rdname build_title
#'
#' @export
#'
build_title.default <- function(x, format = NULL, ...) {
  character()
}

#' @rdname build_title
#'
#' @export
#'
build_title.generic_spct <- function(x, format = NULL, ...) {
  what <- getWhatMeasured(x)
  if (is.na(what)) {
    what <- character(0)
  }
  paste(what, collapse = ", ")
}

# Automatic subtitle ---------------------------------------------------------

#' Build a plot subtitle from object metadata
#'
#' Build a character string to be used as plot title by quering spectral
#' object for its  metadata.
#'
#' @param x An R object.
#' @param format A character string.
#' @param ... Additional parameters of derived methods.
#'
#' @export
#'
build_subtitle <- function(x) UseMethod("build_subtitle")

#' @rdname build_subtitle
#'
#' @export
#'
build_subtitle.default <- function(x, format = NULL, ...) {
  character()
}

#' @rdname build_subtitle
#'
#' @export
#'
build_subtitle.generic_spct <- function(x, format = NULL, ...) {
  where <- getWhereMeasured(x)
  if (any(is.na(where))) {
    where <- character(0)
  } else {
    where <- paste(names(where), where, collapse = ", ", sep = ": ")
  }
  when <- getWhenMeasured(x)
  if (is.na(when)) {
    when <- character(0)
  }
  paste(when, where, sep = "; ")
}

