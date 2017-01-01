# internal data table used by functions in this file
#
exp_pfix.df <-
  tibble::tibble(exponents =
                   c(-24, -21, -18, -15, -12, -9, -6, -3, -2, -1, 0,
                     1, 2, 3, 6, 9, 12, 15, 18, 21, 24),
                 ascii =
                   c("y", "z", "a", "f", "p", "n", "u", "m", "c", "d", "",
                     "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
                 utf8 =
                   c("y", "z", "a", "f", "p", "n", "\u03bc", "m", "c", "d", "",
                     "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
                 latex =
                   c("y", "z", "a", "f", "p", "n", "$\\mu$", "m", "c", "d", "",
                     "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
                 names =
                   c("yopto", "zepto", "atto", "fempto",
                     "pico", "nano", "micro", "milli", "centi", "deci", "",
                     "deca", "hecto", "kilo", "mega", "giga", "tera",
                     "peta", "exa", "zetta", "yotta"))

#' SI unit prefixes
#'
#' Convert SI unit prefixes into exponents of ten of multipliers and vice-versa.
#'
#' @param exponent numeric The power of 10 of the unit multiplier.
#' @param char.set character How to encode Greek letters and other fancy
#'    characters in prefixes: \code{"utf8"}, \code{"ascii"}, \code{"LaTeX"}.
#'
#' @export
#'
#' @note To change the default \code{char.set}, set R option
#'   \code{"photobiology.fancy.chars"}. Implementation is based on a table of
#'   data and extensible to any alphabet supported by R character objects by
#'   expanding the table.
#'
#' @examples
#'
#' exponent2prefix(3)
#' exponent2prefix(0)
#' exponent2prefix(-6)
#'
exponent2prefix <- function(exponent,
                            char.set = getOption("photobiology.fancy.chars",
                                                 default = "utf8")) {
  char.set <- tolower(gsub("-", "", char.set))
  if (char.set %in% names(exp_pfix.df)) {
    prefixes <- char.set
  } else {
    prefixes <- "ascii"
  }
  if (!exponent %in% exp_pfix.df[["exponents"]]) {
    NA_character_
  } else {
    idx <- which(exp_pfix.df[["exponents"]] == exponent)
    exp_pfix.df[[prefixes]][idx]
  }
}

#' @rdname exponent2prefix
#'
#' @export
#'
exponent2prefix_name <- function(exponent) {
  if (!exponent %in% exp_pfix.df[["exponents"]]) {
    NA_character_
  } else {
    idx <- which(exp_pfix.df[["exponents"]] == exponent)
    exp_pfix.df[["names"]][idx]
  }
}

#' @rdname exponent2prefix
#'
#' @param name character Long SI name of multiplier.
#'
#' @export
#'
prefix_name2exponent <- function(name) {
  name <- tolower(name)
  if (!name %in% exp_pfix.df[["names"]]) {
    NA_integer_
  } else {
    idx <- which(exp_pfix.df[["names"]] == name)
    exp_pfix.df[["exponents"]][idx]
  }
}

#' @rdname exponent2prefix
#'
#' @param prefix character Unit prefix used for multiplier.
#'
#' @export
#'
prefix2exponent <- function(prefix,
                            char.set = getOption("photobiology.fancy.chars",
                                                 default = "utf8")) {
  char.set <- tolower(gsub("-", "", char.set))
  if (char.set %in% names(exp_pfix.df)) {
    prefixes <- char.set
  } else {
    prefixes <- "ascii"
  }
  if (!prefix %in% exp_pfix.df[[prefixes]]) {
    NA_integer_
  } else {
    idx <- which(exp_pfix.df[[prefixes]] == prefix)
    exp_pfix.df[["exponents"]][idx]
  }
}

#' @rdname exponent2prefix
#'
#' @export
#'
has_SI_prefix <- function(exponent) {
  exponent %in% exp_pfix.df[["exponents"]]
}

#' @rdname exponent2prefix
#'
#' @export
#'
nearest_SI_exponent <- function(exponent) {
  while (!exponent %in% exp_pfix.df[["exponents"]] &&
         exponent > min(exp_pfix.df[["exponents"]])) {
    exponent <- exponent - 1L
  }
  max(exponent, min(exp_pfix.df[["exponents"]]))
}