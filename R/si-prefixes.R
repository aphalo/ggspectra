# internal data used by functions in this file
#
exp_pfix.df <-
  data.frame(exponents =
               c(-24, -21, -18, -15, -12, -9, -6, -3, -2, -1, 0,
                 1, 2, 3, 6, 9, 12, 15, 18, 21, 24),
             ascii =
               c("y", "z", "a", "f", "p", "n", "u", "m", "c", "d", "",
                 "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
             utf8 =
               c("y", "z", "a", "f", "p", "n", "\u00b5", "m", "c", "d", "",
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
#'    The difference between \code{"utf8"} and \code{"ascii"} is that the first
#'    uses UTF8 character "micro" (similar to Greek mu) and the second uses "u".
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
#' @param if.zero.exponent character string to return when exponent is equal to zero.
#'
#' @export
#'
#' @examples
#'
#' exponent2factor(3)
#' exponent2factor(0)
#' exponent2factor(0, NULL)
#' exponent2factor(0, "")
#' exponent2factor(-6)
#'
exponent2factor <- function(exponent = 0,
                            if.zero.exponent = "1") {
  if (exponent == 0 && !is.null(if.zero.exponent)) {
    if.zero.exponent
  } else {
    paste("10^{", exponent, "}", sep = "")
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

#' Formatter for plain labels discounting for SI multipliers
#'
#' The labels generated represent numbers rescaled to compensate for a change
#' in unit's by a factor of ten or by a power of ten.
#'
#' @return a function with single parameter x, a numeric vector, that
#'   returns a character vector
#' @param exponent numeric Power of 10 to use as multiplier
#' @param digits number of significant digits to show
#' @param ... other arguments passed on to \code{\link{format}}
#' @param x a numeric vector to format
#'
#' @export
#'
#' @examples
#'
#' SI_pl_format()(1:10)
#' SI_pl_format()(runif(10))
#' SI_pl_format(exponent = 2)(runif(10))
#' SI_plain(1:10)
#' SI_plain(runif(10))
#' SI_plain(runif(10), digits = 2)
SI_pl_format <- function(exponent = 0, digits = 3, ...) {
  function(x) SI_plain(x, exponent = exponent, digits = digits, ...)
}

#' @export
#' @rdname SI_pl_format
SI_plain <- function(x, exponent = 0, digits = 3, ...) {
  x <- x / 10^exponent
  x <- signif(x, digits)
  format(x, trim = TRUE, scientific = FALSE, ...)
}

#' Formatter for tagged labels using SI multipliers
#'
#' The labels generated represent the same numbers, but with trailing zeros
#' removed/added and compensated by attaching to each label an SI multiplier
#' "prefix".
#'
#' @note If the exponent passed has no SI prefix defined, the exponent will
#' be adjusted to match one.
#'
#' @return a function with single parameter x, a numeric vector, that
#'   returns a character vector
#' @param exponent numeric Power of 10 to use as multiplier
#' @param digits number of significant digits to show
#' @param ... other arguments passed on to \code{\link{format}}
#' @param x a numeric vector to format
#'
#' @export
#'
#' @examples
#'
#' SI_tg_format()(1:10)
#' SI_tg_format()(runif(10))
#' SI_tg_format(exponent = 2)(runif(10))
#' SI_tagged(1:10)
#' SI_tagged(runif(10))
#' SI_tagged(runif(10), digits = 2)
#'
SI_tg_format <- function(exponent = 0, digits = 3, ...) {
  function(x) SI_tagged(x, exponent = exponent, digits = digits, ...)
}

#' @export
#' @rdname SI_tg_format
SI_tagged <- function(x, exponent = 0, digits = 3, ...) {
  exponent <- nearest_SI_exponent(exponent)
  x <- x / 10^exponent
  x <- signif(x, digits)
  paste(format(x, trim = TRUE, scientific = FALSE, ...),
        exponent2prefix(exponent), sep = "")
}

