# internal
#
exp_pfix.df <-
  tibble::tibble(exponents = c(-24, -21, -18, -15, -12, -9, -6, -3, -2, -1, 0,
                           1, 2, 3, 6, 9, 12, 15, 18, 21, 24),
             prefixes = c("y", "z", "a", "f", "p", "n", "u", "m", "c", "d", "",
                          "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
             prefixes.utf8 = c("y", "z", "a", "f", "p", "n", "\u03bc", "m", "c", "d", "",
                          "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
             prefixes.latex = c("y", "z", "a", "f", "p", "n", "$\\mu$", "m", "c", "d", "",
                               "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
             names = c("yopto", "zepto", "atto", "fempto",
                       "pico", "nano", "micro", "milli", "centi", "deci", "",
                       "deca", "hecto", "kilo", "mega", "giga", "tera",
                       "peta", "exa", "zetta", "yotta"))

#' SI unit prefixes
#'
#' Convert SI unit prefixes to exponents and vice-versa.
#'
#' @param exponent numeric The power of 10 of the unit multiplier.
#' @param encoding character How to encode Greek letters, "utf8", "ascii",
#'   "LaTeX".
#'
#' @export
#'
#' @examples
#'
#' exponent2prefix(3)
#' exponent2prefix(0)
#' exponent2prefix(-6)
#'
exponent2prefix <- function(exponent,
                            encoding = "utf8") {
  encoding <- tolower(encoding)
  if (encoding == "utf8") {
    prefixes <- "prefixes.utf8"
  } else if (encoding == "latex") {
    prefixes <- "prefixes.latex"
  } else {
    prefixes <- "prefixes"
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
                            encoding = "utf8") {
  encoding <- tolower(encoding)
  if (encoding == "utf8") {
    prefixes <- "prefixes.utf8"
  } else if (encoding == "LaTeX") {
    prefixes <- "prefixes.latex"
  } else {
    prefixes <- "prefixes"
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
