#' Absorbance axis labels
#'
#' Generate cps axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#' cps_label()
#' cps_label(3)
#' cps_label(format = "R.expression")
#' cps_label(format = "LaTeX")
#' cps_label(3, format = "LaTeX")
#'
A_internal_label <- function(unit.exponent = 0,
                             format = getOption("photobiology.math",
                                                default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Internal spectral absorbance $\\mathrm{A}_\\mathrm{int}(\\lambda)$ (",
            exponent2prefix(unit.exponent),
            "AU)", sep = "")
    } else {
      paste("Internal spectral absorbance $\\mathrm{A}_\\mathrm{int}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}$ counts~s$^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Internal~~spectral~~absorbance)~~A[int](lambda)~~(.(prefix)*plain(AU)))
    } else {
      bquote(plain(Internal~~spectral~~absorbance)~~A[int](lambda)~~(10^{.(unit.exponent)}*plain(AU)))
    }
  } else if (format == "R.character" && has_SI_prefix(unit.exponent)) {
    paste("Internal spectral absorbance A[int](lambda) (",
          exponent2prefix(unit.exponent),
          "AU)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' @rdname A_internal_label
#'
#' @export
#'
A_total_label <- function(unit.exponent = 0,
                          format = getOption("photobiology.math",
                                             default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Total spectral absorbance $\\mathrm{A}_\\mathrm{tot}(\\lambda)$ (",
            exponent2prefix(unit.exponent),
            "AU)", sep = "")
    } else {
      paste("Total spectral absorbance $\\mathrm{A}_\\mathrm{tot}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}$ counts~s$^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Total~~spectral~~absorbance)~~A[tot](lambda)~~(.(prefix)*plain(AU)))
    } else {
      bquote(plain(Total~~spectral~~absorbance)~~A[tot](lambda)~~(10^{.(unit.exponent)}*plain(AU)))
    }
  } else if (format == "R.character" && has_SI_prefix(unit.exponent)) {
    paste("Total spectral absorbance A[tot](lambda) (",
          exponent2prefix(unit.exponent),
          "AU)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Absorbance y-scale
#'
#' Scale y continuous with defaults suitable for raw detector counts.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_y_continuous}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") +
#'   geom_line() +
#'   scale_y_A_internal_continuous()
#'
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") +
#'   geom_line() +
#'   scale_y_A_total_continuous()
#'
scale_y_A_internal_continuous <- function(unit.exponent = 0,
                                          name = A_internal_label(unit.exponent = unit.exponent),
                                          labels = SI_pl_format(exponent = unit.exponent),
                                          ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

#' @rdname scale_y_A_internal_continuous
#'
#' @export
#'
scale_y_A_total_continuous <- function(unit.exponent = 0,
                                       name = A_total_label(unit.exponent = unit.exponent),
                                       labels = SI_pl_format(exponent = unit.exponent),
                                       ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}
