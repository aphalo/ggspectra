# Absorbance --------------------------------------------------------------

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
#' A_internal_label()
#' A_internal_label(3)
#' A_internal_label(format = "R.expression")
#' A_internal_label(format = "LaTeX")
#' A_internal_label(3, format = "LaTeX")
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
#' @examples
#' A_total_label()
#' A_total_label(3)
#' A_total_label(format = "R.expression")
#' A_total_label(format = "LaTeX")
#' A_total_label(3, format = "LaTeX")
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
#' Scale y continuous with defaults suitable for spectral absorbance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") +
#'   geom_line() +
#'   scale_y_A_internal_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") +
#'   geom_line() +
#'   scale_y_A_total_continuous() +
#'   scale_x_wl_continuous()
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


# Transmittance --------------------------------------------------------------

#' Transmittance axis labels
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
#' Tfr_internal_label()
#' Tfr_internal_label(3)
#' Tfr_internal_label(format = "R.expression")
#' Tfr_internal_label(format = "LaTeX")
#' Tfr_internal_label(3, format = "LaTeX")
#'
Tfr_internal_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression")) {
  if (unit.exponent == 0) {
    unit.text = " (fraction)"
  } else if (unit.exponent == -2) {
    unit.text = " (percent)"
  } else if (unit.exponent == -3) {
    unit.text = " (permil)"
  } else {
    unit.text = ""
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (tolower(format) == "latex") {
    paste("Internal spectral transmittance $\\mathrm{T}_\\mathrm{int}(\\lambda)$",
          unit.text, sep = "")
  } else if (format %in% c("R.expression")) {
    bquote(plain(Internal~~spectral~~transmittance)~~T[int](lambda)~~plain(.(unit.text)))
  } else if (format == "R.character") {
    paste("Internal spectral transmittance T[int](lambda)", unit.text, sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' @rdname Tfr_internal_label
#'
#' @export
#'
#' @examples
#' Tfr_total_label()
#' Tfr_total_label(-2)
#' Tfr_total_label(-3)
#' Tfr_total_label(format = "R.expression")
#' Tfr_total_label(format = "LaTeX")
#' Tfr_total_label(-2, format = "LaTeX")
#' Tfr_total_label(-3, format = "LaTeX")
#'
Tfr_total_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression")) {
  if (unit.exponent == 0) {
    unit.text = " (fraction)"
  } else if (unit.exponent == -2) {
    unit.text = " (percent)"
  } else if (unit.exponent == -3) {
    unit.text = " (permil)"
  } else {
    unit.text = ""
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (tolower(format) == "latex") {
    paste("Total spectral transmittance $\\mathrm{T}_\\mathrm{tot}(\\lambda)$",
         unit.text, sep = "")
  } else if (format %in% c("R.expression")) {
    bquote(plain(Total~~spectral~~transmittance)~~T[tot](lambda)~~plain(.(unit.text)))
  } else if (format == "R.character") {
    paste("Total spectral transmittance T[tot](lambda)", unit.text, sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Transmittance y-scale
#'
#' Scale y continuous with defaults suitable for spectral transmittance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_internal_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_internal_continuous(-2) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_internal_continuous(-3) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_total_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_Tfr_internal_continuous <- function(unit.exponent = 0,
                                            name = Tfr_internal_label(unit.exponent = unit.exponent),
                                            labels = SI_pl_format(exponent = unit.exponent),
                                            ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

#' @rdname scale_y_Tfr_internal_continuous
#'
#' @export
#'
scale_y_Tfr_total_continuous <- function(unit.exponent = 0,
                                         name = Tfr_total_label(unit.exponent = unit.exponent),
                                         labels = SI_pl_format(exponent = unit.exponent),
                                         ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

# Reflectance --------------------------------------------------------------

#' Reflectance axis labels
#'
#' Generate spectral reflectance labels in SI units,
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
#' Rfr_specular_label()
#' Rfr_specular_label(3)
#' Rfr_specular_label(format = "R.expression")
#' Rfr_specular_label(format = "LaTeX")
#' Rfr_specular_label(3, format = "LaTeX")
#'
Rfr_specular_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression")) {
  if (unit.exponent == 0) {
    unit.text = " (fraction)"
  } else if (unit.exponent == -2) {
    unit.text = " (percent)"
  } else if (unit.exponent == -3) {
    unit.text = " (permil)"
  } else {
    unit.text = ""
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (tolower(format) == "latex") {
    paste("Specular spectral reflectance $\\mathrm{T}_\\mathrm{int}(\\lambda)$",
          unit.text, sep = "")
  } else if (format %in% c("R.expression")) {
    bquote(plain(Specular~~spectral~~reflectance)~~T[int](lambda)~~plain(.(unit.text)))
  } else if (format == "R.character") {
    paste("Specular spectral reflectance T[int](lambda)", unit.text, sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' @rdname Rfr_specular_label
#'
#' @export
#'
#' @examples
#' Rfr_total_label()
#' Rfr_total_label(-2)
#' Rfr_total_label(-3)
#' Rfr_total_label(format = "R.expression")
#' Rfr_total_label(format = "LaTeX")
#' Rfr_total_label(-2, format = "LaTeX")
#' Rfr_total_label(-3, format = "LaTeX")
#'
Rfr_total_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression")) {
  if (unit.exponent == 0) {
    unit.text = " (fraction)"
  } else if (unit.exponent == -2) {
    unit.text = " (percent)"
  } else if (unit.exponent == -3) {
    unit.text = " (permil)"
  } else {
    unit.text = ""
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (tolower(format) == "latex") {
    paste("Total spectral reflectance $\\mathrm{R}_\\mathrm{tot}(\\lambda)$",
          unit.text, sep = "")
  } else if (format %in% c("R.expression")) {
    bquote(plain(Total~~spectral~~reflectance)~~R[tot](lambda)~~plain(.(unit.text)))
  } else if (format == "R.character") {
    paste("Total spectral reflectance R[tot](lambda)", unit.text, sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Reflectance y-scale
#'
#' Scale y continuous with defaults suitable for spectral transmittance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Rfr_total_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Rfr_total_continuous(-2) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Rfr_total_continuous(-3) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Rfr_total_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_Rfr_specular_continuous <- function(unit.exponent = 0,
                                            name = Rfr_specular_label(unit.exponent = unit.exponent),
                                            labels = SI_pl_format(exponent = unit.exponent),
                                            ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

#' @rdname scale_y_Rfr_specular_continuous
#'
#' @export
#'
scale_y_Rfr_total_continuous <- function(unit.exponent = 0,
                                         name = Rfr_total_label(unit.exponent = unit.exponent),
                                         labels = SI_pl_format(exponent = unit.exponent),
                                         ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

