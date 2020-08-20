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
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Tfr.type character, either "total" or "internal".
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' A_label(Tfr.type = "internal")
#' A_label(Tfr.type = "total")
#'
A_label <- function(unit.exponent = 0,
                    format = getOption("photobiology.math",
                                       default = "R.expression"),
                    scaled = FALSE,
                    normalized = FALSE,
                    Tfr.type) {
  label.txt <- switch(tolower(Tfr.type),
                      internal = "Internal spectral absorbance",
                      total = "Total spectral absorbance",
                      stop("Bad Tfr.type: ", Tfr.type)
  )

  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $A_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~A[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      paste(label.txt, " A(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $A_{\\lambda}/A_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~A[lambda]/A[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste(label.txt, " A(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.txt, " $A_{\\lambda}$ (",
              exponent2prefix(unit.exponent),
              "AU)", sep = "")
      } else {
        paste(label.txt, "  $A_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}$ AU)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(.(label.txt)~~A[lambda]~~(plain(.(prefix)*AU)))
      } else {
        bquote(.(label.txt)~~A[lambda]~~(10^{.(unit.exponent)}*plain(AU)))
      }
    } else if (format == "R.character" && has_SI_prefix(unit.exponent)) {
      paste(label.txt, " A(lambda) (",
            exponent2prefix(unit.exponent),
            "AU)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' @rdname A_label
#'
#' @export
#'
#' @examples
#'
#' A_internal_label()
#' A_internal_label(-3)
#' A_internal_label(format = "R.expression")
#' A_internal_label(format = "LaTeX")
#' A_internal_label(-3, format = "LaTeX")
#'
A_internal_label <- function(unit.exponent = 0,
                             format = getOption("photobiology.math",
                                                default = "R.expression"),
                             scaled = FALSE,
                             normalized = FALSE) {
  A_label(unit.exponent = unit.exponent,
          format = format,
          scaled = scaled,
          normalized = normalized,
          Tfr.type = "internal")
}

#' @rdname A_label
#'
#' @export
#'
#' @examples
#'
#' A_total_label()
#' A_total_label(-3)
#' A_total_label(format = "R.expression")
#' A_total_label(format = "LaTeX")
#' A_total_label(-3, format = "LaTeX")
#'
A_total_label <- function(unit.exponent = 0,
                          format = getOption("photobiology.math",
                                             default = "R.expression"),
                          scaled = FALSE,
                          normalized = FALSE) {
  A_label(unit.exponent = unit.exponent,
          format = format,
          scaled = scaled,
          normalized = normalized,
          Tfr.type = "total")
}

#' Absorbance y-scale
#'
#' Scale y continuous with defaults suitable for spectral absorbance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Tfr.type character, either "total" or "internal".
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") +
#'   geom_line() +
#'   scale_y_A_continuous(Tfr.type = getTfrType(yellow_gel.spct)) +
#'   scale_x_wl_continuous()
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
scale_y_A_continuous <-
  function(unit.exponent = 0,
           name = A_label(unit.exponent = unit.exponent,
                          format = format,
                          scaled = scaled,
                          normalized = round(normalized, 1),
                          Tfr.type = Tfr.type),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           Tfr.type,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_A_continuous
#'
#' @export
#'
scale_y_A_internal_continuous <-
  function(unit.exponent = 0,
           name = A_label(unit.exponent = unit.exponent,
                          format = format,
                          scaled = scaled,
                          normalized = round(normalized, 1),
                          Tfr.type = "internal"),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_A_continuous
#'
#' @export
#'
scale_y_A_total_continuous <-
  function(unit.exponent = 0,
           name = A_label(unit.exponent = unit.exponent,
                          format = format,
                          scaled = scaled,
                          normalized = round(normalized, 1),
                          Tfr.type = "total"),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
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
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Tfr.type character, either "total" or "internal".
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' Tfr_label(Tfr.type = "internal")
#' Tfr_label(Tfr.type = "total")
#'
Tfr_label <- function(unit.exponent = 0,
                      format = getOption("photobiology.math",
                                         default = "R.expression"),
                      scaled = FALSE,
                      normalized = FALSE,
                      Tfr.type) {
  label.txt <- switch(tolower(Tfr.type),
                      internal = "Internal spectral transmittance",
                      total = "Total spectral transmittance",
                      stop("Bad Tfr.type: ", Tfr.type)
  )
  if (unit.exponent == 0) {
    unit.text = "(/1)"
    unit.tex = "(/1)"
  } else if (unit.exponent == -2) {
    unit.text = "(%)"
    unit.tex = "(\\%)"
  } else if (unit.exponent == -3) {
    unit.text = "(permil)"
    unit.tex = "(permil)"
  } else {
    unit.exponent <- 0
    unit.text = "(/1)"
    unit.tex = "(/1)"
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $t_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~t[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      paste(label.txt, " t(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $t_{\\lambda}/t_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~t[lambda]/t[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste(label.txt, " t(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      paste(label.txt, " $t_{\\lambda}$ ", unit.tex, sep = "")
    } else if (format %in% c("R.expression")) {
        bquote(.(label.txt)~~t[lambda]~~plain(.(unit.text)))
    } else if (format == "R.character") {
      paste(label.txt, " t(lambda) ", unit.text, sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' @rdname Tfr_label
#'
#' @export
#'
#' @examples
#'
#' Tfr_internal_label()
#' Tfr_internal_label(-2)
#' Tfr_internal_label(-3)
#' Tfr_internal_label(format = "R.expression")
#' Tfr_internal_label(format = "LaTeX")
#' Tfr_internal_label(-3, format = "LaTeX")
#'
Tfr_internal_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression"),
                               scaled = FALSE,
                               normalized = FALSE) {
  Tfr_label(unit.exponent = unit.exponent,
            format = format,
            scaled = scaled,
            normalized = normalized,
            Tfr.type = "internal")
}

#' @rdname Tfr_label
#'
#' @export
#'
#' @examples
#'
#' Tfr_total_label()
#' Tfr_total_label(-2)
#' Tfr_total_label(-3)
#' Tfr_total_label(format = "R.expression")
#' Tfr_total_label(format = "LaTeX")
#' Tfr_total_label(-3, format = "LaTeX")
#'
Tfr_total_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression"),
                            scaled = FALSE,
                            normalized = FALSE) {
  Tfr_label(unit.exponent = unit.exponent,
            format = format,
            scaled = scaled,
            normalized = normalized,
            Tfr.type = "total")
}

#' Transmittance y-scale
#'
#' Scale y continuous with defaults suitable for spectral transmittance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param limits One of \code{NULL} for default based on data range, a numeric
#'   vector of length two (\code{NA} allowed) or a function that accepts the
#'   data-based limits as argument and returns new limits.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Tfr.type character, either "total" or "internal".
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' Tfr_as_default()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_continuous(Tfr.type = getTfrType(yellow_gel.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_continuous(unit.exponent = -2,
#'                          Tfr.type = getTfrType(yellow_gel.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_continuous(unit.exponent = -3,
#'                          Tfr.type = getTfrType(yellow_gel.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Tfr_total_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_Tfr_continuous <- function(unit.exponent = 0,
                                   name = Tfr_label(unit.exponent = unit.exponent,
                                                    format = format,
                                                    scaled = scaled,
                                                    normalized = round(normalized, 1),
                                                    Tfr.type = Tfr.type),
                                   labels = SI_pl_format(exponent = unit.exponent),
                                   limits = c(0, 1),
                                   format = getOption("photobiology.math",
                                                      default = "R.expression"),
                                   scaled = FALSE,
                                   normalized = FALSE,
                                   Tfr.type,
                                   ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     limits = limits,
                     ...)
}

#' @rdname scale_y_Tfr_continuous
#'
#' @export
#'
scale_y_Tfr_internal_continuous <-
  function(unit.exponent = 0,
           name = Tfr_label(unit.exponent = unit.exponent,
                            format = format,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            Tfr.type = "internal"),
           labels = SI_pl_format(exponent = unit.exponent),
           limits = c(0, 1),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       limits = limits,
                       ...)
  }

#' @rdname scale_y_Tfr_continuous
#'
#' @export
#'
scale_y_Tfr_total_continuous <-
  function(unit.exponent = 0,
           name = Tfr_label(unit.exponent = unit.exponent,
                            format = format,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            Tfr.type = "total"),
           labels = SI_pl_format(exponent = unit.exponent),
           limits = c(0, 1),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       limits = limits,
                       ...)
  }

# Absorptance --------------------------------------------------------------

#' Absorptance axis labels
#'
#' Generate cps axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' Afr_label()
#' Afr_label(-2)
#' Afr_label(-3)
#' Afr_label(format = "R.expression")
#' Afr_label(format = "LaTeX")
#' Afr_label(-2, format = "LaTeX")
#'
Afr_label <- function(unit.exponent = 0,
                      format = getOption("photobiology.math",
                                         default = "R.expression"),
                      scaled = FALSE,
                      normalized = FALSE) {
  label.txt <- "Spectral absorptance"

  if (unit.exponent == 0) {
    unit.text = "(/1)"
    unit.tex = "(/1)"
  } else if (unit.exponent == -2) {
    unit.text = "(%)"
    unit.tex = "(\\%)"
  } else if (unit.exponent == -3) {
    unit.text = "(permil)"
    unit.tex = "(permil)"
  } else {
    unit.exponent <- 0
    unit.text = "(/1)"
    unit.tex = "(/1)"
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $a_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~a[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      paste(label.txt, " a(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $a_{\\lambda}/a_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~a[lambda]/a[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste(label.txt, " a(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      paste(label.txt, " $a_{\\lambda}$ ", unit.tex, sep = "")
    } else if (format %in% c("R.expression")) {
      bquote(.(label.txt)~~a[lambda]~~plain(.(unit.text)))
    } else if (format == "R.character") {
      paste(label.txt, " a(lambda) ", unit.text, sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' Absorptance y-scale
#'
#' Scale y continuous with defaults suitable for spectral absorptance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param limits One of \code{NULL} for default based on data range, a numeric
#'   vector of length two (\code{NA} allowed) or a function that accepts the
#'   data-based limits as argument and returns new limits.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' Afr_as_default()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Afr_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Afr_continuous(unit.exponent = -2) +
#'   scale_x_wl_continuous()
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   scale_y_Afr_continuous(unit.exponent = -3) +
#'   scale_x_wl_continuous()
#'
scale_y_Afr_continuous <- function(unit.exponent = 0,
                                   name = Afr_label(unit.exponent = unit.exponent,
                                                    format = format,
                                                    scaled = scaled,
                                                    normalized = round(normalized, 1)),
                                   labels = SI_pl_format(exponent = unit.exponent),
                                   limits = c(0, 1),
                                   format = getOption("photobiology.math",
                                                      default = "R.expression"),
                                   scaled = FALSE,
                                   normalized = FALSE,
                                   ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     limits = limits,
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
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Rfr.type character, either "total" or "specular".
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' Rfr_label(Rfr.type = "specular")
#' Rfr_label(Rfr.type = "total")
#'
Rfr_label <- function(unit.exponent = 0,
                      format = getOption("photobiology.math",
                                         default = "R.expression"),
                      scaled = FALSE,
                      normalized = FALSE,
                      Rfr.type) {
  label.txt <- switch(tolower(Rfr.type),
                      specular = "Specular spectral reflectance",
                      total = "Total spectral reflectance",
                      stop("Bad Rfr.type: ", Rfr.type)
  )
  if (unit.exponent == 0) {
    unit.text = "(/1)"
    unit.tex = "(/1)"
  } else if (unit.exponent == -2) {
    unit.text = "(%)"
    unit.tex = "(\\%)"
  } else if (unit.exponent == -3) {
    unit.text = "(permil)"
    unit.tex = "(permil)"
  } else {
    unit.exponent <- 0
    unit.text = "(/1)"
    unit.tex = "(/1)"
    warning("Only values supported for 'unit.exponent' are 0, -2, and -3")
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $r_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~t[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      paste(label.txt, " r(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.txt, " $r_{\\lambda}/r_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(.(label.txt)~~r[lambda]/r[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste(label.txt, " r(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      paste(label.txt, " $r_{\\lambda}$ ", unit.tex, sep = "")
    } else if (format %in% c("R.expression")) {
      bquote(.(label.txt)~~r[lambda]~~plain(.(unit.text)))
    } else if (format == "R.character") {
      paste(label.txt, " r(lambda) ", unit.text, sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' @rdname Rfr_label
#'
#' @export
#'
#' @examples
#'
#' Rfr_specular_label()
#' Rfr_specular_label(-2)
#' Rfr_specular_label(-3)
#' Rfr_specular_label(format = "R.expression")
#' Rfr_specular_label(format = "LaTeX")
#' Rfr_specular_label(-3, format = "LaTeX")
#'
Rfr_specular_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression"),
                               scaled = FALSE,
                               normalized = FALSE) {
  Rfr_label(unit.exponent = unit.exponent,
            format = format,
            scaled = scaled,
            normalized = normalized,
            Rfr.type = "specular")
}

#' @rdname Afr_label
#'
#' @export
#'
#' @examples
#'
#' Rfr_total_label()
#' Rfr_total_label(-2)
#' Rfr_total_label(-3)
#' Rfr_total_label(format = "R.expression")
#' Rfr_total_label(format = "LaTeX")
#' Rfr_total_label(-3, format = "LaTeX")
#'
Rfr_total_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression"),
                            scaled = FALSE,
                            normalized = FALSE) {
  Rfr_label(unit.exponent = unit.exponent,
            format = format,
            scaled = scaled,
            normalized = normalized,
            Rfr.type = "total")
}

#' Reflectance y-scale
#'
#' Scale y continuous with defaults suitable for spectral reflectance.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param limits One of \code{NULL} for default based on data range, a numeric
#'   vector of length two (\code{NA} allowed) or a function that accepts the
#'   data-based limits as argument and returns new limits.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param Rfr.type character, either "total" or "spcular".
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(Ler_leaf_rflt.spct) +
#'   geom_line() +
#'   scale_y_Rfr_continuous(Rfr.type = getRfrType(Ler_leaf_rflt.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(Ler_leaf_rflt.spct) +
#'   geom_line() +
#'   scale_y_Rfr_continuous(unit.exponent = -2,
#'                          Rfr.type = getRfrType(Ler_leaf_rflt.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(Ler_leaf_rflt.spct) +
#'   geom_line() +
#'   scale_y_Rfr_continuous(unit.exponent = -3,
#'                          Rfr.type = getRfrType(Ler_leaf_rflt.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(Ler_leaf_rflt.spct) +
#'   geom_line() +
#'   scale_y_Rfr_specular_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_Rfr_continuous <-
  function(unit.exponent = 0,
           name = Rfr_label(unit.exponent = unit.exponent,
                            format = format,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            Rfr.type = Rfr.type),
           labels = SI_pl_format(exponent = unit.exponent),
           limits = c(0, 1),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           Rfr.type,
           ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     limits = limits,
                     ...)
}

#' @rdname scale_y_Rfr_continuous
#'
#' @export
#'
scale_y_Rfr_specular_continuous <-
  function(unit.exponent = 0,
           name = Rfr_label(unit.exponent = unit.exponent,
                            format = format,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            Rfr.type = "specular"),
           labels = SI_pl_format(exponent = unit.exponent),
           limits = c(0, 1),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       limits = limits,
                       ...)
  }

#' @rdname scale_y_Rfr_continuous
#'
#' @export
#'
scale_y_Rfr_total_continuous <-
  function(unit.exponent = 0,
           name = Rfr_label(unit.exponent = unit.exponent,
                            format = format,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            Rfr.type = "total"),
           labels = SI_pl_format(exponent = unit.exponent),
           limits = c(0, 1),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       limits = limits,
                       ...)
  }
