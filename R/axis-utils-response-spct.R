#' spectral response axis labels
#'
#' Generate axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#' counts_label()
#' counts_label("R.expression")
#' counts_label("LaTeX")
#'
s.e.response_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Spectral energy response $\\mathrm{R}(\\lambda)$ ($",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "J^{-1} m^{-2} nm^{-1})$)", sep = "")
    } else {
      paste("Spectral energy response $\\mathrm{R}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "J^{-1} m^{-2} nm^{-1})$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Spectral~~energy~~response)~~R(lambda)~~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
    } else {
      bquote(plain(Spectral~~energy~~response)~~R(lambda)~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
    }
  } else if (format == "R.character" &&
        has_SI_prefix(unit.exponent)) {
    paste("Spectral energy response R(lambda) (",
          exponent2prefix(unit.exponent, char.set = "ascii"),
          "J-1 m-2 nm-1)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
  }
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.q.response_label <- function(unit.exponent = -6,
                            format = getOption("photobiology.math",
                                               default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Spectral photon response $\\mathrm{R}(\\lambda)$ ($",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
    } else {
      paste("Spectral photon response $\\mathrm{R}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Spectral~~photon~~response~~R(lambda)~~(plain(.(prefix))*mol^{-1}~m^{-2}~nm^{-1})))
    } else {
      bquote(plain(Spectral~~photon~~response~~R(lambda)~~(10^{.(unit.exponent)}*mol^{-1}~m^{-2}~nm^{-1})))
    }
    #    quote(Spectral~~energy~~irradiance~~E(lambda)~~(W~m^{-2}~nm^{-1}))
  } else if (format == "R.character" &&
             has_SI_prefix(unit.exponent)) {
    paste("Spectral photon response R(lambda) (",
          exponent2prefix(unit.exponent, char.set = "ascii"),
          "mol-1 m-2 nm-1)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
  }
}

#' Spectral response y-scale
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
#' ggplot(ccd.spct) +
#'   geom_line() +
#'   scale_y_s.e.response_continuous(unit.exponent = 6) +
#'   scale_x_wl_continuous()
#'
#' ggplot(ccd.spct, unit.out = "photon") +
#'   geom_line() +
#'   scale_y_s.q.response_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_s.e.response_continuous <-
  function(unit.exponent = 0,
           name = s.e.response_label(unit.exponent),
           labels = SI_pl_format(exponent = -unit.exponent), # per unit
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.response_continuous
#'
#' @export
#'
scale_y_s.q.response_continuous <-
  function(unit.exponent = 0,
           name = s.q.response_label(unit.exponent = unit.exponent),
           labels = SI_pl_format(exponent = -unit.exponent),  # per unit
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }
