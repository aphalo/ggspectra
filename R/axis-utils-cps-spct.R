#' Counts-per-second axis labels
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
cps_label <- function(unit.exponent = 0,
                      format = getOption("photobiology.math",
                                            default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (unit.exponent == 0) {
      "Pixel response rate $\\mathrm{cps}(\\lambda)$ (counts~s$^{-1}$)"
    } else {
      paste("Pixel response rate $\\mathrm{cps}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}$ counts~s$^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (unit.exponent == 0) {
      expression(Pixel~~response~~rate~~plain(cps)(lambda)~~(plain(counts)~s^{-1}))
    } else {
      bquote(plain(Pixel~~response~~rate)~~plain(cps)(lambda)~~(10^{.(unit.exponent)}*plain(counts)~s^{-1}))

    }
  } else if (format == "R.character" && unit.exponent == 0) {
    "Pixel response cps(lambda) (counts/s)"
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Counts-per-second  y-scale
#'
#' Scale y continuous with defaults suitable for raw detector counts.
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
#' ggplot(white_led.cps_spct) +
#'   geom_line() +
#'   scale_y_cps_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(white_led.cps_spct) +
#'   geom_line() +
#'   scale_y_cps_continuous(3) +
#'   scale_x_wl_continuous()
#'
scale_y_cps_continuous <- function(unit.exponent = 0,
                                   name = cps_label(unit.exponent = unit.exponent),
                                   labels = SI_pl_format(exponent = unit.exponent),
                                   ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}
