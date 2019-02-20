#' Raw-counts axis labels
#'
#' Generate axis labels in SI units,
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
#'
#' counts_label()
#' counts_label("R.expression")
#' counts_label("LaTeX")
#'
counts_label <- function(unit.exponent = 3,
                         format = getOption("photobiology.math",
                                            default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (unit.exponent == 0) {
      "Pixel response $\\mathrm{N}(\\lambda)$ (counts)"
    } else {
      paste("Pixel response $\\mathrm{N}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}$ counts)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (unit.exponent == 0) {
      expression(Pixel~~response~~N(lambda)~~(counts))
    } else {
      bquote(plain(Pixel~~response~~rate)~~N(lambda)~~(10^{.(unit.exponent)}*plain(counts)))
    }
  } else if (format == "R.character" && unit.exponent == 0) {
    "Pixel response N(lambda) (counts)"
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Raw-counts y-scale
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
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_continuous(0) +
#'   scale_x_wl_continuous()
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_tg_continuous() +
#'   scale_x_wl_continuous()
#'
scale_y_counts_continuous <- function(unit.exponent = 3,
                                      name = counts_label(unit.exponent = unit.exponent),
                                      labels = SI_pl_format(exponent = unit.exponent),
                                      ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}

#' @rdname scale_y_counts_continuous
#'
#' @export
#'
scale_y_counts_tg_continuous <- function(unit.exponent = 3,
                                         name = counts_label(unit.exponent = 0),
                                         labels = SI_tg_format(exponent = unit.exponent),
                                         ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}
