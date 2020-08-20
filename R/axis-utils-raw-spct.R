#' Raw-counts axis labels
#'
#' Generate axis labels in SI units,
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
#' counts_label()
#' counts_label("R.expression")
#' counts_label("LaTeX")
#'
counts_label <- function(unit.exponent = 3,
                         format = getOption("photobiology.math",
                                            default = "R.expression"),
                         scaled = FALSE,
                         normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Pixel response $N_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Pixel~~response)~~N[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Pixel response N(lambda) (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Pixel response $N_{\\lambda} / N_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Pixel~~response)~~N[lambda]/N[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Pixel response N(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (unit.exponent == 0) {
        "Pixel response $N_{\\lambda}$ (counts)"
      } else {
        paste("Pixel response $N_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}$ counts)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (unit.exponent == 0) {
        expression(Pixel~~response~~N[lambda]~~(counts))
      } else {
        bquote(plain(Pixel~~response~~rate)~~N[lambda]~~(10^{.(unit.exponent)}*plain(counts)))
      }
    } else if (format == "R.character" && unit.exponent == 0) {
      "Pixel response N(lambda) (counts)"
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' Raw-counts y-scale
#'
#' Scale y continuous with defaults suitable for raw detector counts.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters default arguments values for \code{name} and
#'   \code{labels}, please, see documentation for
#'   \code{\link[ggplot2]{scale_continuous}} for other parameters.
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
#'   scale_y_counts_continuous(unit.exponent = 0) +
#'   scale_x_wl_continuous()
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_tg_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(white_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_tg_continuous(unit.exponent = 0) +
#'   scale_x_wl_continuous()
#'
#' norm_led.raw_spct <- normalize(white_led.raw_spct[ , 1:2], norm = "max")
#'
#' ggplot(norm_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_continuous(normalized = getNormalized(norm_led.raw_spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(norm_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_tg_continuous(normalized = getNormalized(norm_led.raw_spct)) +
#'   scale_x_wl_continuous()
#'
scale_y_counts_continuous <- function(unit.exponent = ifelse(normalized, 0, 3),
                                      name = counts_label(unit.exponent = unit.exponent,
                                                          format = format,
                                                          scaled = scaled,
                                                          normalized = round(normalized, 1)),
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

#' @rdname scale_y_counts_continuous
#'
#' @export
#'
scale_y_counts_tg_continuous <- function(unit.exponent = ifelse(normalized, 0, 3),
                                         name = counts_label(unit.exponent = 0,
                                                             format = format,
                                                             scaled = scaled,
                                                             normalized = round(normalized, 1)),
                                         labels = SI_tg_format(exponent = unit.exponent),
                                         format = getOption("photobiology.math",
                                                            default = "R.expression"),
                                         scaled = FALSE,
                                         normalized = FALSE,
                                         ...) {
  scale_y_continuous(name = name,
                     labels = labels,
                     ...)
}
