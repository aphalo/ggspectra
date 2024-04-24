#' Counts-per-second axis labels
#'
#' Generate pixel response rate axis labels in cps units. Output can be selected
#' as character, expression (R default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param add.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' cps_label()
#' cps_label(3)
#' cps_label(format = "R.expression")
#' cps_label(format = "R.character")
#' cps_label(format = "LaTeX")
#' cps_label(3, format = "LaTeX")
#'
cps_label <- function(unit.exponent = 0,
                      format = getOption("photobiology.math",
                                         default = "R.expression"),
                      label.text = axis_labels()[["cps"]],
                      scaled = FALSE,
                      normalized = FALSE,
                      add.symbols = getOption("ggspectra.add.symbols",
                                              default = TRUE)) {
  if (!add.symbols) {
    label.text <- gsub(",$", "", label.text)
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "rate $n_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (add.symbols) {
        bquote(.(label.text)~italic(n)[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "n(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.text, " rate $n_{\\lambda} / N_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (add.symbols) {
        bquote(.(label.text)~italic(n)[lambda]/italic(n)[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*"normalised,"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "n(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (unit.exponent == 0) {
        paste(label.text, "$n_{\\lambda}$ (counts~s$^{-1}$)")
      } else {
        paste(label.text, " $n_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}$ counts~s$^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (unit.exponent == 0) {
        if (add.symbols) {
          bquote(.(label.text)~italic(n)[lambda]~(plain(counts)~s^{-1}))
        } else {
          bquote(.(label.text)~(plain(counts)~s^{-1}))
        }
      } else {
        if (add.symbols) {
          bquote(.(label.text)~italic(n)[lambda]~(10^{.(unit.exponent)}*plain(counts)~s^{-1}))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}*plain(counts)~s^{-1}))
        }
      }
    } else if (format == "R.character" && unit.exponent == 0) {
      paste(label.text, "n(lambda) (counts/s)")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' Counts-per-second  y-scale
#'
#' Scale y continuous with defaults suitable for raw detector counts.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param labels The tick labels or a function to generate them.
#' @param format character string, "R", "R.expression", "R.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param add.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#'   documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
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
#' ggplot(white_led.cps_spct * 1e-4) +
#'   geom_line() +
#'   scale_y_cps_continuous(scaled = TRUE) +
#'   scale_x_wl_continuous()
#'
#' norm_led.cps_spct <- normalize(white_led.cps_spct, norm = "max")
#'
#' ggplot(norm_led.cps_spct) +
#'   geom_line() +
#'   scale_y_cps_continuous(normalized = getNormalized(norm_led.cps_spct)) +
#'   scale_x_wl_continuous()
#'
scale_y_cps_continuous <-
  function(unit.exponent = 0,
           name = cps_label(unit.exponent = unit.exponent,
                            format = format,
                            label.text = label.text,
                            scaled = scaled,
                            normalized = round(normalized, 1),
                            add.symbols = add.symbols),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text = axis_labels()[["cps"]],
           scaled = FALSE,
           normalized = FALSE,
           add.symbols = getOption("ggspectra.add.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

