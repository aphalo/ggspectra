#' Calibration multipliers axis labels
#'
#' Calibration multipliers axis labels. Output can be selected
#' as character, expression (R default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' multipliers_label()
#' multipliers_label(3)
#' multipliers_label(format = "R.expression")
#' multipliers_label(format = "R.character")
#' multipliers_label(format = "LaTeX")
#' multipliers_label(3, format = "LaTeX")
#'
multipliers_label <- function(unit.exponent = 0,
                              format = getOption("photobiology.math",
                                                 default = "R.expression"),
                              label.text = axis_labels()[["e.mult"]],
                              scaled = FALSE,
                              normalized = FALSE,
                              axis.symbols = getOption("ggspectra.axis.symbols",
                                                      default = TRUE)) {
  if (!axis.symbols) {
    label.text <- gsub(",$", "", label.text)
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$k_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(k)[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "k(lambda) (rel. units)")
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste(label.text, " $k_{\\lambda} / k_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(k)[lambda]/italic(k)[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "k(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (unit.exponent == 0) {
        paste(label.text, "$k_{\\lambda}$ ($W m^{-2} nm^{-1} \\mathrm{count}^{-1}~s$)")
      } else {
        paste(label.text, " $k_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "} W m^{-2} nm^{-1} \\mathrm{count}^{-1}~s$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (unit.exponent == 0) {
        if (axis.symbols) {
          bquote(.(label.text)~italic(k)[lambda]~(plain(W~m^{-2}~nm^{-1}~counts^{-1}~s)))
        } else {
          bquote(.(label.text)~(plain(W~m^{-2}~nm^{-1}~counts^{-1}~s)))
        }
      } else {
        if (axis.symbols) {

        } else {

        }
        bquote(.(label.text)~italic(k)[lambda]~(10^{.(unit.exponent)}*plain(W~m^{-2}~nm^{-1}~counts^{-1}~s)))
      }
    } else if (format == "R.character" && unit.exponent == 0) {
      paste(label.text, "k(lambda) (W m-2 nm-1 per counts/s)")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }
}

#' Calibration multipliers y-scale
#'
#' Scale y continuous with defaults suitable for raw the calibration
#' multipliers used to convert pixel response rate (counts per second) into
#' energy irradiance units.
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
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#'   documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
scale_y_multipliers_continuous <-
  function(unit.exponent = 0,
           name = multipliers_label(unit.exponent = unit.exponent,
                                    format = format,
                                    label.text = label.text,
                                    scaled = scaled,
                                    normalized = round(normalized, 1),
                                    axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text = axis_labels()[["e.mult"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }
