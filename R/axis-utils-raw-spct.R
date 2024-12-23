#' Raw-counts axis labels
#'
#' Generate axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
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
#' counts_label()
#' counts_label("R.expression")
#' counts_label("LaTeX")
#'
counts_label <- function(unit.exponent = 3,
                         format = getOption("photobiology.math",
                                            default = "R.expression"),
                         label.text = axis_labels()[["counts"]],
                         scaled = FALSE,
                         normalized = FALSE,
                         axis.symbols = getOption("ggspectra.axis.symbols",
                                                 default = TRUE)) {
  if (!axis.symbols) {
    label.text <- gsub(",$", "", label.text)
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$N_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(N)[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      "Pixel response N(lambda) (rel. units)"
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $N_{\\lambda} / N_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(plain(Pixel~~response)~italic(N)[lambda]/italic(N)[.(normalized)]~plain("(/1)"))
      } else {
        bquote(plain(Pixel~~response)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "N(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (unit.exponent == 0) {
        paste(label.text, "$N_{\\lambda}$ (counts)")
      } else {
        paste(label.text, " $N_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}$ counts)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (unit.exponent == 0) {
        if (axis.symbols) {
          bquote(.(label.text)~italic(N)[lambda]~(counts))
        } else {
          bquote(.(label.text)~(counts))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(N)[lambda]~(10^{.(unit.exponent)}*plain(counts)))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}*plain(counts)))
        }
      }
    } else if (format == "R.character" && unit.exponent == 0) {
      paste(label.text, "N(lambda) (counts)")
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
#' @param label.text character Textual portion of the labels.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
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
#' norm_led.raw_spct <- normalize(white_led.raw_spct, norm = "max")
#'
#' ggplot(norm_led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_continuous(unit.exponent = 0, normalized = "max") +
#'   scale_x_wl_continuous()
#'
scale_y_counts_continuous <-
  function(unit.exponent = ifelse(normalized, 0, 3),
           name = counts_label(unit.exponent = unit.exponent,
                               format = format,
                               label.text = label.text,
                               scaled = scaled,
                               normalized = ifelse(is.numeric(normalized),
                                                   round(normalized, 1),
                                                   unique(normalized)),
                               axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text = axis_labels()[["counts"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_counts_continuous
#'
#' @export
#'
scale_y_counts_tg_continuous <-
  function(unit.exponent = ifelse(normalized, 0, 3),
           name = counts_label(unit.exponent = 0,
                               format = format,
                               label.text = label.text,
                               scaled = scaled,
                               normalized = ifelse(is.numeric(normalized),
                                                   round(normalized, 1),
                                                   unique(normalized)),
                               axis.symbols = axis.symbols),
           labels = SI_tg_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text = axis_labels()[["counts"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    ggplot2::scale_y_continuous(name = name,
                                labels = labels,
                                ...)
  }
