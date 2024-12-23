#' Spectral irradiance axis labels
#'
#' Generate axis labels for spectral irradiance in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer.
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
#' str(s.e.irrad_label())
#' str(s.e.irrad_label(axis.symbols = FALSE))
#' str(s.e.irrad_label(format = "R.expression"))
#' str(s.e.irrad_label(format = "LaTeX"))
#'
#'
#' str(s.q.irrad_label())
#' str(s.q.irrad_label(axis.symbols = FALSE))
#' str(s.q.irrad_label(format = "R.expression"))
#' str(s.q.irrad_label(format = "LaTeX"))
#'
s.e.irrad_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression"),
                            label.text = axis_labels()[["s.e.irrad"]],
                            scaled = FALSE,
                            normalized = FALSE,
                            axis.symbols = getOption("ggspectra.axis.symbols",
                                                    default = TRUE)) {
  if (!axis.symbols) {
    label.text <- gsub(",$", "", label.text)
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$E_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(E)[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "E(lambda) (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $E_{\\lambda} / E_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(E)[lambda]/italic(E)[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "E(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $E_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "W m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste(label.text, " $E_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "W m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)[lambda]~(plain(.(prefix))*plain(W~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(W~m^{-2}~nm^{-1})))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)[lambda]~(10^{.(unit.exponent)}~plain(W~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(W~m^{-2}~nm^{-1})))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " E(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "W m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' @rdname s.e.irrad_label
#'
#' @export
#'
s.q.irrad_label <- function(unit.exponent = ifelse(normalized, 0, -6),
                            format = getOption("photobiology.math",
                                               default = "R.expression"),
                            label.text = axis_labels()[["s.q.irrad"]],
                            scaled = FALSE,
                            normalized = FALSE,
                            axis.symbols = getOption("ggspectra.axis.symbols",
                                                    default = TRUE)) {
  if (!axis.symbols) {
    label.text <- gsub(",$", "", label.text)
  }
  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$Q_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(Q)[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "Q(lambda) (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $Q_{\\lambda} / Q_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(Q)[lambda]/italic(Q)[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "Q(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $Q_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste(label.text, " $Q_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(Q)[lambda]~(plain(.(prefix))*mol~s^{-1}~m^{-2}~nm^{-1}))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*mol~s^{-1}~m^{-2}~nm^{-1}))
        }
      } else {
        bquote(.(label.text)~italic(Q)[lambda]~(10^{.(unit.exponent)}*mol~s^{-1}~m^{-2}~nm^{-1}))
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " Q(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "mol s-1 m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' Spectral irradiance y-scale
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
#'   added to the default \code{name}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(label.text = "") +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(label.text = "Irradiancia spectral,") +
#'   scale_x_wl_continuous(label.text = "Longitud de onda,")
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(unit.exponent = -1) +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct, unit.out = "photon") +
#'   geom_line() +
#'   scale_y_s.q.irrad_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(clip_wl(sun.spct, c(295, NA))) +
#'   geom_line() +
#'   scale_y_s.e.irrad_log10() +
#'   scale_x_wl_continuous()
#'
#' ggplot(clip_wl(sun.spct, c(295, NA)),
#'   unit.out = "photon") +
#'   geom_line(na.rm = TRUE) +
#'   scale_y_s.q.irrad_log10() +
#'   scale_x_wl_continuous()
#'
#' photon_as_default()
#' normalized_sun.spct <- normalize(e2q(sun.spct, action = "replace"))
#' ggplot(normalized_sun.spct) +
#'   geom_line(na.rm = TRUE) +
#'   scale_y_s.q.irrad_continuous(normalized =
#'                             normalization(normalized_sun.spct)$norm.wl) +
#'   scale_x_wl_continuous()
#'
#' ggplot(normalized_sun.spct) +
#'   geom_line(na.rm = TRUE) +
#'   scale_y_s.q.irrad_continuous(normalized =
#'                             normalization(normalized_sun.spct)$norm.type) +
#'   scale_x_wl_continuous()
#'
#' unset_radiation_unit_default()
#'
scale_y_s.e.irrad_continuous <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent = unit.exponent,
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
           label.text = axis_labels()[["s.e.irrad"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.q.irrad_continuous <-
  function(unit.exponent = ifelse(normalized, 0, -6),
           name = s.q.irrad_label(unit.exponent = unit.exponent,
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
           label.text = axis_labels()[["s.q.irrad"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.e.irrad_log10 <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent = unit.exponent,
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
           label.text = axis_labels()[["s.e.irrad"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_log10(name = name,
                  labels = labels,
                  ...)
  }

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.q.irrad_log10 <-
  function(unit.exponent = ifelse(normalized, 0, -6),
           name = s.q.irrad_label(unit.exponent = unit.exponent,
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
           label.text = axis_labels()[["s.q.irrad"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_log10(name = name,
                       labels = labels,
                       ...)
  }

