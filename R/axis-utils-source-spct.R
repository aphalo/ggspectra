#' Spectral irradiance axis labels
#'
#' Generate axis labels for spectral irradiance, fluence or exposure in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer. The default is guessed from \code{time.unit},
#'   \code{scaled} and \code{normalized}.
#' @param markup.format character string, "R", "R.expresion", "r.character", or
#'   "LaTeX".
#' @param time.unit character or duration The length of time used as base of
#'   expression.
#' @param label.text character Textual portion of the labels.
#' @param pc.out logical Flag to enable display of normalised data as
#'   percentages.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized,normalised logical (\code{FALSE}) or numeric Normalization wavelength
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
#' str(s.e.irrad_label(markup.format = "R.expression"))
#' str(s.e.irrad_label(markup.format = "LaTeX"))
#' str(s.e.irrad_label(markup.format = "R.character"))
#'
#' str(s.q.irrad_label())
#' str(s.q.irrad_label(axis.symbols = FALSE))
#' str(s.q.irrad_label(markup.format = "R.expression"))
#' str(s.q.irrad_label(markup.format = "LaTeX"))
#' str(s.q.irrad_label(markup.format = "R.character"))
#'
s.e.irrad_label <-
  function(unit.exponent = NULL,
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           time.unit = "second",
           label.text = NULL,
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)

    if (is.logical(normalized)) {
      normalization <- "norm."
    } else if (is.numeric(normalized)) {
      normalization <- sprintf("%.1f", unique(normalized))
      normalized <-!is.na(normalized)
    } else if (is.character(normalized)) {
      normalization <- unique(normalized)
      normalized <- TRUE
    } else {
      stop("Bad argument for 'normalized' or 'normalised'")
    }

    markup.format <- tolower(markup.format)
    stopifnot("Bad argument for 'markup.format'" =
                markup.format %in% c("latex", "r.expression", "r.character"))

    if (pc.out && !normalized) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }

    if (scaled) {
      label.qty <- "s.e.exposure"
      if (is.null(label.text)) {
        label.text <-
          axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
      }
      if (is.null(unit.exponent)) {
        unit.exponent <- 0
      }
      if (markup.format == "latex") {
        paste(label.text, "$E_{\\lambda}$ (rel.\ units)")
      } else if (markup.format == "r.expression") {
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)[lambda]~plain((rel.~units)))
        } else {
          bquote(.(label.text)~plain((rel.~units)))
        }
      } else if (markup.format == "r.character") {
        paste(label.text, "E(lambda) (rel. units)")
      }
    } else if (normalized) {
      label.qty <- "s.e.exposure"
      if (is.null(label.text)) {
        label.text <-
          axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
      }
      if (is.null(unit.exponent)) {
        unit.exponent <- 0
      }
      if (markup.format == "latex") {
        if (pc.out) {
          unit.label <- "(\\%)"
        } else {
          unit.label <- "(/1)"
        }
        paste(label.text, " $E_{\\lambda} / E_{", normalization, "}$ ", unit.label, sep = "")
      } else if (markup.format == "r.expression") {
        if (pc.out) {
          unit.label <- "(%)"
        } else {
          unit.label <- "(/1)"
        }
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)[lambda]/italic(E)[.(normalization)]~plain(.(unit.label)))
        } else {
          bquote(.(label.text)*", normalised"~plain(.(unit.label)))
        }
      } else if (markup.format == "r.character") {
        if (pc.out) {
          unit.label <- "(%)"
        } else {
          unit.label <- "(/1)"
        }
        paste(label.text, " E(lambda) (norm. at", normalization, ") ", unit.label, sep = "")
      }
    } else {
      if (time.unit.char == "second") {
        label.qty <- "s.e.irrad"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        if (is.null(unit.exponent)) {
          unit.exponent <- 0
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $E_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "W m^{-2} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $E_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "W m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
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
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " E(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "W m-2 nm-1)", sep = "")
        }
      } else if (time.unit.char %in% c("hour", "day", "year")) {
        label.qty <- "s.e.exposure"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        time.symbol <- c(hour = "h", day = "d", year = "a")[time.unit.char]
        if (is.null(unit.exponent)) {
          unit.exponent <- c(hour = 3, d = 6, year = 9)[label.qty]
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $E_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "J", time.symbol, "^{-1} m^{-2} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $E_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "} J ", time.symbol, "^{-1} m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
          if (has_SI_prefix(unit.exponent)) {
            prefix <- exponent2prefix(unit.exponent)
            if (axis.symbols) {
              bquote(.(label.text)~italic(E)[lambda]~(plain(.(prefix))*plain(J~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(plain(.(prefix))*plain(J~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            }
          } else {
            if (axis.symbols) {
              bquote(.(label.text)~italic(E)[lambda]~(10^{.(unit.exponent)}~plain(J~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(10^{.(unit.exponent)}~plain(J~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            }
          }
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " E(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "J ", time.symbol, "^{-1} m-2 nm-1)", sep = "")
        }
      } else if (time.unit.char %in% c("duration", "exposure")) {
        label.qty <- "s.e.fluence"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        time.symbol <- c(hour = "h", day = "d", year = "a")[time.unit.char]
        if (is.null(unit.exponent)) {
          unit.exponent <- 0
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $E_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "J m^{-2} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $E_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "} J m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
          if (has_SI_prefix(unit.exponent)) {
            prefix <- exponent2prefix(unit.exponent)
            if (axis.symbols) {
              bquote(.(label.text)~italic(E)[lambda]~(plain(.(prefix))*plain(J~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(plain(.(prefix))*plain(J~m^{-2}~nm^{-1})))
            }
          } else {
            if (axis.symbols) {
              bquote(.(label.text)~italic(E)[lambda]~(10^{.(unit.exponent)}~plain(J~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(10^{.(unit.exponent)}~plain(J~m^{-2}~nm^{-1})))
            }
          }
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " E(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "J m-2 nm-1)", sep = "")
        }
      }
    }
  }

#' @rdname s.e.irrad_label
#'
#' @export
#'
s.q.irrad_label <-
  function(unit.exponent = NULL,
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           time.unit = "second",
           label.text = NULL,
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)

    if (is.logical(normalized)) {
      normalization <- "norm."
    } else if (is.numeric(normalized)) {
      normalization <- sprintf("%.1f", unique(normalized))
      normalized <-!is.na(normalized)
    } else if (is.character(normalized)) {
      normalization <- unique(normalized)
      normalized <- TRUE
    } else {
      stop("Bad argument for 'normalized' or 'normalised'")
    }

    markup.format <- tolower(markup.format)
    stopifnot("Bad argument for 'markup.format'" =
                markup.format %in% c("latex", "r.expression", "r.character"))

    if (pc.out && !normalized) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }

    if (scaled) {
      label.qty <- "s.q.exposure"
      if (is.null(label.text)) {
        label.text <-
          axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
      }
      if (is.null(unit.exponent)) {
        unit.exponent <- 0
      }
      if (markup.format == "latex") {
        paste(label.text, "$Q_{\\lambda}$ (rel.\ units)")
      } else if (markup.format == "r.expression") {
        if (axis.symbols) {
          bquote(.(label.text)~italic(Q)[lambda]~plain((rel.~units)))
        } else {
          bquote(.(label.text)~plain((rel.~units)))
        }
      } else if (markup.format == "r.character") {
        paste(label.text, "Q(lambda) (rel. units)")
      }
    } else if (normalized) {
      label.qty <- "s.q.exposure"
      if (is.null(label.text)) {
        label.text <-
          axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
      }
      if (is.null(unit.exponent)) {
        unit.exponent <- 0
      }
      if (markup.format == "latex") {
        if (pc.out) {
          unit.label <- "(\\%)"
        } else {
          unit.label <- "(/1)"
        }
        paste(label.text, " $Q_{\\lambda} / Q_{", normalization, "}$ ", unit.label, sep = "")
      } else if (markup.format == "r.expression") {
        if (pc.out) {
          unit.label <- "(%)"
        } else {
          unit.label <- "(/1)"
        }
        if (axis.symbols) {
          bquote(.(label.text)~italic(Q)[lambda]/italic(Q)[.(normalization)]~plain(.(unit.label)))
        } else {
          bquote(.(label.text)*", normalised"~plain(.(unit.label)))
        }
      } else if (markup.format == "r.character") {
        if (pc.out) {
          unit.label <- "(%)"
        } else {
          unit.label <- "(/1)"
        }
        paste(label.text, "Q(lambda) (norm. at", normalization, ") ", unit.label, sep = "")
      }
    } else {
      if (time.unit.char=="second") {
        label.qty <- "s.q.irrad"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        if (is.null(unit.exponent)) {
          unit.exponent <- -6
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $Q_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "mol m^{-2} s^{-1} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $Q_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "mol s^{-1} m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
          if (has_SI_prefix(unit.exponent)) {
            prefix <- exponent2prefix(unit.exponent)
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(plain(.(prefix))*plain(mol~m^{-2}~s^{-1}~nm^{-1})))
            } else {
              bquote(.(label.text)~(plain(.(prefix))*plain(mol~m^{-2}~s^{-1}~nm^{-1})))
            }
          } else {
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(10^{.(unit.exponent)}~plain(mol~m^{-2}~s^{-1}~nm^{-1})))
            } else {
              bquote(.(label.text)~(10^{.(unit.exponent)}~plain(mol~m^{-2}~s^{-1}~nm^{-1})))
            }
          }
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " Q(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "mol m-2 s-1 nm-1)", sep = "")
        }
      } else if (time.unit.char %in% c("hour", "day", "year")) {
        label.qty <- "s.q.exposure"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        time.symbol <- c(hour = "h", day = "d", year = "a")[time.unit.char]
        if (is.null(unit.exponent)) {
          unit.exponent <- c(hour = -3, d = 0, year = 3)[label.qty]
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $Q_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "mol", time.symbol, "^{-1} m^{-2} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $Q_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "} mol ", time.symbol, "^{-1} m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
          if (has_SI_prefix(unit.exponent)) {
            prefix <- exponent2prefix(unit.exponent)
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(plain(.(prefix))*plain(mol~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(plain(.(prefix))*plain(mol~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            }
          } else {
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(10^{.(unit.exponent)}~plain(mol~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(10^{.(unit.exponent)}~plain(mol~.(time.symbol)^{-1}~m^{-2}~nm^{-1})))
            }
          }
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " Q(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "mol ", time.symbol, "^{-1} m-2 nm-1)", sep = "")
        }
      } else if (time.unit.char %in% c("duration", "exposure")) {
        label.qty <- "s.e.fluence"
        if (is.null(label.text)) {
          label.text <-
            axis_labels(append = ifelse(axis.symbols, ",", ""))[label.qty]
        }
        if (is.null(unit.exponent)) {
          unit.exponent <- 0
        }
        if (markup.format == "latex") {
          if (has_SI_prefix(unit.exponent)) {
            paste(label.text, " $Q_{\\lambda}$ ($",
                  exponent2prefix(unit.exponent, char.set = "LaTeX"),
                  "mol m^{-2} nm^{-1})$)", sep = "")
          } else {
            paste(label.text, " $Q_{\\lambda}$ ($\\times 10^{",
                  unit.exponent,
                  "} mol m^{-2} nm^{-1})$)", sep = "")
          }
        } else if (markup.format == "r.expression") {
          if (has_SI_prefix(unit.exponent)) {
            prefix <- exponent2prefix(unit.exponent)
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(plain(.(prefix))*plain(mol~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(plain(.(prefix))*plain(mol~m^{-2}~nm^{-1})))
            }
          } else {
            if (axis.symbols) {
              bquote(.(label.text)~italic(Q)[lambda]~(10^{.(unit.exponent)}~plain(mol~m^{-2}~nm^{-1})))
            } else {
              bquote(.(label.text)~(10^{.(unit.exponent)}~plain(mol~m^{-2}~nm^{-1})))
            }
          }
        } else if (markup.format == "r.character" &&
                   has_SI_prefix(unit.exponent)) {
          paste(label.text, " Q(lambda) (",
                exponent2prefix(unit.exponent, char.set = "ascii"),
                "mol m-2 nm-1)", sep = "")
        }
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
#' @param markup.format character string, "R", "R.expression", "r.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#' @param pc.out logical, if \code{TRUE} use percent instead of fraction of one
#'   for normalized spectral data.
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized,normalised logical (\code{FALSE}) or numeric Normalization
#'   wavelength in manometers (nm).
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
#' ggplot(sun.spct, unit.out = "energy") +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(unit.exponent = -3) +
#'   scale_x_wl_continuous()
#'
#' ggplot(fscale(sun.spct)) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(scaled = TRUE) +
#'   scale_x_wl_continuous()
#'
#' ggplot(normalize(sun.spct, norm = "max")) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(normalized = "max") +
#'   scale_x_wl_continuous()
#'
#' my.spct <- normalize(q2e(sun.spct, action = "replace"), norm = "max")
#' ggplot(my.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(normalized = normalization(my.spct)$norm.type,
#'                                pc.out = TRUE) +
#'   scale_x_wl_continuous()
#'
#' ggplot(my.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(normalized = normalization(my.spct)$norm.wl,
#'                                pc.out = TRUE) +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(axis.symbols = FALSE) +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(label.text = "") +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(label.text = "Irradiancia espectral,") +
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
scale_y_s.e.irrad_continuous <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent = unit.exponent,
                                  markup.format = markup.format,
                                  time.unit = "second",
                                  label.text = label.text,
                                  pc.out = pc.out,
                                  scaled = scaled,
                                  normalized = normalized,
                                  axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent - pc.out * 2),
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.irrad"]],
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
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
  function(unit.exponent = -6,
           name = s.q.irrad_label(unit.exponent = unit.exponent,
                                  markup.format = markup.format,
                                  time.unit = "second",
                                  label.text = label.text,
                                  pc.out = pc.out,
                                  scaled = scaled,
                                  normalized = normalized,
                                  axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent - pc.out * 2),
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.irrad"]],
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
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
                                  markup.format = markup.format,
                                  time.unit = "second",
                                  label.text = label.text,
                                  pc.out = pc.out,
                                  scaled = scaled,
                                  normalized = normalized,
                                  axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent - pc.out * 2),
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.irrad"]],
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
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
  function(unit.exponent = -6,
           name = s.q.irrad_label(unit.exponent = unit.exponent,
                                  markup.format = markup.format,
                                  time.unit = "second",
                                  label.text = label.text,
                                  pc.out = pc.out,
                                  scaled = scaled,
                                  normalized = normalized,
                                  axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = unit.exponent - pc.out * 2),
           markup.format = getOption("photobiology.math",
                                     default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.irrad"]],
           pc.out = FALSE,
           scaled = FALSE,
           normalised = FALSE,
           normalized = normalised,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_y_log10(name = name,
                  labels = labels,
                  ...)
  }

