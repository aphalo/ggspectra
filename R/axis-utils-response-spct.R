#' spectral response and action axis labels
#'
#' Generate axis labels for response or action spectra in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expression", "R.character", or
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
#' s.e.response_label()
#' s.e.response_label(format = "R.expression")
#' s.e.response_label(format = "R.character")
#' s.e.response_label(format = "LaTeX")
#' s.e.response_label(unit.exponent = 3, format = "R.character")
#' s.q.response_label(format = "R.character")
#' s.e.action_label(format = "R.character")
#' s.q.action_label(format = "R.character")
#' s.e.response_label(scaled = TRUE)
#' s.e.response_label(scaled = TRUE, format = "R.character")
#' s.e.response_label(scaled = TRUE, format = "LaTeX")
#' s.e.response_label(normalized = 300)
#' s.e.response_label(normalized = 300, format = "R.character")
#' s.e.response_label(normalized = 300, format = "LaTeX")
#' s.q.response_label(scaled = TRUE)
#' s.q.response_label(scaled = TRUE, format = "R.character")
#' s.q.response_label(scaled = TRUE, format = "LaTeX")
#' s.q.response_label(normalized = 300)
#' s.q.response_label(normalized = 300, format = "R.character")
#' s.q.response_label(normalized = 300, format = "LaTeX")
#'
s.e.response_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.response"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {

    if (scaled) {
      if (tolower(format) == "latex") {
        paste(label.text, "$R(E)_{\\lambda}$ (rel.\ units)")
      } else if (format == "R.expression") {
        if (axis.symbols) {
          bquote(.(label.text)~italic(R(E))[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "R(lambda) (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $R(E)_{\\lambda} / R(E)_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(R(E))[lambda]/italic(R(E))[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "R(E)(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $R(E)_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste(label.text, " $R(E)_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(R(E))[lambda]~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(R(E))[lambda]~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " R(E)(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "J-1 m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.q.response_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.response"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {

  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, " $R(Q)_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(R(Q))[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, " (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $R(Q)_{\\lambda} / R(Q)_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(R(Q))[lambda]/italic(R(Q))[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "R(Q)(lambda) (norm.", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $R(Q)_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste(label.text, " $R(Q)_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(R(Q))[lambda]~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(R(Q))[lambda]~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " R(Q)(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "mol-1 m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.e.action_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.action"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {

  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$A(E)_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(A(E))[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "A(E)(lambda) (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $A(E)_{\\lambda} / A(E)_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(A(E))[lambda]/italic(A(E))[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "A(E)(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $A(E)_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste(label.text, " $A(E)_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(A(E))[lambda]~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(A(E))[lambda]~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " A(E)(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "J-1 m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.q.action_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.action"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {

  if (scaled) {
    if (tolower(format) == "latex") {
      paste(label.text, "$A(Q)_{\\lambda}$ (rel.\ units)")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~talic(A(Q))[lambda]~plain((rel.~units)))
      } else {
        bquote(.(label.text)~plain((rel.~units)))
      }
    } else if (format == "R.character") {
      paste(label.text, "A(Q)(lambda) (rel. units)")
    }
  } else if (is.character(normalized) || normalized) {
    if (is.logical(normalized)) {
      normalized <- "norm"
    }
    if (tolower(format) == "latex") {
      paste(label.text, " $A(Q)_{\\lambda} / A(Q)_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      if (axis.symbols) {
        bquote(.(label.text)~italic(A(Q))[lambda]/talic(A(Q))[.(normalized)]~plain("(/1)"))
      } else {
        bquote(.(label.text)*", normalised"~plain("(/1)"))
      }
    } else if (format == "R.character") {
      paste(label.text, "A(Q)(lambda) (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $A(Q)_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste(label.text, " $A(Q)_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(A(Q))[lambda]~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(A(Q))[lambda]~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " A(Q)(lambda) (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "mol-1 m-2 nm-1)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
    }
  }
}

#' Spectral response and action y-scales
#'
#' Scale y continuous with defaults suitable for response and action spectra.
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
#' documentation for \code{\link[ggplot2]{scale_continuous}}.
#'
#' @export
#'
#' @examples
#'
#' ggplot(ccd.spct) +
#'   geom_line() +
#'   scale_y_s.e.action_continuous() + #  per joule
#'   scale_x_wl_continuous()
#'
#' ggplot(ccd.spct) +
#'   geom_line() +
#'   scale_y_s.e.response_continuous() + #  per joule
#'   scale_x_wl_continuous()
#'
#' ggplot(ccd.spct) +
#'   geom_line() +
#'   scale_y_s.e.response_continuous(unit.exponent = 6) + # per mega joule
#'   scale_x_wl_continuous()
#'
#' ggplot(ccd.spct, unit.out = "photon") +
#'   geom_line() +
#'   scale_y_s.q.response_continuous() + # per mol
#'   scale_x_wl_continuous()
#'
#' ggplot(ccd.spct, unit.out = "photon") +
#'   geom_line() +
#'   scale_y_s.q.response_continuous(unit.exponent = 3) + # per 1000 moles
#'   scale_x_wl_continuous()
#'
#' norm_ccd.spct <- normalize(ccd.spct, norm = "max")
#' ggplot(norm_ccd.spct) +
#'   geom_line() +
#'   scale_y_s.e.response_continuous(normalized = getNormalized(norm_ccd.spct)) +
#'   scale_x_wl_continuous()
#'
#' if (packageVersion("photobiology") > "0.11.4") {
#'   ggplot(norm_ccd.spct) +
#'     geom_line() +
#'     scale_y_s.e.response_continuous(normalized =
#'       normalization(norm_ccd.spct)$norm.type) +
#'     scale_x_wl_continuous()
#' }
#' photon_as_default()
#'
#' norm_ccd.spct <- normalize(ccd.spct, norm = "max")
#' ggplot(norm_ccd.spct) +
#'   geom_line() +
#'   scale_y_s.q.response_continuous(normalized = getNormalized(norm_ccd.spct)) +
#'   scale_x_wl_continuous()
#'
#' ggplot(norm_ccd.spct) +
#'   geom_line() +
#'   scale_y_s.q.response_continuous(unit.exponent = 2,
#'                                   normalized = getNormalized(norm_ccd.spct)) +
#'   scale_x_wl_continuous()
#'
#' unset_radiation_unit_default()
#'
scale_y_s.e.response_continuous <-
  function(unit.exponent = 0,
           name = s.e.response_label(unit.exponent = unit.exponent,
                                     format = format,
                                     label.text = label.text,
                                     scaled = scaled,
                                     normalized = ifelse(is.numeric(normalized),
                                                         round(normalized, 1),
                                                         unique(normalized)),
                                     axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = -unit.exponent), # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.response"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
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
           name = s.q.response_label(unit.exponent = unit.exponent,
                                     format = format,
                                     label.text = label.text,
                                     scaled = scaled,
                                     normalized = ifelse(is.numeric(normalized),
                                                         round(normalized, 1),
                                                         unique(normalized)),
                                     axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = -unit.exponent),  # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.response"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.response_continuous
#'
#' @export
#'
scale_y_s.e.action_continuous <-
  function(unit.exponent = 0,
           name = s.e.action_label(unit.exponent = unit.exponent,
                                   format = format,
                                   label.text = label.text,
                                   scaled = scaled,
                                   normalized = ifelse(is.numeric(normalized),
                                                       round(normalized, 1),
                                                       unique(normalized)),
                                   axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = -unit.exponent), # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.e.action"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.response_continuous
#'
#' @export
#'
scale_y_s.q.action_continuous <-
  function(unit.exponent = 0,
           name = s.q.action_label(unit.exponent = unit.exponent,
                                   format = format,
                                   label.text = label.text,
                                   scaled = scaled,
                                   normalized = ifelse(is.numeric(normalized),
                                                       round(normalized, 1),
                                                       unique(normalized)),
                                   axis.symbols = axis.symbols),
           labels = SI_pl_format(exponent = -unit.exponent),  # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.q.action"]],
           scaled = FALSE,
           normalized = FALSE,
           axis.symbols = getOption("ggspectra.axis.symbols",
                                   default = TRUE),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }
