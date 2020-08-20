#' spectral response and action axis labels
#'
#' Generate axis labels for response or action spectra in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expression", "R.character", or
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
s.e.response_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression"),
                               scaled = FALSE,
                               normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral energy response $R_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~energy~~response)~~R[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral energy response (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral energy response $R_{\\lambda} / R_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~energy~~response)~~R[lambda]/R[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral energy response (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral energy response $R_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste("Spectral energy response $R_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~energy~~response)~~R[lambda]~~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~energy~~response)~~R[lambda]~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
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
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.q.response_label <- function(unit.exponent = 0,
                               format = getOption("photobiology.math",
                                                  default = "R.expression"),
                               scaled = FALSE,
                               normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral photon response $R_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~photon~~response)~~R[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral photon response (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral photon response $R_{\\lambda} / R_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~photon~~response)~~R[lambda]/R[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral photon response (norm.", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral photon response $R_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste("Spectral photon response $R_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~photon~~response)~~R[lambda]~~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~photon~~response)~~R[lambda]~~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
      }
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
}

#' @rdname s.e.response_label
#'
#' @export
#'
s.e.action_label <- function(unit.exponent = 0,
                             format = getOption("photobiology.math",
                                                default = "R.expression"),
                             scaled = FALSE,
                             normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral energy action $A_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~energy~~action)~~A[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral energy action (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral energy action $A_{\\lambda} / A_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~energy~~action)~~A[lambda]/A[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral energy action (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral energy action $A_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste("Spectral energy action $A_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "J^{-1} m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~energy~~action)~~A[lambda]~~(plain(.(prefix))*plain(J^{-1}~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~energy~~action)~~A[lambda]~(10^{.(unit.exponent)}~plain(J^{-1}~m^{-2}~nm^{-1})))
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste("Spectral energy action A(lambda) (",
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
s.q.action_label <- function(unit.exponent = 0,
                             format = getOption("photobiology.math",
                                                default = "R.expression"),
                             scaled = FALSE,
                             normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral photon action $A_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~photon~~action)~~A[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral photon action (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral photon action $A_{\\lambda} / A_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~photon~~action)~~A[lambda]/A[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral photon action (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral photon action $A_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste("Spectral photon action $A_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol^{-1}} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~photon~~action)~~A[lambda]~~(plain(.(prefix))*plain(mol^{-1}~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~photon~~action)~~A[lambda]~~(10^{.(unit.exponent)}*plain(mol^{-1}~m^{-2}~nm^{-1})))
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste("Spectral photon action A(lambda) (",
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
#' @param scaled logical If \code{TRUE} relative units are assumed.
#' @param normalized logical (\code{FALSE}) or numeric Normalization wavelength
#'   in manometers (nm).
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' energy_as_default()
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
scale_y_s.e.response_continuous <-
  function(unit.exponent = 0,
           name = s.e.response_label(unit.exponent = unit.exponent,
                                     format = format,
                                     scaled = scaled,
                                     normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = -unit.exponent), # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
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
                                     scaled = scaled,
                                     normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = -unit.exponent),  # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
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
                                   scaled = scaled,
                                   normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = -unit.exponent), # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
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
                                   scaled = scaled,
                                   normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = -unit.exponent),  # per unit
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }
