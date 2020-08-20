#' spectral irradiance axis labels
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
s.e.irrad_label <- function(unit.exponent = 0,
                            format = getOption("photobiology.math",
                                               default = "R.expression"),
                            scaled = FALSE,
                            normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral energy irradiance $\\mathrm{E}_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~energy~~irradiance)~~E[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral energy irradiance (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral energy irradiance $\\mathrm{E}_{\\lambda} / \\mathrm{E}_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~energy~~irradiance)~~E[lambda]/E[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral energy irradiance (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral energy irradiance $\\mathrm{E}_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "W m^{-2} nm^{-1})$)", sep = "")
      } else {
        paste("Spectral energy irradiance $\\mathrm{E}_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "W m^{-2} nm^{-1})$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~energy~~irradiance)~~E[lambda]~~(plain(.(prefix))*plain(W~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~energy~~irradiance)~~E[lambda]~(10^{.(unit.exponent)}~plain(W~m^{-2}~nm^{-1})))
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste("Spectral energy irradiance E(lambda) (",
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
                            scaled = FALSE,
                            normalized = FALSE) {
  if (scaled) {
    if (tolower(format) == "latex") {
      "Spectral photon irradiance $\\mathrm{Q}_{\\lambda}$ (rel.\ units)"
    } else if (format == "R.expression") {
      expression(plain(Spectral~~photon~~irradiance)~~Q[lambda]~~plain((rel.~~units)))
    } else if (format == "R.character") {
      "Spectral photon irradiance (rel. units)"
    }
  } else if (normalized) {
    if (tolower(format) == "latex") {
      paste("Spectral photon irradiance $\\mathrm{Q}_{\\lambda} / \\mathrm{R}_{", normalized, "}$ (/1)", sep = "")
    } else if (format == "R.expression") {
      bquote(plain(Spectral~~photon~~irradiance)~~Q[lambda]/Q[.(normalized)]~~plain("(/1)"))
    } else if (format == "R.character") {
      paste("Spectral photon irradiance (norm. at", normalized, "nm)")
    }
  } else {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste("Spectral photon irradiance $\\mathrm{Q}_{\\lambda}$ ($",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
      } else {
        paste("Spectral photon irradiance $\\mathrm{Q}_{\\lambda}$ ($\\times 10^{",
              unit.exponent,
              "}\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        bquote(plain(Spectral~~photon~~irradiance~~Q[lambda]~~(plain(.(prefix))*mol~s^{-1}~m^{-2}~nm^{-1})))
      } else {
        bquote(plain(Spectral~~photon~~irradiance~~Q[lambda]~~(10^{.(unit.exponent)}*mol~s^{-1}~m^{-2}~nm^{-1})))
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste("Spectral photon irradiance Q(lambda) (",
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
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous() +
#'   scale_x_wl_continuous()
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
#' normalized_sun.spct <- normalize(sun.spct)
#' ggplot(normalized_sun.spct) +
#'   geom_line(na.rm = TRUE) +
#'   scale_y_s.q.irrad_continuous(normalized =
#'                             getNormalized(normalized_sun.spct)) +
#'   scale_x_wl_continuous()
#'
scale_y_s.e.irrad_continuous <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent = unit.exponent,
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

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.q.irrad_continuous <-
  function(unit.exponent = ifelse(normalized, 0, -6),
           name = s.q.irrad_label(unit.exponent = unit.exponent,
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

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.e.irrad_log10 <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent = unit.exponent,
                                  format = format,
                                  scaled = scaled,
                                  normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
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
                                  scaled = scaled,
                                  normalized = round(normalized, 1)),
           labels = SI_pl_format(exponent = unit.exponent),
           format = getOption("photobiology.math",
                              default = "R.expression"),
           scaled = FALSE,
           normalized = FALSE,
           ...) {
    scale_y_log10(name = name,
                       labels = labels,
                       ...)
  }


## internal

#' Convert lubridate duration objects to a string if possible
#'
#' @param time.unit lubridate::duration object or character
#'
#' @keywords internal
#'
duration2character <- function(time.unit) {
  if (is.character(time.unit)) return(time.unit)
  if (!lubridate::is.duration(time.unit)) return("unknown")
  if (time.unit == lubridate::duration(1, "seconds")) return("second")
  if (time.unit == lubridate::duration(1, "hours")) return("hour")
  if (time.unit == lubridate::duration(1, "days")) return("day")
  "duration"
}

