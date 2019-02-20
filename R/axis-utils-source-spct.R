#' spectral irradiance axis labels
#'
#' Generate axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
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
                                               default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Spectral energy irradiance $\\mathrm{E}(\\lambda)$ ($",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "W m^{-2} nm^{-1})$)", sep = "")
    } else {
      paste("Spectral energy irradiance $\\mathrm{E}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "W m^{-2} nm^{-1})$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Spectral~~energy~~irradiance)~~E(lambda)~~(plain(.(prefix))*plain(W~m^{-2}~nm^{-1})))
    } else {
      bquote(plain(Spectral~~energy~~irradiance)~~E(lambda)~(10^{.(unit.exponent)}~plain(W~m^{-2}~nm^{-1})))
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

#' @rdname s.e.irrad_label
#'
#' @export
#'
s.q.irrad_label <- function(unit.exponent = -6,
                            format = getOption("photobiology.math",
                                               default = "R.expression")) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste("Spectral photon irradiance $\\mathrm{Q}(\\lambda)$ ($",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
    } else {
      paste("Spectral photon irradiance $\\mathrm{Q}(\\lambda)$ ($\\times 10^{",
            unit.exponent,
            "}\\mathrm{mol} s^{-1} m^{-2} nm^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(plain(Spectral~~photon~~irradiance~~Q(lambda)~~(plain(.(prefix))*mol~s^{-1}~m^{-2}~nm^{-1})))
    } else {
      bquote(plain(Spectral~~photon~~irradiance~~Q(lambda)~~(10^{.(unit.exponent)}*mol~s^{-1}~m^{-2}~nm^{-1})))
    }
    #    quote(Spectral~~energy~~irradiance~~E(lambda)~~(W~m^{-2}~nm^{-1}))
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

#' Spectral irradiance y-scale
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
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous() +
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_y_s.e.irrad_continuous(-1) +
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
           name = s.e.irrad_label(unit.exponent),
           labels = SI_pl_format(exponent = unit.exponent),
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
           name = s.q.irrad_label(unit.exponent = unit.exponent),
           labels = SI_pl_format(exponent = unit.exponent),
           ...) {
    scale_y_continuous(name = name,
                       labels = labels,
                       ...)
  }

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.q.irrad_log10 <-
  function(unit.exponent = -6,
           name = s.q.irrad_label(unit.exponent = unit.exponent),
           labels = SI_pl_format(exponent = unit.exponent),
           ...) {
    scale_y_log10(name = name,
                  labels = labels,
                  ...)
  }

#' @rdname scale_y_s.e.irrad_continuous
#'
#' @export
#'
scale_y_s.e.irrad_log10 <-
  function(unit.exponent = 0,
           name = s.e.irrad_label(unit.exponent),
           labels = SI_pl_format(exponent = unit.exponent),
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

