#' Wavelength conversions
#'
#' Convert wavelength into wavenumber or into frequency.
#'
#' @param w.length numeric wavelength (nm)
#' @param unit.exponent integer
#'
#' @export
#'
#' @examples
#'
#' w_number(600)
#' w_frequency(600)
#'
w_number <- function(w.length,
                     unit.exponent = 0) {
  1e9 / w.length * 10^unit.exponent # 1/m
}

#' @rdname w_number
#'
#' @export
#'
w_frequency <- function(w.length,
                        unit.exponent = 0) {
  299792458 / (w.length * 1e-9) / 10^unit.exponent # speed of light [m/s] / w.length [m]
}

#' Wave- axis labels
#'
#' Generate wavelength, wavenumber and wave frequency axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param unit.exponent integer
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' w_length_label()
#' w_length_label("R.expression")
#' w_length_label("LaTeX")
#' w_number_label()
#' w_number_label("R.expression")
#' w_frequency_label()
#' w_frequency_label("R.expression")
#'
w_length_label <- function(unit.exponent = -9,
                           format = getOption("photobiology.math",
                                              default = "R.expression"),
                           label.text = axis_labels()[["w.length"]]) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste(label.text, " $\\lambda$ (",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "m)", sep = "")
    } else {
      paste(label.text, " $\\lambda$ ($\\times 10^{",
            unit.exponent,
            "}$~m)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(.(label.text)~lambda~(plain(.(prefix))*plain(m)))
    } else {
      bquote(.(label.text)~lambda~(10^{.(unit.exponent)}~plain(m)))
    }
  } else if (format == "R.character" &&
             has_SI_prefix(unit.exponent)) {
    paste(label.text, " lambda (",
          exponent2prefix(unit.exponent, char.set = "ascii"),
          "m)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
  }
}

#' @rdname w_length_label
#'
#' @export
#'
w_number_label <- function(unit.exponent = 0,
                           format = getOption("photobiology.math",
                                              default = "R.expression"),
                           label.text = axis_labels()[["w.number"]]) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste(label.text, " $\\nu$ (",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "m$^{-1}$)", sep = "")
    } else {
      paste(label.text, " $\\nu$ ($\\times 10^{",
            unit.exponent,
            "m$^{-1}$)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(.(label.text)~nu~(plain(.(prefix))*plain(m)^{-1}))
    } else {
      bquote(.(label.text)~nu~(10^{.(unit.exponent)}~plain(m)^{-1}))
    }
  } else if (format == "R.character" &&
             has_SI_prefix(unit.exponent)) {
    paste(label.text, " v (1/",
          exponent2prefix(unit.exponent, char.set = "ascii"),
          "m)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' @rdname w_length_label
#'
#' @export
#'
w_frequency_label <- function(unit.exponent = 9,
                              format = getOption("photobiology.math",
                                                 default = "R.expression"),
                              label.text = axis_labels()[["freq"]]) {
  if (tolower(format) == "latex") {
    if (has_SI_prefix(unit.exponent)) {
      paste(label.text, " $f$ (",
            exponent2prefix(unit.exponent, char.set = "LaTeX"),
            "Hz)", sep = "")
    } else {
      paste(label.text, " $f$ ($\\times 10^{",
            unit.exponent,
            "}$~Hz)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (has_SI_prefix(unit.exponent)) {
      prefix <- exponent2prefix(unit.exponent)
      bquote(.(label.text)~italic(f)~(plain(.(prefix))*plain(Hz)))
    } else {
      bquote(.(label.text)~italic(f)~(10^{.(unit.exponent)}~plain(Hz)))
    }
  } else if (format == "R.character" &&
             has_SI_prefix(unit.exponent)) {
    paste(label.text, " f (",
          exponent2prefix(unit.exponent, char.set = "ascii"),
          "Hz)", sep = "")
  } else {
    warning("'format = ", format,
            "' not implemented for unit.exponent = ", unit.exponent)
    NA_character_
  }
}

#' Secondary axes for wavelengths
#'
#' Secondary axes for wavelength data in nanometres. With suitable scaling and
#' name (axis label) for frequency and wavenumber.
#'
#' @param unit.exponent integer
#' @param label.text character Textual portion of the labels.
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_number())
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_number(-4))
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_number(nearest_SI_exponent(-4)))
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_number(-3))
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_frequency())
#'
sec_axis_w_number <-
  function(unit.exponent = -6,
           label.text = axis_labels()[["w.number"]]) {
    ggplot2::sec_axis(trans = ~w_number(., unit.exponent),
                      name = w_number_label(unit.exponent = unit.exponent,
                                            label.text = label.text),
                      breaks = scales::pretty_breaks(n = 7))
  }

#' @rdname sec_axis_w_number
#'
#' @export
#'
sec_axis_w_frequency <-
  function(unit.exponent = 12,
           label.text = axis_labels()[["freq"]]) {
    ggplot2::sec_axis(trans = ~w_frequency(., unit.exponent),
                      name = w_frequency_label(unit.exponent = unit.exponent,
                                               label.text = label.text),
                      breaks = scales::pretty_breaks(n = 7)
    )
  }

#' Wavelength x-scale
#'
#' Scale x continuous with defaults suitable for wavelengths in nanometres.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions.
#' @param label.text character Textual portion of the labels.
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
#'   scale_x_wl_continuous()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(-6)
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_w_frequency())
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_w_number())
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(unit.exponent = -6,
#'                         sec.axis = sec_axis_w_number())
#'
scale_x_wl_continuous <-
  function(unit.exponent = -9,
           name = w_length_label(unit.exponent = unit.exponent,
                                 label.text = label.text),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = unit.exponent + 9),
           label.text = axis_labels()[["w.length"]],
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }
