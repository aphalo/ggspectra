#' Deprecated functions
#'
#' To convert wavelength into wavenumber or into frequency, please, use the
#' conversion functions from package 'photobiology' in place of the deprecated
#' functions \code{w_number()} and \code{w_frequency()} from this package.
#'
#' @section Deprecated: These functions will be removed from package 'ggpmisc'
#'   in the near future.
#'
#' @param w.length numeric wavelength (nm)
#' @param unit.exponent integer Exponent of the scale multiplier implicit in
#'   result, e.g., use 3 for kJ.
#'
#' @seealso See \code{\link[photobiology]{wl2wavenumber}} for the functions to
#'   be used in all new code.
#'
#' @export
#'
#' @examples
#' library(photobiology)
#'
#' wl2wavenumber(600)
#' wl2frequency(600)
#'
w_number <- photobiology::wl2wavenumber

#' @rdname w_number
#'
#' @export
#'
w_frequency <- photobiology::wl2frequency

#' Wave- axis labels
#'
#' Generate wavelength, wavenumber, wave frequency, and energy per photon axis
#' labels in SI units, using SI scale factors. Output can be selected as
#' character, expression (R default devices) or LaTeX (for tikz device).
#'
#' @details By default labels consist in a textual name for the quantity, a
#'   symbol separated by a comma and units with scale factor in parenthesis. The
#'   textual names are by default in English but this default can be overridden
#'   for example with translations to a different language. To change or
#'   translate the default texts please see \code{\link{axis_labels_uk}}. The
#'   markup language used for the labels can be selected through a parameter
#'   argument, with character strings ready to be parsed into R expressions
#'   as default.
#'
#'   Wavelengths are assumed to be expressed in nanometres in the data. The
#'   \code{unit.exponent} corresponds to that desired for the tick labels
#'   with the corresponding axis label automatically set to an SI scale
#'   factor if possible, and otherwise shown as a power of 10.
#'
#'   These functions are used internally by \emph{x} scales; see
#'   \code{\link{sec_axis_w_number}} and \code{\link{scale_x_wl_continuous}}.
#'   The scales and secondary axis functions should be used except when
#'   defining new scale functions.
#'
#' @param unit.exponent integer The exponent in base 10 of the scale multiplier
#' to use.
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#'
#' w_length_label()
#' w_length_label(axis.symbols = FALSE)
#' w_length_label(format = "R.expression")
#' w_length_label(format = "LaTeX")
#' w_number_label()
#' w_number_label(format = "R.expression")
#' w_frequency_label()
#' w_frequency_label(format = "R.expression")
#' w_energy_J_label()
#' w_energy_eV_label()
#'
w_length_label <-
  function(unit.exponent = -9,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.length"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
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
        if (axis.symbols) {
          bquote(.(label.text)~lambda~(plain(.(prefix))*plain(m)))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(m)))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~lambda~(10^{.(unit.exponent)}~plain(m)))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(m)))
        }
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
w_number_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.number"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
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
        if (axis.symbols) {
          bquote(.(label.text)~nu~(plain(.(prefix))*plain(m)^{-1}))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(m)^{-1}))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~nu~(10^{.(unit.exponent)}~plain(m)^{-1}))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(m)^{-1}))
        }
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
w_frequency_label <-
  function(unit.exponent = 9,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["freq"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
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
        if (axis.symbols) {
          bquote(.(label.text)~italic(f)~(plain(.(prefix))*plain(Hz)))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(Hz)))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(f)~(10^{.(unit.exponent)}~plain(Hz)))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(Hz)))
        }
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

#' @rdname w_length_label
#'
#' @export
#'
w_energy_eV_label <-
  function(unit.exponent = 0,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $E$ (",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "eV)", sep = "")
      } else {
        paste(label.text, " $E$ ($\\times 10^{",
              unit.exponent,
              "}$~eV)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)~(plain(.(prefix))*plain(eV)))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(eV)))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)~(10^{.(unit.exponent)}~plain(eV)))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(eV)))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " E (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "eV)", sep = "")
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
w_energy_J_label <-
  function(unit.exponent = -18,
           format = getOption("photobiology.math",
                              default = "R.expression"),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    if (tolower(format) == "latex") {
      if (has_SI_prefix(unit.exponent)) {
        paste(label.text, " $E$ (",
              exponent2prefix(unit.exponent, char.set = "LaTeX"),
              "J)", sep = "")
      } else {
        paste(label.text, " $E$ ($\\times 10^{",
              unit.exponent,
              "}$~J)", sep = "")
      }
    } else if (format %in% c("R.expression")) {
      if (has_SI_prefix(unit.exponent)) {
        prefix <- exponent2prefix(unit.exponent)
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)~(plain(.(prefix))*plain(J)))
        } else {
          bquote(.(label.text)~(plain(.(prefix))*plain(J)))
        }
      } else {
        if (axis.symbols) {
          bquote(.(label.text)~italic(E)~(10^{.(unit.exponent)}~plain(J)))
        } else {
          bquote(.(label.text)~(10^{.(unit.exponent)}~plain(J)))
        }
      }
    } else if (format == "R.character" &&
               has_SI_prefix(unit.exponent)) {
      paste(label.text, " E (",
            exponent2prefix(unit.exponent, char.set = "ascii"),
            "J)", sep = "")
    } else {
      warning("'format = ", format,
              "' not implemented for unit.exponent = ", unit.exponent)
      NA_character_
    }
  }

#' Secondary axes for wavelengths
#'
#' Secondary axes for wavelength data in nanometres. With suitable scaling and
#' name (axis label) for frequency, wave number, photon energy and wavelength.
#'
#' @details These secondary axis functions can be used only when the \emph{x}
#'   aesthetic is mapped to a numerical variable containing wavelength values
#'   expressed in nanometres. They can be used to add a secondary x axis to
#'   plots created using \code{ggplot()} or \code{autoplot()}.
#'
#' @seealso the default text used for quantity names are most easily changed
#'   by resetting all the defaults once as explained in
#'   \code{\link{axis_labels_uk}}, even if it is possible to override them
#'   also in each call.
#'
#' @param unit.exponent integer The exponent on base 10 of the scale multiplier
#'   used for the axis labels, e.g., 3 for \eqn{10^3} or \eqn{k}.
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_w_number())
#'
#' # Secondary axes can be added to plots built with autoplot() methods
#' autoplot(sun.spct) +
#'   scale_x_wl_continuous(sec.axis = sec_axis_w_number())
#'
#' # Using 'ggplot2' scale
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_continuous(name = w_length_label(),
#'                      sec.axis = sec_axis_w_number())
#'
#' # change scale multipliers, SI defined
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(-6, sec.axis = sec_axis_w_number(-3))
#'
#' # change scale multipliers, not SI defined (best avoided)
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(-8, sec.axis = sec_axis_w_number(-4))
#'
#' # Change quantity name to Spanish
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(label.text = "Longitud de onda,",
#'                         sec.axis = sec_axis_w_frequency(label.text = "Frecuencia,"))
#'
#' # Frequency in secondary axis
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_w_frequency())
#'
#' # Energy (per photon) in atto joules
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_energy_J())
#'
#' # Energy (per photon) in electron volts
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_energy_eV())
#'
#' # Secondary axis with wavelength using a different scale factor
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(sec.axis = sec_axis_wl(-6))
#'
#' # Secondary axes can be added to plots built with autoplot() methods
#' autoplot(sun.spct) +
#'   scale_x_wl_continuous(sec.axis = sec_axis_wl(-6))
#'
sec_axis_w_number <-
  function(unit.exponent = -6,
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.number"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    ggplot2::sec_axis(trans = ~photobiology::wl2wavenumber(., unit.exponent),
                      name = w_number_label(unit.exponent = unit.exponent,
                                            label.text = label.text,
                                            axis.symbols = axis.symbols),
                      breaks = scales::pretty_breaks(n = 7))
  }

#' @rdname sec_axis_w_number
#'
#' @export
#'
sec_axis_w_frequency <-
  function(unit.exponent = 12,
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["freq"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    ggplot2::sec_axis(trans = ~photobiology::wl2frequency(., unit.exponent),
                      name = w_frequency_label(unit.exponent = unit.exponent,
                                               label.text = label.text,
                                               axis.symbols = axis.symbols),
                      breaks = scales::pretty_breaks(n = 7)
    )
  }

#' @rdname sec_axis_w_number
#'
#' @export
#'
sec_axis_energy_eV <-
  function(unit.exponent = 0,
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    ggplot2::sec_axis(trans = ~photobiology::wl2energy(., unit.exponent, unit = "eV"),
                      name = w_energy_eV_label(unit.exponent = unit.exponent,
                                               label.text = label.text,
                                               axis.symbols = axis.symbols),
                      breaks = scales::pretty_breaks(n = 8)
    )
  }

#' @rdname sec_axis_w_number
#'
#' @export
#'
sec_axis_energy_J <-
  function(unit.exponent = -18,
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    ggplot2::sec_axis(trans = ~photobiology::wl2energy(., unit.exponent,
                                                       unit = "joule"),
                      name = w_energy_J_label(unit.exponent = unit.exponent,
                                              label.text = label.text,
                                              axis.symbols = axis.symbols),
                      breaks = scales::pretty_breaks(n = 8)
    )
  }

#' @rdname sec_axis_w_number
#'
#' @export
#'
sec_axis_wl <-
  function(unit.exponent = -9,
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.length"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE)) {
    ggplot2::sec_axis(trans = function(x) {x / 10^(9 + unit.exponent)},
                      name = w_length_label(unit.exponent = unit.exponent,
                                            label.text = label.text,
                                            axis.symbols = axis.symbols),
                      breaks = scales::pretty_breaks(n = 7)
    )
  }

#' Wavelength x-scale
#'
#' Scale x continuous with defaults suitable for wavelengths in nanometres.
#'
#' @details This scale automates the generation of axis labels when the variable
#'   mapped to the \emph{x} aesthetic contains numeric values for wavelengths
#'   expressed in nanometres. This is how spectral data are stored in all the
#'   packages of the R for Photobiology suite, inlcuding the the expected data
#'   by the \code{autoplot()} methods defined in 'ggspectra'.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions.
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
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
#'   scale_x_wl_continuous(unit.exponent = -6)
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   scale_x_wl_continuous(label.text = "Longitud de onda,")
#'
#' autoplot(sun.spct) +
#'   scale_x_wl_continuous(label.text = "Longitud de onda,",
#'                         unit.exponent = -6)
#'
scale_x_wl_continuous <-
  function(unit.exponent = -9,
           name = w_length_label(unit.exponent = unit.exponent,
                                 label.text = label.text,
                                 axis.symbols= axis.symbols),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = unit.exponent + 9),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.length"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }

#' Wavenumber x-scale
#'
#' Scale x continuous with defaults suitable for wavelengths expressed as
#' wavenumbers [\eqn{m^{-2}}].
#'
#' @details This scale automates the generation of axis labels when the variable
#'   mapped to the \emph{x} aesthetic contains numeric values for wavelengths
#'   expressed wavenumbers. This is \strong{not} how spectral data are stored in
#'   all the packages of the R for Photobiology suite and can be used in plots
#'   built with \code{ggplot2()} with explicit mapping using a conversion
#'   function. If desired, a secondary axis can be added manually as described
#'   in \code{\link[ggplot2]{sec_axis}}.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions.
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct, aes(x = wl2wavenumber(w.length), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_wavenumber_continuous()
#'
#' ggplot(sun.spct, aes(x = wl2wavenumber(w.length), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_wavenumber_continuous(unit.exponent = -5)
#'
scale_x_wavenumber_continuous <-
  function(unit.exponent = -6,
           name = w_number_label(unit.exponent = unit.exponent,
                                 label.text = label.text,
                                 axis.symbols = axis.symbols),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = -unit.exponent),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["w.number"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }

#' Frequency x-scale
#'
#' Scale x continuous with defaults suitable for wavelengths expressed as
#' frequencies [Hz].
#'
#' @details This scale automates the generation of axis labels when the variable
#'   mapped to the \emph{x} aesthetic contains numeric values for wavelengths
#'   expressed as frequency. This is \strong{not} how spectral data are stored in
#'   the packages of the R for Photobiology suite and can be only used in plots
#'   built with \code{ggplot2()} with explicit mapping using a conversion
#'   function. If desired, a secondary axis can be added manually as described
#'   in \code{\link[ggplot2]{sec_axis}}.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions.
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct, aes(x = wl2frequency(w.length), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_frequency_continuous()
#'
#' ggplot(sun.spct, aes(x = wl2frequency(w.length), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_frequency_continuous(14)
#'
scale_x_frequency_continuous <-
  function(unit.exponent = 12,
           name = w_frequency_label(unit.exponent = unit.exponent,
                                    label.text = label.text,
                                    axis.symbols = axis.symbols),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = unit.exponent),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["freq"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }

#' Energy per photon x-scale
#'
#' Scale x continuous with defaults suitable for wavelengths expressed as energy
#' per photon [eV] or [J].
#'
#' @details This scale automates the generation of axis labels when the variable
#'   mapped to the \emph{x} aesthetic contains numeric values for wavelengths
#'   expressed as energy per photon. This is \strong{not} how spectral data are
#'   stored in all the packages of the R for Photobiology suite and can be used
#'   in plots built with \code{ggplot2()} with explicit mapping using a
#'   conversion function. If desired, a secondary axis can be added manually as
#'   described in \code{\link[ggplot2]{sec_axis}}.
#'
#' @param unit.exponent integer
#' @param name The name of the scale, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions.
#' @param label.text character Textual portion of the labels.
#' @param axis.symbols logical If \code{TRUE} symbols of the quantities are
#'   added to the \code{name}. Supported only by \code{format = "R.expression"}.
#' @param ... other named arguments passed to \code{scale_y_continuous}
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_continuous}}
#'
#' @export
#'
#' @examples
#'
#' ggplot(sun.spct, aes(x = wl2energy(w.length, unit = "joule"), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_energy_J_continuous()
#'
#' ggplot(sun.spct, aes(x = wl2energy(w.length, unit = "joule"), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_energy_J_continuous(unit.exponent = -19)
#'
#' ggplot(sun.spct, aes(x = wl2energy(w.length, unit = "eV"), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_energy_eV_continuous()
#'
#' ggplot(sun.spct, aes(x = wl2energy(w.length, unit = "eV"), y = s.e.irrad)) +
#'   geom_line() +
#'   scale_x_energy_eV_continuous(unit.exponent = -3)
#'
scale_x_energy_eV_continuous <-
  function(unit.exponent = 0,
           name = w_energy_eV_label(unit.exponent = unit.exponent,
                                    label.text = label.text,
                                    axis.symbols = axis.symbols),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = unit.exponent),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }

#' @rdname scale_x_energy_eV_continuous
#'
#' @export
#'
scale_x_energy_J_continuous <-
  function(unit.exponent = -18,
           name = w_energy_J_label(unit.exponent = unit.exponent,
                                   label.text = label.text,
                                   axis.symbols = axis.symbols),
           breaks = scales::pretty_breaks(n = 7),
           labels = SI_pl_format(exponent = unit.exponent),
           label.text =
             axis_labels(append = ifelse(axis.symbols, ",", ""))[["energy"]],
           axis.symbols = getOption("ggspectra.axis.symbols",
                                    default = TRUE),
           ...) {
    scale_x_continuous(name = name,
                       breaks = breaks,
                       labels = labels,
                       ...)
  }

