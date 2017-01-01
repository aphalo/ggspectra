#' Wavelength conversions
#'
#' Convert wavelength to wavenumber and frequency
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
w_number <- function(w.length, unit.exponent = 0) {
  1e6 / w.length / 10^unit.exponent # 1/m
}

#' @rdname w_number
#'
#' @export
#'
w_frequency <- function(w.length, unit.exponent = 0) {
  299792458 / (w.length * 1e-6) / 10^unit.exponent # speed of light [m/s] / w.length [m]
}

#' Wave- axis labels
#'
#' Generate wavelength, wavenumber and wave frequency axis labels in SI units.
#'
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param unit.exponent integer
#'
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#' w_length_label()
#' w_length_label("R.expression")
#' w_length_label("LaTeX")
#' w_number_label()
#' w_number_label("R.expression")
#' w_frequency_label()
#' w_frequency_label("R.expression")
#'
w_length_label <- function(format = "R", unit.exponent = "n") {
  if (format %in% c("LaTeX")) {
    if (is.character(unit.exponent)) {
      paste("Wavelength (", unit.exponent, "m)", sep = "")
    } else {
      if (unit.exponent == 0) {
        "Wavelength (m)"
      } else {
        paste("Wavelength ($\times 10^{", unit.exponent, "}~m)", sep = "")
      }
    }
  } else if (format %in% c("R", "R.expression")) {
    if (is.character(unit.exponent)) {
      bquote(plain(Wavelength)~~(plain(.(unit.exponent))*plain(m)))
    } else {
      if (unit.exponent == 0) {
        expression(plain(Wavelength)~~(plain(m)))
      } else {
        bquote(plain(Wavelength)~~(10^{.(unit.exponent)}*plain(m)))
      }
    }
  } else if (format == "") {
    ""
  } else {
    warning("'format = ", format, "' not implemented")
    NA_character_
  }
}

#' @rdname w_length_label
#'
#' @export
#'
w_number_label <- function(format = "R", unit.exponent = "") {
  if (format %in% c("LaTeX")) {
    if (is.character(unit.exponent)) {
      paste("Wavenumber (", unit.exponent, "m$^{-1}$)", sep = "")
    } else {
      if (unit.exponent == 0) {
        "Wavenumber (m$^{-1}$)"
      } else {
        paste("Wavenumber ($\times 10^{", unit.exponent, "}~m$^{-1}$)", sep = "")
      }
    }
  } else if (format %in% c("R", "R.expression")) {
    if (is.character(unit.exponent)) {
      bquote(plain(Wavenumber)~~(plain(.(unit.exponent))*plain(m)^{-1}))
    } else {
      if (unit.exponent == 0) {
        expression(plain(Wavenumber)~~(plain(m)))
      } else {
        bquote(plain(Wavenumber)~~(10^{.(unit.exponent)}*plain(m)^{-1}))
      }
    }
  } else if (format == "") {
    ""
  } else {
    warning("'format = ", format, "' not implemented")
    NA_character_
  }
}

#' @rdname w_length_label
#'
#' @export
#'
w_frequency_label <- function(format = "R", unit.exponent = "") {
  if (format %in% c("LaTeX")) {
    if (is.character(unit.exponent)) {
      paste("Frequency (", unit.exponent, "Hz)", sep = "")
    } else {
      if (unit.exponent == 0) {
        "Frequency (Hz)"
      } else {
        paste("Frequency ($\times 10^{", unit.exponent, "}~Hz)", sep = "")
      }
    }
  } else if (format %in% c("R", "R.expression")) {
    if (is.character(unit.exponent)) {
      bquote(plain(Frequency)~~(plain(.(unit.exponent))*plain(Hz)))
    } else {
      if (unit.exponent == 0) {
        expression(plain(Frequency)~~(plain(Hz)))
      } else {
        bquote(plain(Frequency)~~(10^{.(unit.exponent)}*plain(Hz)))
      }
    }
  } else if (format == "") {
    ""
  } else {
    warning("'format = ", format, "' not implemented")
    NA_character_
  }
}
