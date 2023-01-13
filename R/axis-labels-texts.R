#' Default text for axis labels
#'
#' Texts used by default for axis labels in plots are recalled from character
#' vectors returned by these functions. The aim is that their default values can
#' be easily changed or translated to other languages. They contain only the
#' text part, but not symbols or units of expression.
#'
#' @details By default \code{axis_labes()} contains a copy of
#'   \code{axis_labels_uk_comma()}. By assigning to this name a user function
#'   that returns a named character vector using the same names as those
#'   returned by these funtcions, it is possible to temporarily change
#'   the default texts.
#'
#' @return A character vector
#'
#' @rdname axis_labels
#'
#' @export
#'
#' @examples
#' axis_labels()[["w.length"]] # ending in a comma
#' axis_labels <- axis_labels_uk
#' axis_labels()[["w.length"]] # no comma
#'
axis_labels_uk <- function(){
  c(w.length = "Wavelength",
    w.number = "Wavenumber",
    freq = "Frequency",
    energy = "Energy per photon",
    s.irrad = "Spectral irradiance",
    s.e.irrad = "Spectral energy irradiance",
    s.q.irrad = "Spectral photon irradiance",
    s.response = "Spectral response",
    s.e.response = "Spectral energy response",
    s.q.response = "Spectral photon response",
    s.e.action = "Spectral energy action",
    s.q.action = "Spectral photon action",
    A = "Spectral absorbance",
    A.int = "Internal spectral absorbance",
    A.tot = "Total spectral absorbance",
    Afr = "Spectral absorptance",
    Tfr = "Spectral transmittance",
    Tfr.int = "Internal spectral transmittance",
    Tfr.tot = "Total spectral transmittance",
    Rfr = "Spectral reflectance",
    Rfr.spec = "Specular spectral reflectance",
    Rfr.tot = "Total spectral reflectance",
    e.mult = "Pixel response multipliers",
    q.mult = "Pixel response multipliers",
    cps = "Pixel response rate",
    counts = "Pixel response"
  )

}

#' @rdname axis_labels
#'
#' @export
#'
axis_labels_uk_comma <- function(){
  c(w.length = "Wavelength,",
    w.number = "Wavenumber,",
    freq = "Frequency,",
    energy = "Energy per photon,",
    s.irrad = "Spectral irradiance,",
    s.e.irrad = "Spectral energy irradiance,",
    s.q.irrad = "Spectral photon irradiance,",
    s.response = "Spectral response,",
    s.e.response = "Spectral energy response,",
    s.q.response = "Spectral photon response,",
    s.e.action = "Spectral energy action,",
    s.q.action = "Spectral photon action,",
    A = "Spectral absorbance,",
    A.int = "Internal spectral absorbance,",
    A.tot = "Total spectral absorbance,",
    Afr = "Spectral absorptance,",
    Tfr = "Spectral transmittance,",
    Tfr.int = "Internal spectral transmittance,",
    Tfr.tot = "Total spectral transmittance,",
    Rfr = "Spectral reflectance,",
    Rfr.spec = "Specular spectral reflectance,",
    Rfr.tot = "Total spectral reflectance,",
    e.mult = "Pixel response multipliers,",
    q.mult = "Pixel response multipliers,",
    cps = "Pixel response rate,",
    counts = "Pixel response,"
  )
}

#' @rdname axis_labels
#'
#' @export
#'
axis_labels_none <- function(){
  c(w.length = "",
    w.number = "",
    freq = "",
    energy = "",
    s.irrad = "",
    s.e.irrad = "",
    s.q.irrad = "",
    s.response = "",
    s.e.response = "",
    s.q.response = "",
    s.e.action = "",
    s.q.action = "",
    A = "",
    A.int = "",
    A.tot = "",
    Afr = "",
    Tfr = "",
    Tfr.int = "",
    Tfr.tot = "",
    Rfr = "",
    Rfr.spec = "",
    Rfr.tot = "",
    e.mult = "",
    q.mult = "",
    cps = "",
    counts = ""
  )
}

#' @rdname axis_labels
#'
#' @export
#'
#' @order 1
#'
axis_labels <- axis_labels_uk_comma
