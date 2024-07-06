#' Default text for axis labels
#'
#' Texts used by default for axis labels in plots are recalled from character
#' vectors returned by these functions. The aim is that their default values can
#' be easily changed or translated to other languages. They contain only the
#' text part, but not symbols or units of expression.
#'
#' @param append character The string to be appended to each label,
#' @param sep character Passed to function \code{paste} as argument for
#' parameter \code{sep}.
#'
#' @details By default \code{axis_labels()} contains a copy of
#'   \code{axis_labels_uk_comma()}. By assigning to this name a user function
#'   that returns a named character vector using the same names as those
#'   returned by these functions, it is possible to temporarily change
#'   the default texts.
#'
#'   Currently only UK English label texts are predefined and
#'   \code{axis_labels()} is a synonym of \code{axis_labels_uk()}.
#'
#' @return A character vector
#'
#' @rdname axis_labels
#'
#' @export
#'
#' @examples
#' axis_labels()[["w.length"]] # ending in a comma
#' axis_labels_uk()[["w.length"]] # no comma
#' axis_labels_none()[["w.length"]] # empty label
#'
axis_labels_uk <- function(append = "", sep = ""){
  z <-
    c(w.length = "Wavelength",
      w.number = "Wavenumber",
      freq = "Frequency",
      energy = "Energy per photon",
      irrad = "Irradiance",
      e.irrad = "Energy irradiance",
      q.irrad = "Photon irradiance",
      s.irrad = "Spectral irradiance",
      s.e.irrad = "Spectral energy irradiance",
      s.q.irrad = "Spectral photon irradiance",
      response = "Response",
      e.response = "Energy response",
      q.response = "Photon response",
      s.response = "Spectral response",
      s.e.response = "Spectral energy response",
      s.q.response = "Spectral photon response",
      s.e.action = "Spectral energy action",
      s.q.action = "Spectral photon action",
      A = "Absorbance",
      A.int = "Internal absorbance",
      A.tot = "Total absorbance",
      s.A = "Spectral absorbance",
      s.A.int = "Internal spectral absorbance",
      s.A.tot = "Total spectral absorbance",
      Afr = "Absorptance",
      s.Afr = "Spectral absorptance",
      Tfr = "Transmittance",
      Tfr.int = "Internal transmittance",
      Tfr.tot = "Total transmittance",
      s.Tfr = "Spectral transmittance",
      s.Tfr.int = "Internal spectral transmittance",
      s.Tfr.tot = "Total spectral transmittance",
      Rfr = "Reflectance",
      Rfr.spec = "Specular reflectance",
      Rfr.tot = "Total reflectance",
      s.Rfr = "Spectral reflectance",
      s.Rfr.spec = "Specular spectral reflectance",
      s.Rfr.tot = "Total spectral reflectance",
      e.mult = "Pixel response multipliers",
      q.mult = "Pixel response multipliers",
      cps = "Pixel response rate",
      counts = "Pixel response"
    )

  if (length(append) && append != "") {
    z <- axis_labels_uk(append = "")
    zz <- paste(z, append, sep = sep)
    names(zz) <- names(z)
    stopifnot(length(zz) == length(z))
    zz
  } else {
    z
  }
}

#' @rdname axis_labels
#'
#' @export
#'
axis_labels_uk_comma <- function(){
  axis_labels_uk(append = ", ", sep = "")
}

#' @rdname axis_labels
#'
#' @export
#'
axis_labels_none <- function(){
  z.names <- names(axis_labels_uk())
  z <- rep_len("", length(z.names))
  names(z) <- z.names
  z
}

#' @rdname axis_labels
#'
#' @export
#'
axis_labels <- axis_labels_uk_comma

