#' Default text for axis labels
#'
#' Obtain texts used by default for axis labels in
#'   plots. They contain only the text part, but not symbols or units of
#'   expression. Can be used to change the language or to suppress the text.
#'
#' @return A character vector
#'
#' @rdname axis_labels
#'
#' @export
#'
#' @examples
#' axis_labels()[["w.length"]]
#'
axis_labels_uk <- function(){
  c(w.length = "Wavelength",
    w.number = "Wavenumber",
    freq = "Frequency",
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
axis_labels_none <- function(){
  c(w.length = "",
    w.number = "",
    freq = "",
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
axis_labels <- axis_labels_uk
