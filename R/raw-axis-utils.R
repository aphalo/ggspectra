#' Raw-counts axis labels
#'
#' Generate wavelength, wavenumber and wave frequency axis labels in SI units,
#' using SI scale factors. Output can be selected as character, expression (R
#' default devices) or LaTeX (for tikz device).
#'
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @return a character string or an R expression.
#'
#' @export
#'
#' @examples
#' counts_label()
#' counts_label("R.expression")
#' counts_label("LaTeX")
#'
counts_label <- function(format = getOption("photobiology.math",
                                            default = "R.expression")) {
  if (tolower(format) == "latex") {
    "Pixel response $\\mathrm{N}(\\lambda)$ (counts)"
  } else if (format %in% c("R.expression")) {
    expression(Pixel~~response~~N(lambda)~~(counts))
  } else if (format == "R.character") {
    "Pixel response N(lambda) (counts)"
  } else {
    warning("'format = ", format, "' not implemented")
  }
}

#' Raw-counts y-scale
#'
#' Scale y continuous with defaults suitable for raw detector counts.
#'
#' @note This function only alters two default arguments, please, see
#' documentation for \code{\link[ggplot2]{scale_y_continuous}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' ggplot(led.raw_spct) +
#'   geom_line() +
#'   scale_y_counts_continuous()
#'
scale_y_counts_continuous <- function(name = counts_label(),
                                      ...) {
  scale_y_continuous(name = name,
                     ...)
}
