#' Counts-per-second axis labels
#'
#' Generate cps axis labels in SI units,
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
cps_label <- function(format = getOption("photobiology.math",
                                            default = "R.expression")) {
  if (tolower(format) == "latex") {
    "Pixel response rate $\\mathrm{cps}(\\lambda)$ (counts~s$^{-1}$)"
  } else if (format %in% c("R.expression")) {
    expression(Pixel~~response~~rate~~plain(cps)(lambda)~~(counts~~s^{-1}))
  } else if (format == "R.character") {
    "Pixel response cps(lambda) (counts/s)"
  } else {
    warning("'format = ", format, "' not implemented")
  }
}

#' Counts-per-second  y-scale
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
#' ggplot(led.cps_spct) +
#'   geom_line() +
#'   scale_y_cps_continuous()
#'
scale_y_cps_continuous <- function(name = cps_label(),
                                   ...) {
  scale_y_continuous(name = name,
                     ...)
}
