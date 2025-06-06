% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis-utils-filter-spct.R
\name{scale_y_Afr_continuous}
\alias{scale_y_Afr_continuous}
\title{Absorptance y-scale}
\usage{
scale_y_Afr_continuous(
  unit.exponent = ifelse(pc.out, -2, 0),
  name = Afr_label(unit.exponent = unit.exponent, format = format, label.text =
    label.text, scaled = scaled, normalized = round(normalized, 1), axis.symbols =
    axis.symbols),
  labels = SI_pl_format(exponent = unit.exponent),
  limits = c(0, 1),
  format = getOption("photobiology.math", default = "R.expression"),
  label.text = axis_labels(append = ifelse(axis.symbols, ",", ""))[["s.Afr"]],
  scaled = FALSE,
  normalized = FALSE,
  axis.symbols = getOption("ggspectra.axis.symbols", default = TRUE),
  pc.out = getOption("ggspectra.pc.out", default = FALSE),
  ...
)
}
\arguments{
\item{unit.exponent}{integer}

\item{name}{The name of the scale, used for the axis-label.}

\item{labels}{The tick labels or a function to generate them.}

\item{limits}{One of \code{NULL} for default based on data range, a numeric
vector of length two (\code{NA} allowed) or a function that accepts the
data-based limits as argument and returns new limits.}

\item{format}{character string, "R", "R.expression", "R.character", or
"LaTeX".}

\item{label.text}{character Textual portion of the labels.}

\item{scaled}{logical If \code{TRUE} relative units are assumed.}

\item{normalized}{logical (\code{FALSE}) or numeric Normalization wavelength
in manometers (nm).}

\item{axis.symbols}{logical If \code{TRUE} symbols of the quantities are
added to the \code{name}. Supported only by \code{format = "R.expression"}.}

\item{pc.out}{logical, if TRUE use percent as default instead of fraction
of one.}

\item{...}{other named arguments passed to \code{scale_y_continuous}}
}
\description{
Scale y continuous with defaults suitable for spectral absorptance.
}
\note{
This function only alters two default arguments, please, see
documentation for \code{\link[ggplot2]{scale_continuous}}
}
\examples{

Afr_as_default()

ggplot(yellow_gel.spct) +
  geom_line() +
  scale_y_Afr_continuous() +
  scale_x_wl_continuous()

ggplot(yellow_gel.spct) +
  geom_line() +
  scale_y_Afr_continuous(unit.exponent = -2) +
  scale_x_wl_continuous()

ggplot(yellow_gel.spct) +
  geom_line() +
  scale_y_Afr_continuous(unit.exponent = -3) +
  scale_x_wl_continuous()

ggplot(yellow_gel.spct) +
  geom_line() +
  scale_y_Afr_continuous(axis.symbols = FALSE) +
  scale_x_wl_continuous(axis.symbols = FALSE)

unset_filter_qty_default()

}
