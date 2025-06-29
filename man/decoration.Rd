% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decoration.R
\name{decoration}
\alias{decoration}
\title{Add decorations to spectrum plot (private)}
\usage{
decoration(
  w.band,
  y.max,
  y.min,
  x.max,
  x.min,
  x.expanse = x.max - x.min,
  y.expanse = y.max - y.min,
  annotations,
  span,
  strict = FALSE,
  wls.target = "HM",
  label.qty,
  label.mult = 1,
  summary.label,
  unit.out = NULL,
  time.unit = NULL,
  text.size = 2.5,
  label.color = NULL,
  chroma.type = "CMF",
  pos.shift = 0,
  by.group = FALSE,
  na.rm = TRUE
)
}
\arguments{
\item{w.band}{waveband object or list of waveband objects passed to
statistics as argument to their \code{w.band} formal parameter.}

\item{y.max, y.min, x.max, x.min, x.expanse, y.expanse}{numeric. Used to compute
the positions of annotations.}

\item{annotations}{character vector with names of annotations.}

\item{span}{numeric passed to \code{stat_peaks()} and \code{stat_valleys()}.}

\item{strict}{logical passed to \code{stat_peaks()} and \code{stat_valleys()}.}

\item{wls.target}{numeric or character vector passed to \code{stat_find_wls()}}

\item{label.qty}{character the quantity for \code{"summaries"} annotaion,
affecting the statistic called or the arguments passed to it.}

\item{summary.label}{character the name of the quantity to be parsed into a
plotmath expression.}

\item{text.size}{numeric giving the size of text for \code{"labels"} and
\code{"summaries"}.}

\item{label.color}{color definition or name}

\item{chroma.type}{character one of "CMF" (color matching function) or "CC"
(color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
Used to generate colour definitions from wavelengths.}

\item{pos.shift}{numeric Shift the position of the annotations.}

\item{by.group}{logical flag If TRUE repeated identical annotation layers are
added for each group within a plot panel as needed for animation. If
\code{FALSE}, the default, single layers are added per panel.}

\item{na.rm}{logical Passed to all statistics and geometries.}
}
\value{
A list of ggplot "components" that can be added to a ggplot object
  with operator \code{+}. The length of the list depends on the value of argument
  \code{annotations}.
}
\description{
Add decorations to plots generated by the \code{autoplot()} methods defined in
this package. It collects code that is common to \code{autoplot()} methods for
different types of spectra but as it may change in the future it is not
exported.
}
\details{
Vectors of character strings passed as argument to
\code{annotations} are parsed so that if the first member string is
\code{"+"}, the remaining members are added to the current default for
annotations; if it is \code{"-"} the remaining members are removed from the
current default for annotations; and if it is \code{"="} the remaining
members become the new default. If the first member is none of these three
strings, the whole vector becomes the new default. If \code{annotations} is
\code{NULL} the annotations are reset to the package defaults. When removing
annotations \code{"title*"}, \code{"peaks*"} and \code{"valleys*"} will
remove any variation of these annotations. The string \code{""} means no
annotations while \code{"reserve.space"} means no annotations but expand y
scale to reserve space for annotations. These two values take precedence over
any other values in the character vector. The order of the names of
annotations has no meaning: the vector is interpreted as a set except for the
three possible "operators" at position 1.
}
\section{Plot Annotations}{
 The recognized annotation names are: \code{"summaries"}, \code{"peaks"},
  \code{"peak.labels"}, \code{"valleys"}, \code{"valley.labels"},
  \code{"wls"}, \code{"wls.labels"}, \code{"colour.guide"},
  \code{"color.guide"}, \code{"boxes"}, \code{"segments"}, \code{"labels"}.
  In addition, \code{"+"} is interpreted as a request to add to the already
  present default annotations, \code{"-"} as request to remove annotations
  and \code{"="} or missing\code{"+"} and \code{"-"} as a request to reset
  annotations to those requested. If used, \code{"+"}, \code{"-"} or
  \code{"="} must be the first member of a character vector, and followed by
  one or more of the names given above. To simultaneously add and remove
  annotations one can pass a \code{list} containing \code{character} vectors
  each assembled as described. The vectors are applied in the order they
  appear in the list. To disable all annotations pass \code{""} or
  \code{c("=", "")} as argument. Adding a variation of an annotation already
  present, replaces the existing one automatically: e.g., adding
  \code{"peak.labels"} replaces\code{"peaks"} if present.

  The annotation layers are added to the plot using statistics defined in 'ggspectra':
  \code{\link{stat_peaks}}, \code{\link{stat_valleys}},
  \code{\link{stat_label_peaks}}, \code{\link{stat_label_valleys}},
  \code{\link{stat_find_wls}}, \code{\link{stat_spikes}},
  \code{\link{stat_wb_total}}, \code{\link{stat_wb_mean}},
  \code{\link{stat_wb_irrad}}, \code{\link{stat_wb_sirrad}},
  \code{\link{stat_wb_contribution}}, \code{\link{stat_wb_relative}},
  and \code{\link{stat_wl_strip}}. However, only some of their parameters
  can be passed arguments through \code{autoplot} methods. In some cases
  the defaults used by \code{autoplot} methods are not the defaults of the
  statistics.
}

\keyword{internal}
