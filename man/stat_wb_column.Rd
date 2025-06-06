% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-wb-column.R
\name{stat_wb_column}
\alias{stat_wb_column}
\title{Integrate ranges under curve.}
\usage{
stat_wb_column(
  mapping = NULL,
  data = NULL,
  geom = "rect",
  position = "identity",
  ...,
  w.band = NULL,
  integral.fun = integrate_xy,
  chroma.type = "CMF",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{The aesthetic mapping, usually constructed with
\code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
to be set at the layer level if you are overriding the plot defaults.}

\item{data}{A layer specific dataset - only needed if you want to override
the plot defaults.}

\item{geom}{The geometric object to use display the data}

\item{position}{The position adjustment to use for overlapping points on this
layer}

\item{...}{other arguments passed on to \code{\link[ggplot2]{layer}}. This
can include aesthetics whose values you want to set, not map. See
\code{\link[ggplot2]{layer}} for more details.}

\item{w.band}{a waveband object or a list of waveband objects or numeric
vector of at least length two.}

\item{integral.fun}{function on $x$ and $y$.}

\item{chroma.type}{character one of "CMF" (color matching function) or "CC"
(color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.}

\item{na.rm}{a logical value indicating whether NA values should be stripped
before the computation proceeds.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
never includes, and \code{TRUE} always includes.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics, rather
than combining with them. This is most useful for helper functions that
define both data and aesthetics and shouldn't inherit behaviour from the
default plot specification, e.g. \code{\link[ggplot2]{borders}}.}
}
\value{
A data frame with one row for each waveband object in the argument
to \code{w.band}. Wavebeand outside the range of the spectral data are
trimmed or discarded.
}
\description{
\code{stat_wb_column} computes means under a curve. It first integrates the
  area under a spectral curve and also the mean expressed per nanaometre of
  wavelength for each waveband in the input. Sets suitable default aesthetics
  for \code{geom_rect()}. \strong{\code{x}-scale transformations and axis
  flipping are currently not supported}.
}
\note{
If the argument passed to \code{w.band} is a BSWF it is silently
  converted to a wavelength range and the average of spectral values without
  weighting is returned as default value for \code{ymax} while the default
  value for \code{ymin} is zero.
}
\section{Computed variables}{

What it is named integral below is the result of appying \code{integral.fun},
with default \code{integrate_xy}.
\describe{
  \item{x}{w.band-midpoint}
  \item{wb.xmin}{w.band minimum}
  \item{wb.xmax}{w.band maximum}
  \item{wb.ymin}{data$y minimum}
  \item{wb.ymax}{data$y maximum}
  \item{wb.ymean}{yint divided by wl_expanse(w.band)}
  \item{y}{wb.ymeam}
  \item{wb.color}{color of the w.band}
  \item{wb.name}{label of w.band}
  \item{BW.color}{\code{black_or_white(wb.color)}}
}
}

\section{Default aesthetics}{

Set by the statistic and available to geoms.
\describe{
  \item{xmin}{..wb.xmin..}
  \item{xmax}{..wb.xmax..}
  \item{ymin}{0}
  \item{ymax}{..wb.ymean..}
  \item{fill}{..wb.color..}
}
}

\section{Required aesthetics}{

Required by the statistic and need to be set with \code{aes()}.
\describe{
  \item{x}{numeric, wavelength in nanometres}
  \item{y}{numeric, a spectral quantity}
}
}

\examples{

library(photobiologyWavebands)
# ggplot() methods for spectral objects set a default mapping for x and y.
ggplot(sun.spct) +
  stat_wb_column(w.band = VIS_bands()) +
  geom_line() +
  scale_fill_identity()

ggplot(sun.spct) +
  stat_wb_column(w.band = VIS_bands(), alpha = 0.5) +
  geom_line() +
  scale_fill_identity()

}
\seealso{
Other stats functions: 
\code{\link{stat_color}()},
\code{\link{stat_find_qtys}()},
\code{\link{stat_find_wls}()},
\code{\link{stat_label_peaks}()},
\code{\link{stat_peaks}()},
\code{\link{stat_spikes}()},
\code{\link{stat_wb_box}()},
\code{\link{stat_wb_contribution}()},
\code{\link{stat_wb_hbar}()},
\code{\link{stat_wb_irrad}()},
\code{\link{stat_wb_label}()},
\code{\link{stat_wb_mean}()},
\code{\link{stat_wb_relative}()},
\code{\link{stat_wb_sirrad}()},
\code{\link{stat_wb_total}()},
\code{\link{stat_wl_strip}()},
\code{\link{stat_wl_summary}()}
}
\concept{stats functions}
