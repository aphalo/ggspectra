#' Integrate ranges under curve.
#'
#' \code{stat_waveband} computes areas under a curve.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be
#'   stripped before the computation proceeds.
#' @param w.band a numeric vector of at least length two.
#' @param integral.fun function on $x$ and $y$.
#' @param label.fmt character string giving a format definition for converting
#'   y-integral values into character strings by means of function \code{\link{sprintf}}.
#' @section Computed variables:
#' \describe{
#'   \item{label}{intergral value as formatted text}
#'   \item{x}{w.band-midpoint}
#'   \item{xmin}{w.band minimum}
#'   \item{xmax}{w.band maximum}
#'   \item{y}{integral value as numeric}
#' }
#'
#' @import photobiology
#'
#' @examples
#' library(photobiology)
#' library(ggplot2)
#' ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
#'   stat_waveband()
#'
#' @export
#' @family stats functions
#'
stat_waveband <- function(mapping = NULL, data = NULL, geom = "text",
                       w.band = NULL,
                       integral.fun = integrate_xy,
                       label.fmt = "%.1f",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWaveband, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  integral.fun = integral.fun,
                  label.fmt = label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWaveband <-
  ggplot2::ggproto("StatWaveband", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            integral.fun,
                                            label.fmt,
                                            summary.fmt) {
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) {
                         wb <- waveband(wb)
                       }
                       wb <- trim_wl(wb, range = data$x)
                       range <- range(wb)
                       mydata <- trim_tails(data$x, data$y,
                                                          low.limit = range[1],
                                                          high.limit = range[2])
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(mydata$x),
                                                    xmin = range[1],
                                                    xmax = range[2],
                                                    y = integral.fun(mydata$x, mydata$y) /
                                                      (range[2] - range[1]),
                                                    wb.color = color(wb)$CMF,
                                                    wb.name = labels(wb)$label)
                                         )
                     }
                     integ.df$y.label <- sprintf(label.fmt, integ.df$y)
                     integ.df$ymid <- integ.df$y / 2
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = ..y.label..,
                                              x = ..x..,
                                              xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              y = 0 * ..y..,
                                              ymax = ..y..,
                                              ymin = 0 * ..y..,
                                              yintercept = ..y..,
                                              color = ..wb.color..,
                                              fill = ..wb.color..),
                   required_aes = c("x", "y")
  )
