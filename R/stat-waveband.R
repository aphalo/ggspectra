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
stat_waveband <- function(mapping = NULL, data = NULL, geom = "rect",
                       w.band = NULL,
                       integral.fun = "mean",
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
                     if (is.null(w.band)) {
                       w.band <- waveband(data$x)
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     if (is.character(integral.fun)) {
                       if (integral.fun %in% c("mean", "average")) {
                         integral.fun <- function(xx, yy) {
                           photobiology::integrate_xy(xx, yy) / diff(range(xx))
                         }
                       } else if (integral.fun == "total") {
                         integral.fun <- phtobiology::integrate_xy
                       } else {
                         stop("'integral.fun' value '", integral.fun, "' not supported.")
                       }
                     } else {
                       stopifnot(is.function(integral.fun))
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
                                                    y = integral.fun(mydata$x, mydata$y),
                                                    wb.color = color(wb),
                                                    wb.name = labels(wb)$label)
                                         )
                     }
                     integ.df$ymid <- integ.df$y / 2
                     integ.df$y.label <- sprintf(label.fmt, integ.df$y)
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = ..y.label..,
                                              x = ..x..,
                                              xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              y = ..ymid..,
                                              ymax = ..y..,
                                              ymin = 0 * ..y..,
                                              yintercept = ..y..,
                                              color = ..wb.color..,
                                              fill = ..wb.color..),
                   required_aes = c("x", "y")
  )

#' @rdname stat_waveband
#'
#' @param label.y numeric position of label
#' @param rect.alpha numeric transparency of "rect"
#' @param guide.position character or numericguiving y positon of "guide"
#' @param guide.width numeric y-width of the "guide"
#'
#' @export
#'
waveband_guide <- function(mapping = NULL, data = NULL,
                          w.band = NULL,
                          integral.fun = "mean",
                          label.fmt = "%.1f", label.y = 0.3,
                          guide.position = "bottom",
                          guide.width = 0.05,
                          rect.alpha = 0.7,
                          position = "identity", na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE, ...){
  if (is.character(guide.position)) {
    ymax <- switch (guide.position,
      bottom = 0.1,
      middle = 0.5,
      top    = 1.0,
      NA
    )
    ymin = ymax - guide.width
  }
  list(
    stat_waveband(mapping = mapping, data = data,
                  geom = "rect",
                  w.band = w.band,
                  integral.fun = integral.fun,
                  label.fmt = label.fmt,
                  position = position,
                  na.rm = na.rm,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  alpha = rect.alpha,
                  ymax = ymax,
                  ymin = ymin,
                  color = "black",
                  size = 1,
                  ...),
    stat_waveband(mapping = mapping, data = data,
                  geom = "text",
                  w.band = w.band,
                  integral.fun = integral.fun,
                  label.fmt = label.fmt,
                  position = position,
                  na.rm = na.rm,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  y = ymax - 0.5 * guide.width,
                  color = "white",
                  ...),
    scale_fill_identity()
  )
}