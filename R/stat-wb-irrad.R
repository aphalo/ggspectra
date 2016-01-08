#' Integrate irradiance for wavebands.
#'
#' \code{stat_wb_irrad} computes areas under a curve.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param w.band a waveband object or a list of waveband objects or numeric
#'   vector of at least length two.
#' @param unit.in character One of "photon","quantum" or "energy"
#' @param time.unit character or lubridate::duration
#' @param label.fmt character string giving a format definition for converting
#'   y-integral values into character strings by means of function
#'   \code{\link{sprintf}}.
#' @param y.multiplier numeric Multiplier constant used to scale returned
#'   \code{y} values.
#' @param y.position numeric If not \code{NULL} used a constant value returned
#'   in \code{y}.
#'
#' @section Computed variables:
#' \describe{
#'   \item{y.label}{integral value as formatted text}
#'   \item{x}{w.band-midpoint}
#'   \item{xmin}{w.band minimum}
#'   \item{xmax}{w.band maximum}
#'   \item{yeff}{Effective irradiance as numeric value}
#'   \item{yint}{Mean unweighted irradiance}
#'   \item{y}{Scaled mean value as numeric, or \code{y.position} if not \code{NULL}}
#'   \item{wb.name}{character}
#'   \item{wb.color}{character}
#' }
#'
#' @import photobiology
#'
#' @examples
#' library(photobiology)
#' library(ggplot2)
#' ggplot(sun.spct, unit.out = "photon") + geom_line() +
#'   stat_wb_irrad(unit.in = "photon", time.unit = "second")
#'
#' @export
#' @family stats functions
#'
stat_wb_irrad <- function(mapping = NULL, data = NULL, geom = "text",
                       w.band = NULL,
                       time.unit,
                       unit.in,
                       label.mult = 1,
                       label.fmt = "%.3g",
                       y.multiplier = 1,
                       y.position = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.mult = label.mult,
                  label.fmt = label.fmt,
                  y.multiplier = y.multiplier,
                  y.position = y.position,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbIrrad <-
  ggplot2::ggproto("StatWbIrrad", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            time.unit,
                                            unit.in,
                                            label.mult,
                                            label.fmt,
                                            y.multiplier,
                                            y.position) {
                     if (is.null(w.band)) {
                       w.band <- waveband(data$x)
                     }
                     if (is.any_spct(w.band) ||
                         (is.numeric(w.band) && length(na.omit(w.band)) >= 2)) {
                       w.band <- waveband(range(w.band, na.rm = TRUE))
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     w.band <- trim_wl(w.band, data$x)
                     if (unit.in == "energy") {
                       tmp.spct <- source_spct(w.length = data$x, s.e.irrad = data$y)
                     } else if (unit.in %in% c("photon", "quantum")) {
                       tmp.spct <- source_spct(w.length = data$x, s.q.irrad = data$y)
                     } else {
                       stop("Bad 'unit.in' argument.")
                     }
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }
                       yeff.tmp <- irrad(tmp.spct, wb, use.hinges = TRUE, wb.trim = FALSE,
                                         unit.out = unit.in, time.unit = time.unit)
                       yint.tmp <- irrad(tmp.spct, waveband(wb), use.hinges = TRUE, wb.trim = FALSE,
                                         unit.out = unit.in, time.unit = time.unit)
                       ymult.tmp <- y.multiplier / spread(wb)
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(wb),
                                                    xmin = min(wb),
                                                    xmax = max(wb),
                                                    yeff = yeff.tmp,
                                                    yint = yint.tmp,
                                                    ymult = ymult.tmp,
                                                    wb.color = color(wb),
                                                    wb.name = labels(wb)$label)
                                         )
                     }

                     if (is.null(y.position)) {
                       integ.df$y <- with(integ.df, yint * ymult)
                     } else {
                       integ.df$y <- y.position
                     }
                     integ.df$y.label <- sprintf(label.fmt, integ.df$yeff * label.mult)
                     print(integ.df)
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = ..y.label..,
                                              xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              ymax = ..yint.. * ymult,
                                              ymin = 0 * ..yint..,
                                              yintercept = ..yint.. * ymult,
                                              fill = ..wb.color..),
                   required_aes = c("x", "y")
  )

#' @rdname stat_wb_irrad
#'
#' @param label.y numeric position of label
#' @param rect.alpha numeric transparency of "rect"
#' @param guide.position character or numericguiving y positon of "guide"
#' @param guide.width numeric y-width of the "guide"
#'
#' @export
#'
irrad_guide <- function(mapping = NULL, data = NULL,
                          w.band = NULL,
                          unit.in,
                          time.unit,
                          label.mult = 1,
                          label.fmt = "%.3g", label.y = 0.3,
                          guide.position = "bottom",
                          guide.width = 0.05,
                          rect.alpha = 0.7,
                          position = "identity", na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE, ...) {
  if (is.character(guide.position)) {
    if (guide.position %in% c("bottom2", "middle2", "top2")) {
      guide.width <- guide.width * 1.75
    }
    ymax <- switch(guide.position,
      bottom = 0.0,
      bottom2 = 0.0,
      middle = 0.5 + guide.width / 2,
      middle2 = 0.5 + guide.width / 2,
      top    = 1.05 + guide.width,
      top2    = 1.05 + guide.width,
      NA
    )
    ymin <- (ymax - guide.width)
  }
  list(
    stat_wb_irrad(mapping = mapping, data = data,
                  geom = "rect",
                  w.band = w.band,
                  unit.in = unit.in,
                  time.unit = time.unit,
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
    stat_wb_irrad(mapping = mapping, data = data,
                  geom = "text",
                  w.band = w.band,
                  unit.in = unit.in,
                  time.unit = time.unit,
                  label.mult = label.mult,
                  label.fmt = label.fmt,
                  position = position,
                  y.position = ymax - 0.45 * guide.width,
                  na.rm = na.rm,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  color = "white",
                  size = 2,
                  ...),
    scale_fill_identity()
  )
}

