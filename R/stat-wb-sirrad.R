#' Integrate spectral irradiance for wavebands.
#'
#' \code{stat_wb_sirrad} computes areas under a curve.
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
#' @param label.qty character
#' @param label.mult numeric Scaling factor applied to y-integral values before
#'   conversion into character strings.
#' @param label.fmt character string giving a format definition for converting
#'   y-integral values into character strings by means of function
#'   \code{\link{sprintf}}.
#' @param ypos.mult numeric Multiplier constant used to scale returned
#'   \code{y} values.
#' @param xpos.fixed,ypos.fixed numeric If not \code{NULL} used a constant value returned
#'   in \code{x} or \code{y}.
#'
#' @return A data frame with one row for each waveband object in the argument
#' to \code{w.band}. Wavebeand outside the range of the spectral data are
#' trimmed or discarded.
#'
#' @section Computed variables:
#' What it is named integral below is the result of appying \code{irrad},
#' \code{e_irrad} or \code{q_irrad} to the data.
#' \describe{
#'   \item{y.label}{yeff multiplied by \code{label.mult} and formatted
#'   according to \code{label.fmt}}
#'   \item{x}{w.band-midpoint}
#'   \item{xmin}{w.band minimum}
#'   \item{xmax}{w.band maximum}
#'   \item{ymin}{data$y minimum}
#'   \item{ymax}{data$y maximum}
#'   \item{yeff}{weighted irradiance if \code{w.band} describes a BSWF}
#'   \item{yint}{not weighted irradiance for the range of \code{w.band}}
#'   \item{xmean}{yint divided by spread(w.band)}
#'   \item{y}{ypos.fixed or top of data, adjusted by \code{ypos.mult}}
#'   \item{wb.color}{color of the w.band}
#'   \item{wb.name}{label of w.band}
#'   \item{BW.color}{\code{black_or_white(wb.color)}}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{label}{..y.label..}
#'   \item{x}{..x..}
#'   \item{xmin}{..xmin..}
#'   \item{xmax}{..xmax..}
#'   \item{ymin}{0}
#'   \item{ymax}{..ymean..}
#'   \item{yintercept}{..ymean..}
#'   \item{fill}{..wb.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity}
#' }
#'
#' @examples
#' library(photobiology)
#' library(photobiologyWavebands)
#' library(ggplot2)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   stat_wb_column(w.band = VIS_bands()) +
#'   stat_wb_e_sirrad(w.band = VIS_bands(), angle = 90, size = 4,
#'                   label.fmt = "%1.2f", ypos.fixed = 0.1) +
#'   geom_line() +
#'   scale_fill_identity() + scale_color_identity()
#'
#' ggplot(sun.spct, unit.out = "photon") +
#'   geom_line() +
#'   stat_wb_hbar(w.band = PAR(), size = 1) +
#'   stat_wb_q_sirrad(aes(color = ..wb.color..),
#'                    w.band = PAR(), label.fmt = "mean = %.3g",
#'                    ypos.mult = 1, xpos.fixed = 390, hjust = 1) +
#'   scale_color_identity()
#'
#' @export
#' @family stats functions
#'
stat_wb_sirrad <- function(mapping = NULL, data = NULL, geom = "text",
                       w.band = NULL,
                       time.unit,
                       unit.in,
                       label.qty = "mean",
                       label.mult = 1,
                       label.fmt = "%.3g",
                       ypos.mult = 0.55,
                       xpos.fixed = NULL,
                       ypos.fixed = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbSIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
                  label.mult = label.mult,
                  label.fmt = label.fmt,
                  ypos.mult = ypos.mult,
                  xpos.fixed = xpos.fixed,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_wb_sirrad
#' @export
stat_wb_e_sirrad <- function(mapping = NULL, data = NULL, geom = "text",
                           w.band = NULL,
                           time.unit = "second",
                           unit.in = "energy",
                           label.qty = "mean",
                           label.mult = 1,
                           label.fmt = "%.3g",
                           ypos.mult = 0.55,
                           xpos.fixed = NULL,
                           ypos.fixed = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbSIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
                  label.mult = label.mult,
                  label.fmt = label.fmt,
                  ypos.mult = ypos.mult,
                  xpos.fixed = xpos.fixed,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_wb_sirrad
#' @export
stat_wb_q_sirrad <- function(mapping = NULL, data = NULL, geom = "text",
                           w.band = NULL,
                           time.unit = "second",
                           unit.in = "photon",
                           label.qty = "mean",
                           label.mult = 1,
                           label.fmt = "%.3g",
                           ypos.mult = 1.07,
                           xpos.fixed = NULL,
                           ypos.fixed = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbSIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
                  label.mult = label.mult,
                  label.fmt = label.fmt,
                  ypos.mult = ypos.mult,
                  xpos.fixed = xpos.fixed,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbSIrrad <-
  ggplot2::ggproto("StatWbSIrrad", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            time.unit,
                                            unit.in,
                                            label.qty,
                                            label.mult,
                                            label.fmt,
                                            ypos.mult,
                                            xpos.fixed,
                                            ypos.fixed) {
                     if (length(w.band) == 0) {
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
                       tmp.spct <- source_spct(w.length = data$x, s.e.irrad = data$y,
                                               time.unit = time.unit)
                     } else if (unit.in %in% c("photon", "quantum")) {
                       tmp.spct <- source_spct(w.length = data$x, s.q.irrad = data$y,
                                               time.unit = time.unit)
                     } else {
                       stop("Bad 'unit.in' argument.")
                     }
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }
                       yeff.tmp <- irrad(tmp.spct, wb, quantity = label.qty,
                                         use.hinges = TRUE,
                                         unit.out = unit.in)
                       yint.tmp <- irrad(tmp.spct, waveband(range(wb)), quantity = "total",
                                         use.hinges = TRUE,
                                         unit.out = unit.in)
                       ymean.tmp <- irrad(tmp.spct, waveband(range(wb)), quantity = "mean",
                                          use.hinges = TRUE,
                                          unit.out = unit.in)
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(wb),
                                                    xmin = min(wb),
                                                    xmax = max(wb),
                                                    yeff = yeff.tmp,
                                                    yint = yint.tmp,
                                                    ymax = max(data$y),
                                                    ymin = min(data$y),
                                                    ymean = ymean.tmp,
                                                    wb.color = color(wb),
                                                    wb.name = labels(wb)$label,
                                                    BW.color = black_or_white(color(wb)))
                                         )
                     }
                     if (!is.null(xpos.fixed)) {
                       integ.df$x <- xpos.fixed
                     }
                     if (is.null(ypos.fixed)) {
                       integ.df$y <- with(integ.df, ymin + (ymean - ymin) * ypos.mult)
                     } else {
                       integ.df$y <- ypos.fixed
                     }
                     integ.df$y.label <- sprintf(label.fmt, integ.df$yeff * label.mult)
                     integ.df$y.label <- sprintf(label.fmt, integ.df$yeff * label.mult)
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = ..y.label..,
                                              xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              ymin = 0,
                                              ymax = ..ymean..,
                                              yintercept = ..ymean..,
                                              fill = ..wb.color..,
                                              color = ..BW.color..),
                   required_aes = c("x", "y")
  )

