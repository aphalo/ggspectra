#' Draw colour boxes for wavebands
#'
#' \code{stat_wb_box} plots boxes corresponding to wavebands, by default located
#' slightly above the peak of the spectrum. Sets suitable default aesthetics for
#' "rect" geom.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
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
#' @param ypos.mult numeric Multiplier constant used to scale returned
#'   \code{y} values.
#' @param ypos.fixed numeric If not \code{NULL} used a constant value returned
#'   in \code{y}.
#'
#' @return A data frame with one row for each waveband object in the argument
#' to \code{w.band}. Wavebeand outside the range of the spectral data are
#' trimmed or discarded.
#'
#' @section Computed variables:
#' What it is named integral below is the result of appying \code{integral.fun}
#' to the data, with default \code{integrate_xy}.
#' \describe{
#'   \item{x}{w.band-midpoint}
#'   \item{wb.xmin}{w.band minimum}
#'   \item{wb.xmax}{w.band maximum}
#'   \item{wb.ymin}{data$y minimum}
#'   \item{wb.ymax}{data$y maximum}
#'   \item{y}{ypos.fixed or top of data, adjusted by \code{ypos.mult}}
#'   \item{wb.color}{color of the w.band}
#'   \item{wb.name}{label of w.band}
#'   \item{BW.color}{\code{black_or_white(wb.color)}}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{xmin}{..wb.xmin..}
#'   \item{xmax}{..wb.xmax..}
#'   \item{ymin}{..y.. - (..wb.ymax.. - ..wb.ymin..) * 0.03}
#'   \item{ymax}{..y.. + (..wb.ymax.. - ..wb.ymin..) * 0.03}
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
#'
#' library(photobiologyWavebands)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   stat_wb_box(w.band = VIS_bands()) +
#'   geom_line() +
#'   scale_fill_identity()
#' ggplot(sun.spct) +
#'   stat_wb_box(w.band = VIS_bands(), color = "white") +
#'   geom_line() +
#'   scale_fill_identity()
#'
#' @note The value returned as default value for \code{y} is based on the
#'   y-range of spectral values for the whole data set.
#'
#' @export
#' @family stats functions
#'
stat_wb_box <- function(mapping = NULL,
                        data = NULL,
                        geom = "rect",
                        w.band = NULL,
                        ypos.mult = 1.07,
                        ypos.fixed = NULL,
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbBox, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  ypos.mult = ypos.mult,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbBox <-
  ggplot2::ggproto("StatWbBox", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            ypos.mult,
                                            ypos.fixed) {
                     wl.range <- range(data$x)
                     if (length(w.band) == 0) {
                       w.band <- waveband(wl.range)
                     }
                     if (is.any_spct(w.band) ||
                         (is.numeric(w.band) && length(na.omit(w.band)) >= 2)) {
                       w.band <- waveband(range(w.band, na.rm = TRUE))
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     w.band <- trim_wl(w.band, wl.range)

                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }

                       range <- range(wb)
                       mydata <- trim_tails(data$x, data$y, use.hinges = TRUE,
                                            low.limit = range[1],
                                            high.limit = range[2])
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(mydata$x),
                                                    wb.xmin = min(wb),
                                                    wb.xmax = max(wb),
                                                    wb.ymin = min(data[["y"]]),
                                                    wb.ymax = max(data[["y"]]),
                                                    wb.color = color_of(wb),
                                                    wb.name = labels(wb)[["label"]],
                                                    BW.color = black_or_white(color_of(wb)))
                                         )
                     }
                     if (is.null(ypos.fixed)) {
                       integ.df[["y"]] <- with(integ.df, wb.ymin + (wb.ymax - wb.ymin) * ypos.mult)
                     } else {
                       integ.df[["y"]] <- ypos.fixed
                     }
                     integ.df
                   },
                   default_aes = ggplot2::aes(xmin = ..wb.xmin..,
                                              xmax = ..wb.xmax..,
                                              ymin = ..y.. - (..wb.ymax.. - ..wb.ymin..) * 0.03,
                                              ymax = ..y.. + (..wb.ymax.. - ..wb.ymin..) * 0.03,
                                              fill = ..wb.color..),
                   required_aes = c("x", "y")
  )
