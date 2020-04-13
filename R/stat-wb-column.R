#' Integrate ranges under curve.
#'
#' \code{stat_wb_column} computes means under a curve. It first integrates the
#'   area under a spectral curve and also the mean expressed per nanaometre of
#'   wavelength for each waveband in the input. Sets suitable default aesthetics
#'   for "rect" geom.
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
#' @param integral.fun function on $x$ and $y$.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#'
#' @return A data frame with one row for each waveband object in the argument
#' to \code{w.band}. Wavebeand outside the range of the spectral data are
#' trimmed or discarded.
#'
#' @section Computed variables:
#' What it is named integral below is the result of appying \code{integral.fun},
#' with default \code{integrate_xy}.
#' \describe{
#'   \item{x}{w.band-midpoint}
#'   \item{wb.xmin}{w.band minimum}
#'   \item{wb.xmax}{w.band maximum}
#'   \item{wb.ymin}{data$y minimum}
#'   \item{wb.ymax}{data$y maximum}
#'   \item{wb.ymean}{yint divided by wl_expanse(w.band)}
#'   \item{y}{wb.ymeam}
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
#'   \item{ymin}{0}
#'   \item{ymax}{..wb.ymean..}
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
#'   stat_wb_column(w.band = VIS_bands()) +
#'   geom_line() +
#'   scale_fill_identity()
#'
#' ggplot(sun.spct) +
#'   stat_wb_column(w.band = VIS_bands(), alpha = 0.5) +
#'   geom_line() +
#'   scale_fill_identity()
#'
#' @note If the argument passed to \code{w.band} is a BSWF it is silently
#'   converted to a wavelength range and the average of spectral values without
#'   weighting is returned as default value for \code{ymax} while the default
#'   value for \code{ymin} is zero.
#'
#' @export
#'
#' @family stats functions
#'
stat_wb_column <- function(mapping = NULL,
                           data = NULL,
                           geom = "rect",
                           w.band = NULL,
                           integral.fun = integrate_xy,
                           chroma.type = "CMF",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  ggplot2::layer(
    stat = StatWbColumn, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  integral.fun = integral.fun,
                  chroma.type = chroma.type,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbColumn <-
  ggplot2::ggproto("StatWbColumn", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            integral.fun,
                                            chroma.type) {
                     if (length(w.band) == 0) {
                       w.band <- waveband(data[["x"]])
                     }
                     if (is.any_spct(w.band) ||
                         (is.numeric(w.band) && length(na.omit(w.band)) >= 2)) {
                       w.band <- waveband(range(w.band, na.rm = TRUE))
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     stopifnot(is.function(integral.fun))
                     w.band <- trim_wl(w.band, data[["x"]])
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }

                       range <- range(wb)
                       mydata <- trim_tails(data[["x"]], data[["y"]], use.hinges = TRUE,
                                            low.limit = range[1],
                                            high.limit = range[2])
                       yint.tmp <- integral.fun(mydata[["x"]], mydata[["y"]])
                       ymean.tmp <- yint.tmp / wl_expanse(wb)
                       wb.color <- color_of(wb, chroma.type = chroma.type) # avoid 'expensive' recalculation
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(mydata[["x"]]),
                                                    wb.xmin = min(wb),
                                                    wb.xmax = max(wb),
                                                    y =  ymean.tmp,
                                                    yzero = 0,
                                                    wb.ymin = min(data[["y"]]),
                                                    wb.ymax = max(data[["y"]]),
                                                    wb.ymean = ymean.tmp,
                                                    wb.color = wb.color,
                                                    wb.name = labels(wb)[["label"]],
                                                    BW.color = black_or_white(wb.color))
                                         )
                     }
                    integ.df
                   },
                   default_aes = ggplot2::aes(xmin = ..wb.xmin..,
                                              xmax = ..wb.xmax..,
                                              ymax = ..wb.ymean..,
                                              ymin = ..yzero.., # a numeric constant is ignored!!
                                              fill = ..wb.color..),
                   required_aes = c("x", "y")
  )

