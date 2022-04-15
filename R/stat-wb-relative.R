#' Integrate ranges under spectral curve.
#'
#' \code{stat_wb_relative} computes means under a curve. It first integrates the
#'   area under a spectral curve and also the mean expressed per nanaometre of
#'   wavelength for each waveband in the input. Sets suitable default aesthetics
#'   for "rect", "hline", "vline", "text" and "label" geoms displaying
#'   values per waveband "relative" to the sum of the wavebands.
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
#' @param label.mult numeric Scaling factor applied to y-integral values before
#'   conversion into character strings.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt character string giving a format definition for converting
#'   y-integral values into character strings by means of function
#'   \code{\link{sprintf}}.
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
#'   \item{y.label}{yint multiplied by \code{label.mult} and formatted
#'   according to \code{label.fmt}}
#'   \item{x}{w.band-midpoint}
#'   \item{wb.xmin}{w.band minimum}
#'   \item{wb.xmax}{w.band maximum}
#'   \item{wb.ymin}{data$y minimum}
#'   \item{wb.ymax}{data$y maximum}
#'   \item{wb.yint}{data$y integral for each mebmer of w.band / sum of data$y
#'   integrals for all wavebands in w.band}
#'   \item{wb.xmean}{yint divided by wl_expanse(w.band)}
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
#'   \item{xmin}{..wb.xmin..}
#'   \item{xmax}{..wb.xmax..}
#'   \item{ymin}{..y.. - (..wb.ymax.. - ..wb.ymin..) * 0.03}
#'   \item{ymax}{..y.. + (..wb.ymax.. - ..wb.ymin..) * 0.03}
#'   \item{yintercept}{..wb.ymean..}
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
#'   geom_line() +
#'   stat_wb_box(w.band = VIS()) +
#'   stat_wb_relative(w.band = VIS()) +
#'   scale_fill_identity() + scale_color_identity()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_box(w.band = VIS_bands()) +
#'   stat_wb_relative(w.band = VIS_bands(), angle = 90, size = 2.5) +
#'   scale_fill_identity() + scale_color_identity()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_box(w.band = VIS_bands()) +
#'   stat_wb_relative(w.band = VIS_bands(), angle = 90, size = 2.5,
#'                    label.mult = 100, label.fmt = "%3.0f%%") +
#'   scale_fill_identity() + scale_color_identity()
#'
#' @export
#' @family stats functions
#'
stat_wb_relative <- function(mapping = NULL, data = NULL, geom = "text",
                       w.band = NULL,
                       integral.fun = integrate_xy,
                       label.mult = 1,
                       chroma.type = "CMF",
                       label.fmt = "%1.2f",
                       ypos.mult = 1.07,
                       ypos.fixed = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbRelative, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  integral.fun = integral.fun,
                  label.mult = label.mult,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
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
StatWbRelative <-
  ggplot2::ggproto("StatWbRelative", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            integral.fun,
                                            label.mult,
                                            chroma.type,
                                            label.fmt,
                                            ypos.mult,
                                            ypos.fixed) {
                     if (length(w.band) == 0) {
                       w.band <- waveband(data$x)
                     }
                     if (is.any_spct(w.band) ||
                         (is.numeric(w.band) && length(stats::na.omit(w.band)) >= 2)) {
                       w.band <- waveband(range(w.band, na.rm = TRUE))
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     stopifnot(is.function(integral.fun))
                     w.band <- trim_wl(w.band, data$x)
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }

                       range <- range(wb)
                       mydata <- trim_tails(data$x, data$y, use.hinges = TRUE,
                                            low.limit = range[1],
                                            high.limit = range[2],
                                            verbose = FALSE)
                       if (is_effective(wb)) {
                         warning("BSWFs not supported by summary: using wavelength range for ",
                                 labels(wb)$label, "'.")
                         wb <- waveband(wb)
                       }
                       yint.tmp <- integral.fun(mydata$x, mydata$y)
                       ymean.tmp <- yint.tmp / wl_expanse(wb)
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(mydata$x),
                                                    xmin = min(wb),
                                                    xmax = max(wb),
                                                    ymin = min(data$y),
                                                    ymax = max(data$y),
                                                    yint = yint.tmp,
                                                    ymean = ymean.tmp,
                                                    wb.color = fast_color_of_wb(wb, chroma.type = chroma.type),
                                                    wb.name = labels(wb)$label,
                                                    BW.color = black_or_white(fast_color_of_wb(wb, chroma.type = chroma.type)))
                                         )
                     }
                     if (is.null(ypos.fixed)) {
                       integ.df$y <- with(integ.df, ymin + (ymax - ymin) * ypos.mult)
                     } else {
                       integ.df$y <- ypos.fixed
                     }
                     integ.df$yint <- integ.df$yint / sum(integ.df$yint, na.rm = TRUE)
                     integ.df$y.label <- sprintf(label.fmt, integ.df$yint * label.mult)
#                     print(integ.df)
                     integ.df
                   },
                   default_aes =
                     ggplot2::aes(label = after_stat(y.label),
                                  xmin = after_stat(xmin),
                                  xmax = after_stat(xmax),
                                  ymin = after_stat(y) - (after_stat(ymax) - after_stat(ymin)) * 0.03,
                                  ymax = after_stat(y) + (after_stat(ymax) - after_stat(ymin)) * 0.03,
                                  yintercept = after_stat(ymean),
                                  fill = after_stat(wb.color),
                                  color = after_stat(BW.color)),
                   required_aes = c("x", "y")
  )
