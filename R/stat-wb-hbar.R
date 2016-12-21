#' Integrate ranges under curve.
#'
#' \code{stat_wb_hbar} computes means under a curve. It first integrates the
#' area under a spectral curve and also the mean expressed per nanaometre of
#' wavelength for each waveband in the input. Sets suitable default aesthetics
#' for geoms "errorbarh" and "hline" from 'ggplot', and "linerangeh",
#' and "errorbarh" from 'ggstance'.
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
#' @param integral.fun function on $x$ and $y$.
#' @param ypos.fixed numeric If not \code{NULL} used a constant value returned
#'   in \code{y}.
#'
#' @return A data frame with one row for each waveband object in the argument to
#'   \code{w.band}. Wavebeand outside the range of the spectral data are trimmed
#'   or discarded.
#'
#' @section Computed variables: What it is named integral below is the result of
#'   appying \code{integral.fun}, with default \code{integrate_xy}. \describe{
#'   \item{x}{w.band-midpoint} \item{xmin}{w.band minimum} \item{xmax}{w.band
#'   maximum} \item{ymin}{data$y minimum} \item{ymax}{data$y maximum}
#'   \item{yint}{data$y integral for the range of \code{w.band}}
#'   \item{ymean}{yint divided by spread(w.band)} \item{y}{ypos.fixed or mean of
#'   data} \item{wb.color}{color of the w.band} \item{wb.name}{label of w.band}
#'   }
#'
#' @section Default aesthetics: Set by the statistic and available to geoms.
#'   \describe{ \item{xmin}{..xmin..} \item{xmax}{..xmax..}
#'   \item{yintercept}{..ymean..} \item{height}{(..ymax.. - ..ymin..) * 2e-2}
#'   \item{color}{..wb.color..} }
#'
#' @section Required aesthetics: Required by the statistic and need to be set
#'   with \code{aes()}. \describe{ \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity} }
#'
#' @examples
#' library(photobiology)
#' library(photobiologyWavebands)
#' library(ggplot2)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_hbar(w.band = VIS_bands(), size = 1) +
#'   scale_color_identity() +
#'   theme_bw()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_hbar(w.band = PAR(), size = 1) +
#'   scale_color_identity() +
#'   theme_bw()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_hbar(w.band = PAR(), size = 1, ypos.fixed = 0) +
#'   scale_color_identity() +
#'   theme_bw()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_hbar(w.band = CIE(), size = 1) +
#'   scale_color_identity() +
#'   theme_bw()
#'
#' @note If the argument passed to \code{w.band} is a BSWF it is silently
#'   converted to a wavelength range and the average of spectral values without
#'   any weighting is returned as default value for \code{y}.
#'
#' @export
#' @family stats functions
#'
stat_wb_hbar <- function(mapping = NULL,
                         data = NULL,
                         geom = "errorbarh",
                         w.band = NULL,
                         integral.fun = integrate_xy,
                         ypos.fixed = NULL,
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbHbar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  integral.fun = integral.fun,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbHbar <-
  ggplot2::ggproto("StatWbHbar", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            integral.fun,
                                            ypos.mult,
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
                                            high.limit = range[2])
                       yint.tmp <- integral.fun(mydata$x, mydata$y)
                       ymean.tmp <- yint.tmp / spread(wb)
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(mydata$x),
                                                    xmin = min(wb),
                                                    xmax = max(wb),
                                                    ymin = min(data$y),
                                                    ymax = max(data$y),
                                                    yint = yint.tmp,
                                                    ymean = ymean.tmp,
                                                    wb.color = color(wb),
                                                    wb.name = labels(wb)$label)
                                         )
                     }
                     if (is.null(ypos.fixed)) {
                       integ.df$y <- with(integ.df, ymin + (ymean - ymin))
                     } else {
                       integ.df$y <- ypos.fixed
                     }
                     integ.df
                   },
                   default_aes = ggplot2::aes(xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              yintercept = ..ymean..,
                                              height = (..ymax.. - ..ymin..) * 2e-2,
                                              color = ..wb.color..),
                   required_aes = c("x", "y")
  )

