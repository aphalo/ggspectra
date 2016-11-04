#' Label ranges under spectral curve.
#'
#' \code{stat_wb_label} computes computes the center of a waveband. Sets
#' suitable default aestheics for "rect", "hline", "vline", "text" and "label"
#' geoms displaying "boundaries" and "names" of wavebands.
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
#' @param label.fmt character string giving a format definition for formating
#'   the name of the waveband.
#'   \code{\link{sprintf}}.
#' @param ypos.fixed numeric If not \code{NULL} used a constant value returned
#'   in \code{y}.
#'
#' @return A data frame with one row for each waveband object in the argument
#' to \code{w.band}. Wavebeand outside the range of the spectral data are
#' trimmed or discarded.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{w.band-midpoint}
#'   \item{xmin}{w.band minimum}
#'   \item{xmax}{w.band maximum}
#'   \item{y}{ypos.fixed or zero}
#'   \item{wb.color}{color of the w.band}
#'   \item{wb.name}{label of w.band}
#'   \item{wb.label}{formatted wb.name}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{label}{..wb.label..}
#'   \item{x}{..x..}
#'   \item{xmin}{..xmin..}
#'   \item{xmax}{..xmax..}
#'   \item{fill}{..wb.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#' }
#'
#' @examples
#' library(photobiology)
#' library(photobiologyWavebands)
#' library(ggplot2)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_label(w.band = VIS(), geom = "rect", ymin = -0.04, ymax = 0,
#'   color = "black", fill = "white") +
#'   stat_wb_label(w.band = VIS(), y = -0.02)
#'
#' @export
#' @family stats functions
#'
stat_wb_label <- function(mapping = NULL, data = NULL, geom = "text",
                       w.band = NULL,
                       label.fmt = "%s",
                       ypos.fixed = 0,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWbLabel, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  label.fmt = label.fmt,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbLabel <-
  ggplot2::ggproto("StatWbLabel", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            label.fmt,
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
                     integ.df <- data.frame()
                     for (wb in w.band) {
                       if (is.numeric(wb)) { # user supplied a list of numeric vectors
                         wb <- waveband(wb)
                       }
                       range <- range(wb)
                       integ.df <- rbind(integ.df,
                                         data.frame(x = midpoint(wb),
                                                    xmin = min(wb),
                                                    xmax = max(wb),
                                                    wb.color = color(wb),
                                                    wb.name = labels(wb)$label)
                                         )
                     }
                     if (is.null(ypos.fixed)) {
                       integ.df$y <- 0
                     } else {
                       integ.df$y <- ypos.fixed
                     }
                     integ.df$wb.label <- sprintf(label.fmt, integ.df$wb.name)
#                     print(integ.df)
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = ..wb.label..,
                                              x = ..x..,
                                              xmin = ..xmin..,
                                              xmax = ..xmax..,
                                              fill = ..wb.color..),
                   required_aes = c("x")
  )
