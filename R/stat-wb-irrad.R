#' Integrate irradiance for wavebands.
#'
#' \code{stat_wb_irrad} computes areas under a curve.
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
#' @param unit.in character One of "photon","quantum" or "energy"
#' @param time.unit character or lubridate::duration
#' @param label.qty character
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
#' What it is named integral below is the result of appying \code{irrad},
#' \code{e_irrad} or \code{q_irrad} to the data.
#' \describe{
#'   \item{y.label}{yeff multiplied by \code{label.mult} and formatted
#'   according to \code{label.fmt}}
#'   \item{x}{w.band-midpoint}
#'   \item{wb.xmin}{w.band minimum}
#'   \item{wb.xmax}{w.band maximum}
#'   \item{wb.ymin}{data$y minimum}
#'   \item{wb.ymax}{data$y maximum}
#'   \item{wb.yeff}{weighted irradiance if \code{w.band} describes a BSWF}
#'   \item{wb.yint}{not weighted irradiance for the range of \code{w.band}}
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
#'
#' # using defaults for energy irradiance in W m-2
#' ggplot(sun.spct) +
#'   stat_wb_column(w.band = PAR(), alpha = 0.5) +
#'   stat_wb_e_irrad(w.band = PAR(), ypos.fixed = 0.32) +
#'   geom_line() +
#'   scale_fill_identity() + scale_color_identity()
#'
#' # using defaults for photon irradiance in umol m-2 s-1
#' ggplot(sun.spct, unit.out = "photon") +
#'   stat_wb_column(w.band = PAR(), alpha = 0.5) +
#'   stat_wb_q_irrad(w.band = PAR(), ypos.fixed = 1.5e-6, label.mult = 1e6) +
#'   geom_line() +
#'   scale_fill_identity() + scale_color_identity()
#'
#' # modify label format and position
#' ggplot(sun.spct) +
#'   stat_wb_column(w.band = VIS_bands(), alpha = 0.7) +
#'   stat_wb_e_irrad(w.band = VIS_bands(),
#'                   angle = 90, size = 3, hjust = "left",
#'                   label.fmt = "%2.0f~~W~m^{-2}", parse = TRUE,
#'                   ypos.fixed = 0.1) +
#'   geom_line() +
#'   scale_fill_identity() + scale_color_identity()
#'
#' # Changing label mapping
#' ggplot(sun.spct) +
#'   stat_wb_column(w.band = VIS_bands(), alpha = 0.5) +
#'   stat_wb_e_irrad(w.band = VIS_bands(),
#'                label.fmt = "%.2f",
#'                angle = 90, color = "black", ypos.fixed = 0.1,
#'                hjust = "left", size = 3,
#'                mapping = aes(label = after_stat(paste(wb.name, ": ",
#'                                                 signif(wb.yint, 3),
#'                                                 sep = "")))) +
#'   geom_line() +
#'   scale_fill_identity() + scale_color_identity() +
#'   theme_bw()
#'
#' @export
#' @family stats functions
#'
stat_wb_irrad <- function(mapping = NULL,
                          data = NULL,
                          geom = "text",
                          position = "identity",
                          ...,
                          w.band = NULL,
                          time.unit,
                          unit.in,
                          label.qty = "total",
                          label.mult = 1,
                          chroma.type = "CMF",
                          label.fmt = "%.3g",
                          ypos.mult = 1.07,
                          ypos.fixed = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatWbIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
                  label.mult = label.mult,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  ypos.mult = ypos.mult,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_wb_irrad
#' @export
stat_wb_e_irrad <- function(mapping = NULL,
                            data = NULL,
                            geom = "text",
                            position = "identity",
                            ...,
                            w.band = NULL,
                            time.unit = "second",
                            unit.in = "energy",
                            label.qty = "total",
                            label.mult = 1,
                            chroma.type = "CMF",
                            label.fmt = "%.3g",
                            ypos.mult = 1.07,
                            ypos.fixed = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    ggplot2::layer(
    stat = StatWbIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
                  label.mult = label.mult,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  ypos.mult = ypos.mult,
                  ypos.fixed = ypos.fixed,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_wb_irrad
#' @export
stat_wb_q_irrad <- function(mapping = NULL,
                            data = NULL,
                            geom = "text",
                            position = "identity",
                            ...,
                            w.band = NULL,
                            time.unit = "second",
                            unit.in = "photon",
                            label.qty = "total",
                            label.mult = 1,
                            chroma.type = "CMF",
                            label.fmt = "%.3g",
                            ypos.mult = 1.07,
                            ypos.fixed = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatWbIrrad, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(w.band = w.band,
                  time.unit = time.unit,
                  unit.in = unit.in,
                  label.qty = label.qty,
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
StatWbIrrad <-
  ggplot2::ggproto("StatWbIrrad", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            w.band,
                                            time.unit,
                                            unit.in,
                                            label.qty,
                                            label.mult,
                                            chroma.type,
                                            label.fmt,
                                            ypos.mult,
                                            ypos.fixed) {
                     if (length(w.band) == 0) {
                       w.band <- waveband(data[["x"]])
                     }
                     if (is.any_spct(w.band) ||
                         (is.numeric(w.band) && length(stats::na.omit(w.band)) >= 2)) {
                       w.band <- waveband(range(w.band, na.rm = TRUE))
                     }
                     if (!is.list(w.band) || is.waveband(w.band)) {
                       w.band <- list(w.band)
                     }
                     w.band <- trim_wl(w.band, data[["x"]])
                     if (unit.in == "energy") {
                       tmp.spct <- source_spct(w.length = data[["x"]], s.e.irrad = data[["y"]],
                                               time.unit = time.unit)
                     } else if (unit.in %in% c("photon", "quantum")) {
                       tmp.spct <- source_spct(w.length = data[["x"]], s.q.irrad = data[["y"]],
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
                                                    wb.xmin = min(wb),
                                                    wb.xmax = max(wb),
                                                    wb.yeff = yeff.tmp,
                                                    wb.yint = yint.tmp,
                                                    wb.ymax = max(data[["y"]]),
                                                    wb.ymin = min(data[["y"]]),
                                                    wb.ymean = ymean.tmp,
                                                    wb.color = fast_color_of_wb(wb, chroma.type = chroma.type),
                                                    wb.name = labels(wb)[["label"]],
                                                    BW.color = black_or_white(fast_color_of_wb(wb, chroma.type = chroma.type)))
                                         )
                     }

                     if (is.null(ypos.fixed)) {
                       integ.df[["y"]] <- with(integ.df, wb.ymin + (wb.ymax - wb.ymin) * ypos.mult)
                     } else {
                       integ.df[["y"]] <- ypos.fixed
                     }
                     # we need to avoid too many digits after decimal point
                     # we apply the format after rounding
                     digits <- 5 - floor(log10(max(integ.df[["wb.yeff"]])))
                     integ.df[["y.label"]] <-
                       sprintf(label.fmt, round(integ.df[["wb.yeff"]], digits) * label.mult)
                     integ.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(y.label),
                                              xmin = after_stat(wb.xmin),
                                              xmax = after_stat(wb.xmax),
                                              ymin = after_stat(y) - (after_stat(wb.ymax) - after_stat(wb.ymin)) * 0.03,
                                              ymax = after_stat(y) + (after_stat(wb.ymax) - after_stat(wb.ymin)) * 0.03,
                                              yintercept = after_stat(wb.ymean),
                                              fill = after_stat(wb.color),
                                              color = after_stat(BW.color)),
                   required_aes = c("x", "y")
  )

