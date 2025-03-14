#' Draw colour boxes for wavebands
#'
#' \code{stat_wb_box} plots boxes corresponding to wavebands, by default located
#' slightly above the peak of the spectrum. Sets suitable default aesthetics for
#' \code{geom_rect()}. \strong{\code{x}-scale transformations and axis
#'   flipping are currently not supported}.
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
#' @param by.group logical flag If TRUE repeated identical layers are added
#'   for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, a single layer is added per panel.
#' @param w.band a waveband object or a list of waveband objects or numeric
#'   vector of at least length two.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param ypos.mult numeric Multiplier constant used to compute returned
#'   \code{y} values. This is numerically similar to using npc units, but values
#'   larger than one expand the plotting area.
#' @param ypos.fixed numeric If not \code{NULL} used a constant value returned
#'   in \code{y}.
#' @param box.height numeric The height of the box as a fraction of the range of
#'   $y$. This is similar to using npc units.
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
#'   \item{ymin}{box bottom}
#'   \item{ymax}{box top}
#'   \item{y}{ypos.fixed or top of data, adjusted by \code{ypos.mult}}
#'   \item{wb.color}{color of the w.band}
#'   \item{wb.name}{label of w.band}
#'   \item{BW.color}{\code{black_or_white(wb.color)}}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{xmin}{stat(wb.xmin)}
#'   \item{xmax}{stat(wb.xmax)}
#'   \item{ymin}{stat(ymin)}
#'   \item{ymax}{stat(ymax)}
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
#' @details By default \code{stat_wb_box()} uses a panel function and ignores
#'   grouping as needed for annotation of layers supporting free axis scales.
#'   Passing \code{by.group = TRUE} as argument changes this behaviour adding
#'   the same layer repeatedly for each group as needed for constructing
#'   animated plots with functions from package 'gganimate'.
#'
#'   The value returned as default value for \code{y} is based on the y-range of
#'   spectral values for the whole data set.
#'
#'   As colours are returned as RGB colour definitions, depending on the
#'   geometry used the use of \code{\link[ggplot2]{scale_fill_identity}}
#'   and/or \code{\link[ggplot2]{scale_colour_identity}} can be necessary for
#'   the correct colours to be displayed in the plot.
#'
#' @note As only one colour scale can exist within a \code{"gg"} object, using
#'   this scale prevents the mapping to the colour aesthetic of
#'   factors in \code{data} to create a grouping.
#'
#' @seealso \code{\link[photobiology]{fast_color_of_wb}}, which is used in the
#'   implementation.
#
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
#' @export
#' @family stats functions
#'
stat_wb_box <- function(mapping = NULL,
                        data = NULL,
                        geom = "rect",
                        position = "identity",
                        ...,
                        by.group = FALSE,
                        w.band = NULL,
                        chroma.type = "CMF",
                        ypos.mult = 1.07,
                        ypos.fixed = NULL,
                        box.height = 0.06,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (by.group) {
    ggplot2::layer(
      stat = StatWbBoxG, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(w.band = w.band,
                    chroma.type = chroma.type,
                    ypos.mult = ypos.mult,
                    ypos.fixed = ypos.fixed,
                    box.height = box.height,
                    na.rm = na.rm,
                    ...)
    )
  } else {
    ggplot2::layer(
      stat = StatWbBox, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(w.band = w.band,
                    chroma.type = chroma.type,
                    ypos.mult = ypos.mult,
                    ypos.fixed = ypos.fixed,
                    box.height = box.height,
                    na.rm = na.rm,
                    ...)
    )
  }
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
compute_wb_box <- function(data,
                           scales,
                           w.band,
                           chroma.type,
                           ypos.mult,
                           ypos.fixed,
                           box.height) {
  wl.range <- range(data$x)
  if (length(w.band) == 0) {
    w.band <- waveband(wl.range)
  }
  if (is.any_spct(w.band) ||
      (is.numeric(w.band) && length(stats::na.omit(w.band)) >= 2)) {
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
    wb.color <- photobiology::fast_color_of_wb(wb, chroma.type = chroma.type)
    integ.df <- rbind(integ.df,
                      data.frame(x = midpoint(mydata$x),
                                 wb.xmin = min(wb),
                                 wb.xmax = max(wb),
                                 wb.ymin = min(data[["y"]]),
                                 wb.ymax = max(data[["y"]]),
                                 wb.color = wb.color,
                                 wb.name = labels(wb)[["label"]],
                                 BW.color = black_or_white(wb.color))
    )
  }
  if (is.null(ypos.fixed)) {
    integ.df[["y"]] <- with(integ.df, wb.ymin + (wb.ymax - wb.ymin) * ypos.mult)
  } else {
    integ.df[["y"]] <- ypos.fixed
  }
  integ.df[["ymax"]] <- with(integ.df, y + (wb.ymax - wb.ymin) * box.height / 2)
  integ.df[["ymin"]] <- with(integ.df, y - (wb.ymax - wb.ymin) * box.height / 2)

  integ.df
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbBox <-
  ggplot2::ggproto("StatWbBox", ggplot2::Stat,
                   compute_panel = compute_wb_box,
                   default_aes = ggplot2::aes(xmin = after_stat(wb.xmin),
                                              xmax = after_stat(wb.xmax),
                                              ymin = after_stat(ymin),
                                              ymax = after_stat(ymax),
                                              fill = after_stat(wb.color)),
                   required_aes = c("x", "y")
  )

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbBoxG <-
  ggplot2::ggproto("StatWbBoxG", ggplot2::Stat,
                   compute_group = compute_wb_box,
                   default_aes = ggplot2::aes(xmin = after_stat(wb.xmin),
                                              xmax = after_stat(wb.xmax),
                                              ymin = after_stat(ymin),
                                              ymax = after_stat(ymax),
                                              fill = after_stat(wb.color)),
                   required_aes = c("x", "y")
  )
