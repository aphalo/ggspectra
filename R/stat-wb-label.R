#' Label ranges under spectral curve.
#'
#' \code{stat_wb_label} computes the center of a waveband. Sets suitable default
#' aesthetics for "text" and "label" geoms displaying "boundaries" and "names"
#' of wavebands. \strong{\code{x}-scale transformations and axis flipping are
#' currently not supported}.
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
#'   \item{wb.xmin}{w.band minimum}
#'   \item{wb.xmax}{w.band maximum}
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
#'   \item{xmin}{..wb.xmin..}
#'   \item{xmax}{..wb.xmax..}
#'   \item{fill}{..wb.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#' }
#'
#' @details By default \code{stat_wb_label()} uses a panel function and ignores
#'   grouping as needed for annotation of layers supporting free axis scales.
#'   Passing \code{by.group = TRUE} as argument changes this behaviour adding
#'   the same layer repeatedly for each group as needed for constructing
#'   animated plots with functions from package 'gganimate'.
#'
#'   As colours are returned as RGB colour definitions, depending on the
#'   geometry used the use of \code{\link[ggplot2]{scale_fill_identity}}
#'   and/or \code{\link[ggplot2]{scale_colour_identity}} will be necessary for
#'   the correct colours to be displayed in the plot.
#'
#' @note As only one colour scale can exist within a \code{"gg"} object, using
#'   this scale prevents the mapping to the colour aesthetic of
#'   factors in \code{data} to create a grouping.
#'
#' @seealso \code{\link[photobiology]{fast_color_of_wb}}, which is used in the
#'   implementation.
#'
#' @examples
#'
#' library(photobiologyWavebands)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_box(w.band = VIS(), ymin = -0.04, ymax = 0,
#'   color = "black", fill = "white") +
#'   stat_wb_label(w.band = VIS(), ypos.fixed = -0.02, color = "black")
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wb_hbar(w.band = PAR(), ypos.fixed = 0, linewidth = 1) +
#'   stat_wb_label(aes(color = after_stat(wb.color)),
#'                 w.band = PAR(), ypos.fixed = +0.025) +
#'   scale_color_identity()
#'
#' @export
#' @family stats functions
#'
stat_wb_label <- function(mapping = NULL,
                          data = NULL,
                          geom = "text",
                          position = "identity",
                          ...,
                          by.group = FALSE,
                          w.band = NULL,
                          chroma.type = "CMF",
                          label.fmt = "%s",
                          ypos.fixed = 0,
                          na.rm = TRUE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  if (by.group) {
    ggplot2::layer(
      stat = StatWbLabelG, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(w.band = w.band,
                    chroma.type = chroma.type,
                    label.fmt = label.fmt,
                    ypos.fixed = ypos.fixed,
                    na.rm = na.rm,
                    ...)
    )
  } else {
    ggplot2::layer(
      stat = StatWbLabel, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(w.band = w.band,
                    chroma.type = chroma.type,
                    label.fmt = label.fmt,
                    ypos.fixed = ypos.fixed,
                    na.rm = na.rm,
                    ...)
    )
  }
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
compute_wb_label <- function(data,
                             scales,
                             w.band,
                             chroma.type,
                             label.fmt,
                             ypos.fixed) {
  x.range <- range(data[["x"]])
  if (length(w.band) == 0) {
    w.band <- waveband(x.range)
  }
  if (is.any_spct(w.band) ||
      (is.numeric(w.band) && length(stats::na.omit(w.band)) >= 2)) {
    w.band <- waveband(range(w.band, na.rm = TRUE))
  }
  if (!is.list(w.band) || is.waveband(w.band)) {
    w.band <- list(w.band)
  }
  w.band <- trim_wl(w.band, x.range)
  integ.df <- data.frame()
  for (wb in w.band) {
    if (is.numeric(wb)) { # user supplied a list of numeric vectors
      wb <- waveband(wb)
    }
    range <- range(wb)
    colors <- fast_color_of_wb(wb, chroma.type = chroma.type)
    integ.df <-
      rbind(integ.df,
            tibble::tibble(x = midpoint(wb),
                           wb.xmin = min(wb),
                           wb.xmax = max(wb),
                           wb.name = labels(wb)$label,
                           wb.color = colors,
                           BW.color = black_or_white(colors))
      )
  }
  if (is.null(ypos.fixed)) {
    integ.df[["y"]] <- 0
  } else {
    integ.df[["y"]] <- ypos.fixed
  }
  integ.df$wb.label <- sprintf(label.fmt, integ.df$wb.name)
  #                     print(integ.df)
  integ.df
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbLabel <-
  ggplot2::ggproto("StatWbLabel", ggplot2::Stat,
                   compute_panel = compute_wb_label,
                   default_aes = ggplot2::aes(label = after_stat(wb.label),
                                              y = after_stat(y),
                                              xmin = after_stat(wb.xmin),
                                              xmax = after_stat(wb.xmax),
                                              fill = after_stat(wb.color),
                                              color = after_stat(BW.color)),
                   required_aes = c("x")
  )

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatWbLabelG <-
  ggplot2::ggproto("StatWbLabelG", ggplot2::Stat,
                   compute_group = compute_wb_label,
                   default_aes = ggplot2::aes(label = after_stat(wb.label),
                                              y = after_stat(y),
                                              xmin = after_stat(wb.xmin),
                                              xmax = after_stat(wb.xmax),
                                              fill = after_stat(wb.color),
                                              color = after_stat(BW.color)),
                   required_aes = c("x")
  )
