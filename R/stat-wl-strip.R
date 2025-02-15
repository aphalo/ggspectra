#' Calculate colours from wavelength.
#'
#' \code{stat_wl_strip} computes color definitions according to human vision and
#' by default plots a narrow, guide-like colour gradient strip based on
#' wavelength. \strong{\code{x}-scale transformations and axis flipping are
#' currently not supported}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
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
#' @param w.band waveband object or a list of such objects or NULL.
#' @param length.out The number of steps to use to simulate a continuous range
#'   of colours when w.band == NULL.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#'
#' @return generic_spect object with new \code{x} values plus other computed
#'   variables described below.
#'
#' @section Computed variables:
#' \describe{
#'    \item{x}{(w.low + wl.high) / 2}
#'    \item{wl.low}{boundary of waveband}
#'    \item{wl.high}{boundary of waveband}
#'    \item{wl.color}{color corresponding to wavelength}
#'    \item{wb.color}{color corresponding to waveband}
#'    \item{wb.name}{label of w.band}
#' }
#'
#' @section Default aesthetics:
#' Set by the statistic and available to geoms.
#' \describe{
#'   \item{x}{..x..}
#'   \item{label}{as.character(..wb.f..)}
#'   \item{xmin}{..wl.low..}
#'   \item{xmax}{..wl.high..}
#'   \item{fill}{..wb.color..}
#' }
#'
#' @section Required aesthetics:
#' Required by the statistic and need to be set with \code{aes()}.
#' \describe{
#'   \item{x}{numeric, wavelength in nanometres}
#' }
#'
#' @details By default \code{stat_wl_strip()} uses a panel function and ignores
#'   grouping as needed for annotation of layers supporting free axis scales.
#'   Passing \code{by.group = TRUE} as argument changes this behaviour adding
#'   the same layer repeatedly for each group as needed for constructing
#'   animated plots with functions from package 'gganimate'.
#'
#'   Function \code{wl_guide()} is a conveneince wrapper on
#'   \code{stat_wl_strip()} that also adds the required
#'   \code{scale_fill_identity()}.
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
#' @seealso \code{\link[photobiology]{color_of}} and
#'   \code{\link[photobiology]{fast_color_of_wl}}, which are used in the
#'   implementation.
#'
#' @examples
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_wl_strip(ymax = -0.02, ymin = -0.04) +
#'   scale_fill_identity()
#'
#' # on some graphic devices the output may show spurious vertical lines
#' ggplot(sun.spct) +
#'   wl_guide(alpha = 0.33, color = NA) +
#'   geom_line()
#'
#' @export
#' @family stats functions
#'
stat_wl_strip <- function(mapping = NULL,
                          data = NULL,
                          geom = "rect",
                          position = "identity",
                          ...,
                          by.group = FALSE,
                          w.band = NULL,
                          length.out = 150,
                          chroma.type = "CMF",
                          na.rm = TRUE,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {
  if (by.group) {
    ggplot2::layer(
      stat = StatColorGuideG, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(chroma.type = chroma.type,
                    w.band = w.band,
                    length.out = length.out,
                    na.rm = na.rm,
                    ...)
    )
  } else {
    ggplot2::layer(
      stat = StatColorGuide, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(chroma.type = chroma.type,
                    w.band = w.band,
                    length.out = length.out,
                    na.rm = na.rm,
                    ...)
    )
  }
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
colour_guide_function <- function(data,
                                  scales,
                                  w.band,
                                  length.out,
                                  chroma.type) {
  if (length(w.band) == 0) {
    w.band <- split_bands(range(data[["x"]]),
                          length.out = length.out)
  } else {
    w.band <- trim_waveband(w.band = w.band,
                            range = data[["x"]],
                            trim = TRUE)
  }
  fast_wb2rect_df(w.band = w.band, chroma.type = chroma.type)
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatColorGuide <-
  ggplot2::ggproto("StatColorGuide", ggplot2::Stat,
                   compute_panel = colour_guide_function,
                   default_aes =
                     ggplot2::aes(xmin = after_stat(wl.low),
                                  xmax = after_stat(wl.high),
                                  fill = after_stat(wb.color)),
                   required_aes = c("x")
  )

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatColorGuideG <-
  ggplot2::ggproto("StatColorGuideG", ggplot2::Stat,
                   compute_group = colour_guide_function,
                   default_aes =
                     ggplot2::aes(xmin = after_stat(wl.low),
                                  xmax = after_stat(wl.high),
                                  fill = after_stat(wb.color)),
                   required_aes = c("x")
  )

#' @rdname stat_wl_strip
#' @param ymin,ymax numeric used as aesthetics for plotting the guide.
#'
#' @export
#'
wl_guide <- function(mapping = NULL,
                     data = NULL,
                     position = "identity",
                     ...,
                     by.group = FALSE,
                     chroma.type = "CMF",
                     w.band = NULL,
                     length.out = 150,
                     ymin = -Inf,
                     ymax = Inf,
                     na.rm = FALSE,
                     show.legend = FALSE,
                     inherit.aes = TRUE) {
  list(stat_wl_strip(mapping = mapping,
                     data = data,
                     geom = "rect",
                     w.band = w.band,
                     chroma.type = chroma.type,
                     length.out = length.out,
                     by.group = by.group,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes,
                     ymin = ymin,
                     ymax = ymax,
                     ...),
       scale_fill_identity()
  )
}

#' Colours from wavebands
#'
#' Compute colours for a list of narrow wavebands or a range of wavelengths.
#'
#' @param w.band waveband or list of waveband objects. The waveband(s) determine
#'   the wavelengths in variable \code{w.length} of the returned spectrum.
#' @param chroma.type character telling whether "CMF", "CC", or "both" should be
#'   returned for human vision, or an object of class \code{chroma_spct} for any
#'   other trichromic visual system.
#' @param simplify logical Flag indicating whether to merge neighbouring
#'   rectangles of equal color.
#'
#' @return A data frame with columns "x", "y", "w.length", "wb.color",
#' "wl.high", "wl.low".
#'
#' @details Function \code{fast_wb2rect_df()} computes colours for
#'   wavebands based on the midpoint wavelength and uses vectorization when
#'   possible. It always returns color definitions with short names. The purpose
#'   of merging of rectangles is to speed up rendering and to reduce the size of
#'   vector graphics output. This function should be used with care as the color
#'   definitions returned are only approximate and original waveband names can
#'   be lost.
#'
#'   Function \code{fast_wb2rect_df()} intended use is when colour definitions
#'   are needed in a simple data frame as in the compute functions of
#'   statistics that return colours.
#'
#' @keywords internal
#'
fast_wb2rect_df <- function(w.band,
                            chroma.type = "CMF",
                            simplify = TRUE) {
  if (!length(w.band)) {
    warning("Misssing/empty 'w.band' definition.")
    return(data.frame())
  }
  if (photobiology::is.waveband(w.band)) {
    w.band <- list(w.band)
  }
  wbs.number <- length(w.band) # number of wavebands in list
  wbs.wds <- sapply(w.band, photobiology::expanse)
  if (any(wbs.wds >= 10)) { # waveband wider than 10 nm
    # we compute colours based on the whole waveband
    wbs.wl.mid <- wbs.wl.high <- wbs.wl.low <- numeric(wbs.number)
    wbs.rgb <- character(wbs.number)
    i <- 0L
    for (wb in w.band) {
      i <- i + 1L
      wbs.wl.low[i] <- photobiology::wl_min(wb)
      wbs.wl.mid[i] <- photobiology::wl_midpoint(wb)
      wbs.wl.high[i] <- photobiology::wl_max(wb)
      wbs.rgb[i] <- photobiology::color_of(wb, type = chroma.type)[1]
    }
  } else { # waveband 10 nm wide or narrower
    # we only compute the colour at the center wavelength of each waveband
    wbs.wl.mid <- sapply(w.band, photobiology::wl_midpoint)
    wbs.wl.high <- sapply(w.band, photobiology::wl_max)
    wbs.wl.low <- sapply(w.band, photobiology::wl_min)
    wbs.rgb <- photobiology::fast_color_of_wl(wbs.wl.mid)
    if (simplify) {
      # merge neighbouring rectangles that share the same colour definition
      # and recompute wl at midpoint
      rgb.rle <- rle(wbs.rgb)
      new.nrow <- length(rgb.rle[["lengths"]])
      runs.ends <- cumsum(rgb.rle[["lengths"]])
      runs.start <- c(1L, runs.ends[-new.nrow] + 1L)
      wbs.wl.low <- wbs.wl.low[runs.start]
      wbs.wl.high <- wbs.wl.high[runs.ends]
      wbs.wl.mid <- wbs.wl.low + (wbs.wl.high - wbs.wl.low) / 2
      wbs.rgb <- wbs.rgb[runs.ends]
    }
  }
  data.frame(x = wbs.wl.mid,
             y = 0,
             w.length = wbs.wl.mid,
             wl.color = wbs.rgb,
             wb.color = wbs.rgb,
             wl.high = wbs.wl.high,
             wl.low = wbs.wl.low)
}
