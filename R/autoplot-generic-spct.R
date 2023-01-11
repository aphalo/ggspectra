#' Create a complete ggplot for generic spectral data.
#'
#' This function returns a ggplot object with an annotated plot of a
#' generic_spct object.
#'
#' Note that scales are expanded so as to make space for the annotations. The
#' object returned is a ggplot object, and can be further manipulated. When spct
#' has more than one column with spectral data, each of these columns is
#' normalized individually.
#'
#' @param spct a generic_spct object
#' @param y.name,ymin.name,ymax.name character Names of the columns to be mapped
#'   to the y aesthetic.
#' @param ylab character or expression The y-axis label.
#' @param w.band list of waveband objects
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm)
#' @param pc.out logical, if TRUE use percents instead of fraction of one
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels, one of "mean", "total", "contribution", and "relative".
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element.
#' @param wls.target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings "half.maximum" and "half.range" are also accepted
#'   as arguments. A list with \code{numeric} and/or \code{character} values is
#'   also accepted.
#' @param annotations a character vector
#' @param text.size numeric size of text in the plot decorations.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried.
#' @param facets logical or integer Indicating if facets are to be created for
#'   the levels of \code{idfactor} when \code{spct} contain multiple spectra in
#'   long form.
#' @param ylim numeric y axis limits,
#' @param na.rm logical.
#' @param ... currently ignored.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
generic_plot <- function(spct,
                         y.name,
                         ymin.name,
                         ymax.name,
                         ylab,
                         w.band,
                         range,
                         label.qty,
                         span,
                         wls.target,
                         annotations,
                         text.size,
                         idfactor,
                         facets,
                         ylim,
                         na.rm,
                         ...) {
  if (!photobiology::is.generic_spct(spct)) {
    stop("generic_plot() can only plot generic_spct objects.")
  }

  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- photobiology::trim_wl(w.band, range = range(spct))
  }

  y.name <- intersect(names(spct), y.name)
  if (length(y.name) != 1L) {
    warning("Attempt to map other than one variable to y.")
    return(ggplot())
  }
  ymax.name <- intersect(names(spct), ymax.name)
  ymin.name <- intersect(names(spct), ymin.name)
  if (xor(length(ymax.name), length(ymin.name))) {
    warning("Both 'ymax.name' and 'ymin.name', or none need to be supplied")
    ymax.name <- ymin.name <- character()
  }

  # ggplot construction
  if (length(ymin.name) && length(ymax.name)) {
    plot <-
      ggplot2::ggplot(spct,
                      spct_class = "generic_spct",
                      ggplot2::aes(x = .data[["w.length"]],
                                   y = .data[[y.name]],
                                   ymax = .data[[ymax.name]],
                                   ymin = .data[[ymin.name]]))
    with.band <- TRUE
  } else {
    plot <-
      ggplot2::ggplot(spct,
                      spct_class = "generic_spct",
                      ggplot2::aes(x = .data[["w.length"]],
                                   y = .data[[y.name]]))
    with.band <- FALSE
  }

  if (with.band) {
    plot <- plot + ggplot2::geom_ribbon(fill = "grey50", alpha = 0.5)
    y.min <- min(c(spct[[ymin.name]], ylim), na.rm = TRUE)
    y.max <- max(c(spct[[ymax.name]], ylim), na.rm = TRUE)
  } else {
    y.min <- min(c(spct[[y.name]], ylim), na.rm = TRUE)
    y.max <- max(c(spct[[y.name]], ylim), na.rm = TRUE)
  }

  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

  plot <- plot + ggplot2::geom_line(na.rm = na.rm)

  if (length(annotations) != 1 || annotations != "") {

    plot <-
      plot +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_color_identity()

    plot <-
      plot + decoration(w.band = w.band,
                        y.max = y.max,
                        y.min = y.min,
                        x.max = max(spct),
                        x.min = min(spct),
                        annotations = annotations,
                        label.qty = label.qty,
                        span = span,
                        wls.target = wls.target,
                        text.size = text.size,
                        na.rm = TRUE)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.max + (y.max - y.min) * 0.25)
    x.limits <-
      c(photobiology::wl_min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- photobiology::wl_range(spct)
  }

  plot <- plot +
    ggplot2::scale_y_continuous(name = ylab,
                                limits = y.limits,
                                breaks = scales::pretty_breaks(n = 5))

  plot + scale_x_wl_continuous(limits = x.limits)

}

#' Create a complete ggplot for generic spectral data.
#'
#' This function returns a ggplot object with an annotated plot of a
#' generic_spct object.
#'
#' @details No automatic plot method is possible for objects of class \code{generic_spct}
#' as this class is meant to be mainly used only as a pure base class for
#' derivation. A method is provided for \code{generic_spct} but not for
#' \code{generic_mspct} as \code{generic_mspct} objects can contain an
#' assortment of objects including \code{generic_spct} and classes derived from
#' \code{generic_spct} making the spectra are unlikely be suitable for plotting
#' in the same ggplot.
#'
#' Contrary to other autoplot methods, the method for \code{generic_spct} does
#' not supply defaults to several of its parameters and cannot be used simply by
#' calling it with the spectrum as argument. There are also limitations on which
#' annotations are accepted. On-the-fly normalization is not supported.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @note Scales are expanded so as to make space for the annotations if these
#'   are present. The object returned is a ggplot object, and can be further
#'   manipulated.
#'
#' @param object a cps_spct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param y.name,ymin.name,ymax.name character Names of the columns to be mapped
#'   to the y aesthetic.
#' @param ylab character or expression The y-axis label.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels, one of "mean", "total", "contribution", and "relative".
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element.
#' @param wls.target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings "half.maximum" and "half.range" are also accepted
#'   as arguments. A list with \code{numeric} and/or \code{character} values is
#'   also accepted.
#' @param annotations a character vector ("summaries" is ignored). For details
#'   please see sections Plot Annotations and Title Annotations.
#' @param time.format character Format as accepted by \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param text.size numeric size of text in the plot decorations.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried.
#' @param facets logical or integer Indicating if facets are to be created for
#'   the levels of \code{idfactor} when \code{spct} contain multiple spectra in
#'   long form.
#' @param ylim numeric y axis limits,
#' @param object.label character The name of the object being plotted.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{generic_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}},
#'   \code{\link[ggplot2]{scale_continuous}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#'
#' sun.generic_spct <- as.generic_spct(sun.spct)
#' autoplot(sun.generic_spct, y.name = "s.q.irrad")
#' autoplot(sun.generic_spct, y.name = "s.q.irrad",
#'          annotations = "")
#' autoplot(sun.generic_spct, y.name = "s.q.irrad",
#'          annotations = "title:objt:when",
#'          ylab = s.q.irrad_label(unit.exponent = 0))
#' autoplot(sun.generic_spct, y.name = "s.e.irrad",
#'          annotations = "colour.guide")
#' autoplot(sun.generic_spct, y.name = "s.q.irrad",
#'          ylim = c(-1e-6, 4e-6))
#'
#' @keywords internal
#'
#' @export
#'
autoplot.generic_spct <-
    function(object,
             ...,
             y.name,
             ymin.name = NULL,
             ymax.name = NULL,
             ylab = ggplot2::waiver(),
             w.band = getOption("photobiology.plot.bands",
                                default = list(UVC(), UVB(), UVA(), PhR())),
             range = NULL,
             label.qty = "none",
             span = NULL,
             wls.target = "HM",
             annotations = NULL,
             time.format = "",
             tz = "UTC",
             text.size = 2.5,
             idfactor = NULL,
             facets = FALSE,
             ylim = c(NA, NA),
             object.label = deparse(substitute(object)),
             na.rm = TRUE) {

      force(object.label)

      annotations.default <-
        getOption("photobiology.plot.annotations",
                  default = c("boxes", "labels", "colour.guide"))
      annotations <- decode_annotations(annotations,
                                        annotations.default)
      if (length(w.band) == 0) {
        if (is.null(range)) {
          w.band <- photobiology::waveband(object)
        } else if (photobiology::is.waveband(range)) {
          w.band <- range
        } else {
          w.band <- photobiology::waveband(range, wb.name = "Total")
        }
      }

      generic_plot(spct = object,
                   y.name,
                   ymin.name = ymin.name,
                   ymax.name = ymax.name,
                   ylab = ylab,
                   w.band = w.band,
                   range = range,
                   label.qty = label.qty,
                   span = span,
                   wls.target = wls.target,
                   annotations = annotations,
                   text.size = text.size,
                   idfactor = idfactor,
                   facets = facets,
                   ylim = ylim,
                   na.rm = na.rm,
                   ...) +
        autotitle(object = object,
                  time.format = time.format,
                  tz = tz,
                  object.label = object.label,
                  annotations = annotations)
    }

#' @rdname autoplot.generic_spct
#'
#' @export
#'
autoplot.generic_mspct <- function(object, ...) {
  warning("No specialized 'autoplot()' method exist for objects of class generic_mspct.")
  ggplot()
}
