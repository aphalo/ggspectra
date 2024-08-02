#' Create a complete ggplot for raw detector-counts spectra.
#'
#' Plot method for spectra expressed as raw detector counts.
#'
#' This function returns a ggplot object with an annotated plot of a
#' raw_spct object.
#'
#' Note that scales are expanded so as to make space for the annotations. The
#' object returned is a ggplot objects, and can be further manipulated. When
#' spct has more than one column with spectral data, each of these columns is
#' normalized individually.
#'
#' @param spct a raw_spct object.
#' @param w.band list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param pc.out logical, if TRUE use percents instead of fraction of one.
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
#' @param annotations a character vector.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
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
raw_plot <- function(spct,
                     w.band,
                     range,
                     pc.out,
                     label.qty,
                     span,
                     wls.target,
                     annotations,
                     geom,
                     relative,
                     text.size,
                     idfactor,
                     facets,
                     ylim,
                     na.rm,
                     ...) {
  if (!is.raw_spct(spct)) {
    stop("raw_plot() can only plot response_spct objects.")
  }
  if (!is.null(geom) && !geom %in% c("area", "line", "spct")) {
    warning("'geom = ", geom, "' not supported, using default instead.")
    geom <- NULL
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  # Attempt to retrieve max.counts from metadata
  linearized <- getInstrSettings(spct)[["linearized"]]
  if (!(is.null(linearized) || linearized)) {
    upper.boundary <- getInstrDesc(spct)[["max.counts"]]
    if (is.null(upper.boundary)) {
      upper.boundary <- NA_real_
    }
  } else {
    upper.boundary <- NA_real_
  }

  counts.cols <- names(spct)[grep("^counts", names(spct))]
  num.counts.cols <- length(counts.cols)
  # if individual spectra have multiple columns we force facets
  if (!as.logical(facets) && num.counts.cols > 1L && getMultipleWl(spct) > 1L) {
    message("Usings facets because spectra contain multiple scans.")
    facets <- TRUE
  }

  if (is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.counts.label <-
      expression(Pixel~~response~~k %*% N[lambda]~~("rel."))
    counts.label <- ""
  } else if (is_normalized(spct)) {
    norm.ls <- photobiology::getNormalization(spct)
    norm.wl <- round(norm.ls[["norm.wl"]], digits = 1)
    if (pc.out) {
      multiplier.label <- "%"
    } else {
      multiplier.label <- "/1"
    }
    s.counts.label <-
      bquote(Pixel~~response~~N[lambda]/N[lambda == .(norm.wl)]~~(.(multiplier.label)))
    counts.label <- ""
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.counts.label <-
      expression(Pixel~~response~~N[lambda]~~(counts))
    counts.label <- ""
  }

  if (num.counts.cols > 1L) {
    spct <- photobiology::spct_wide2long(spct = spct, idfactor = "scan")
    plot <- ggplot(spct) + aes(x = .data[["w.length"]], y = .data[["counts"]], linetype = .data[["scan"]])
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations,
                          num.columns = num.counts.cols)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  } else {
    plot <- ggplot(spct) + aes(x = .data[["w.length"]], y = .data[["counts"]])
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["counts"]] <- ifelse(spct[["counts"]] < y.min,
                               NA_real_,
                               spct[["counts"]])
  } else {
    y.min <- min(spct[["counts"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["counts"]] <- ifelse(spct[["counts"]] > y.max,
                               NA_real_,
                               spct[["counts"]])
  } else {
    y.max <- max(spct[["counts"]],
                 ifelse(is.na(upper.boundary), 0, upper.boundary - 1),
                 y.min,
                 na.rm = TRUE)
  }

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (!is.null(upper.boundary) && is.finite(upper.boundary)) {
      if (y.max >= upper.boundary) {
        plot <- plot + geom_hline(yintercept = upper.boundary,
                                  linetype = "dashed", colour = "red")
      } else {
        plot <- plot + geom_hline(yintercept = upper.boundary,
                                  linetype = "dashed", colour = "black")
      }
    }
    if (y.min < -0.01 * y.max) {
      plot <- plot + geom_hline(yintercept = 0,
                                linetype = "dashed", colour = "red")
    } else if ("boundaries" %in% annotations) {
      plot <- plot + geom_hline(yintercept = 0,
                                linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = expression("Wavelength, "*lambda~(nm)), y = s.counts.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot + scale_fill_identity() + scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = counts.label,
                            text.size = text.size,
                            na.rm = TRUE)

  if (abs(y.max - 1) < 0.02 && abs(y.min) < 0.02) {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    y.breaks <- scales::pretty_breaks(n = 5)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels",
                         "summaries", "colour.guide", "reserve.space"),
                       annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- range(spct)
  }

  if (pc.out) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent,
                         breaks = y.breaks,
                         limits = y.limits)
  } else {
    plot <-
      plot + scale_y_continuous(breaks = y.breaks,
                                limits = y.limits)
  }
  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))

}


#' Plot one or more raw-detector-counts spectra.
#'
#' These methods construct a ggplot object with an annotated plot of a
#' \code{raw_spct} or a \code{raw_mspct} object.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object a raw_spct object.
#' @param unit.out character IGNORED.
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{raw_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @export
#'
#' @examples
#'
#' low_res.raw_spct <- thin_wl(white_led.raw_spct,
#'                             max.wl.step = 20,
#'                             max.slope.delta = 0.05,
#'                             col.names = "counts_3")
#' autoplot(low_res.raw_spct)
#' autoplot(low_res.raw_spct, annotations = "")
#'
#' two_leds.mspct <-
#'   raw_mspct(list("LED 1" = low_res.raw_spct,
#'                  "LED 2" = low_res.raw_spct))
#' autoplot(two_leds.mspct)
#' autoplot(two_leds.mspct, facets = 1) # one column
#'
#' @family autoplot methods
#'
autoplot.raw_spct <-
  function(object, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           unit.out = "counts",
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           label.qty = "mean",
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           geom = "line",
           time.format = "",
           tz = "UTC",
           norm = "skip",
           text.size = 2.5,
           idfactor = NULL,
           facets = FALSE,
           plot.data = "as.is",
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    if (is.null(idfactor)) {
      idfactor <- getIdFactor(object)
    }
    if (is.na(idfactor) || !is.character(idfactor)) {
      idfactor <- getMultipleWl(object) > 1L
    }

    if (plot.data != "as.is") {
      return(
        autoplot(object = subset2mspct(object),
                 w.band = w.band,
                 range = range,
                 norm = norm,
                 unit.out = unit.out,
                 pc.out = pc.out,
                 label.qty = label.qty,
                 span = span,
                 wls.target = wls.target,
                 annotations = annotations,
                 geom = geom,
                 time.format = time.format,
                 tz = tz,
                 text.size = text.size,
#                 chroma.type = chroma.type,
                 idfactor = idfactor,
                 facets = facets,
                 plot.data = plot.data,
                 ylim = ylim,
                 object.label = object.label,
                 na.rm = na.rm)
      )
    }

    force(object.label)

    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide",
                            "peaks", "boundaries"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    # avoid warning in 'photobiology' (== 0.10.10)
    if (is.character(norm) && norm == "update" && !is_normalized(object)) {
      norm <- "skip"
    }
    # normalization skipping is handled by normalize()
    object <- photobiology::normalize(x = object,
                                      range = range,
                                      norm = norm,
                                      na.rm = na.rm)
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- waveband(object)
      } else if (is.waveband(range)) {
        w.band <- range
      } else {
        w.band <- waveband(range, wb.name = "Total")
      }
    }

    raw_plot(spct = object,
             w.band = w.band,
             range = range,
             label.qty = label.qty,
             span = span,
             wls.target = wls.target,
             pc.out = pc.out,
             annotations = annotations,
             geom = geom,
             norm = norm,
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

#' @rdname autoplot.raw_spct
#'
#' @export
#'
autoplot.raw_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = getOption("ggspectra.norm",
                            default = "skip"),
           unit.out = "counts",
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           idfactor = TRUE,
           facets = FALSE,
           plot.data = "as.is",
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)

    idfactor <- validate_idfactor(idfactor = idfactor)
    # We trim the spectra to avoid unnecesary computaions later
    if (!is.null(range)) {
      object <- photobiology::trim_wl(object,
                                      range = range,
                                      use.hinges = TRUE,
                                      fill = NULL)
    }
    # We apply the normalization to the collection if it is to be bound
    # otherwise normalization is applied to the "parallel-summary" spectrum
    if (plot.data == "as.is") {
      object <- photobiology::normalize(object,
                                        range = getOption("ggspectra.wlrange", default = NULL),
                                        norm = norm,
                                        na.rm = na.rm)
      norm <- "skip"
    }
    # we convert the collection of spectra into a single spectrum object
    # containing a summary spectrum or multiple spectra in long form.
    z <- switch(plot.data,
                as.is = photobiology::rbindspct(object, idfactor = idfactor),
                mean = photobiology::s_mean(object),
                median = photobiology::s_median(object),
                sum = photobiology::s_sum(object),
                prod = photobiology::s_prod(object),
                var = photobiology::s_var(object),
                sd = photobiology::s_sd(object),
                se = photobiology::s_se(object)
    )
    if (is.raw_spct(z) && any(c("counts", "counts_1") %in% names(z))) {
      autoplot(object = z,
               range = getOption("ggspectra.wlrange", default = NULL),
               norm = norm,
               unit.out = unit.out,
               pc.out = pc.out,
               idfactor = idfactor,
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else {
      z <- as.generic_spct(z)
      autoplot(object = z,
               y.name = paste("counts", plot.data, sep = "."),
               range = getOption("ggspectra.wlrange", default = NULL),
               norm = norm,
               pc.out = pc.out,
               idfactor = idfactor,
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    }
  }

