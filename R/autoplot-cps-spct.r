#' Create a complete ggplot for detector-counts per second spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' cps_spct object.
#'
#' Note that scales are expanded so as to make space for the annotations. The
#' object returned is a ggplot object, and can be further manipulated. When spct
#' has more than one column with spectral data, each of these columns is
#' normalized individually.
#'
#' @param spct a cps_spct object
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
cps_plot <- function(spct,
                     w.band,
                     range,
                     pc.out,
                     label.qty,
                     span,
                     wls.target,
                     annotations,
                     geom,
                     text.size,
                     idfactor,
                     facets,
                     ylim,
                     object.label,
                     na.rm,
                     ...) {
  if (!is.cps_spct(spct)) {
    stop("cps_plot() can only plot cps_spct objects.")
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
  cps.cols <- names(spct)[grep("^cps", names(spct))]
  num.cps.cols <- length(cps.cols)
  # if individual spectra have multiple columns we force facets
  if (!as.logical(facets) && num.cps.cols > 1L && getMultipleWl(spct) > 1L) {
    message("Usings facets because spectra contain multiple scans.")
    facets <- TRUE
  }

  if (is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.cps.label <-
      expression(Pixel~~response~~rate~~k %*% N[lambda]~~("rel."))
    cps.label <- ""
  } else if (is_normalized(spct)) {
    norm.ls <- photobiology::getNormalization(spct)
    norm.wl <- round(norm.ls[["norm.wl"]], digits = 1)
    if (pc.out) {
      multiplier.label <- "%"
    } else {
      multiplier.label <- "/1"
    }
    s.cps.label <-
      bquote(Pixel~~response~~rate~~N[lambda]/N[lambda == .(norm.wl)]~~(.(multiplier.label)))
    cps.label <- ""
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.cps.label <-
      expression(Pixel~~response~~rate~~N[lambda]~~(counts~~s^{-1}))
    cps.label <- ""
  }

  if (num.cps.cols > 1L) {
    # remove cps_spct class before melting as it invalidates expectations
    rmDerivedSpct(spct)
    spct <- tidyr::pivot_longer(data = spct,
                                cols = tidyselect::all_of(cps.cols),
                                names_to = "scan",
                                values_to = "cps")
    setCpsSpct(spct, multiple.wl = NULL) # guessed from data
    plot <- ggplot(spct, aes(x = .data[["w.length"]], y = .data[["cps"]], linetype = .data[["scan"]]))
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations,
                          num.columns = num.cps.cols)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  } else {
    plot <- ggplot(spct, aes(x = .data[["w.length"]], y = .data[["cps"]]))
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  }

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(c(spct[["cps"]], 0), na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(c(spct[["cps"]], 0), na.rm = TRUE))

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.min < (-0.01 * y.max)) {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
    } else {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.cps.label)

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
                            summary.label = cps.label,
                            text.size = text.size,
                            na.rm = TRUE)

  if (abs(y.max - 1) < 0.02 && abs(y.min) < 0.02) {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    y.breaks <- scales::pretty_breaks(n = 5)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries", "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
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

#' Create a complete ggplot for detector-counts per second spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' Note that scales are expanded so as to make space for the annotations. The
#' object returned is a ggplot objects, and can be further manipulated.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @param object a cps_spct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param unit.out character IGNORED.
#' @param pc.out logical, if TRUE use percent instead of fraction of one for
#'   normalized spectral data.
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
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
#' @param time.format character Format as accepted by \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param norm numeric normalization wavelength (nm) or character string
#'   \code{"max"} for normalization at the wavelength of highest peak or
#'   \code{"skip"} for no change to exisitng normalization in \code{object}.
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
#'   \code{\link[photobiology]{cps_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @export
#'
#' @examples
#'
#' autoplot(white_led.cps_spct)
#' autoplot(white_led.cps_spct, geom = "spct")
#' autoplot(white_led.cps_spct, norm = "max")
#'
#' two_leds.mspct <-
#'   cps_mspct(list("LED 1" = white_led.cps_spct,
#'                  "LED 2" = white_led.cps_spct / 2))
#' autoplot(two_leds.mspct)
#' autoplot(two_leds.mspct, idfactor = "Spectra")
#' autoplot(two_leds.mspct, plot.data = "mean")
#'
#' @family autoplot methods
#'
autoplot.cps_spct <-
  function(object,
           ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PAR())),
           range = NULL,
           norm = "skip",
           unit.out = NULL,
           pc.out = FALSE,
           label.qty = "mean",
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           geom = "line",
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
                default = c("boxes", "labels", "colour.guide", "peaks"))
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
        w.band <-  waveband(range, wb.name = "Total")
      }
    }

    cps_plot(spct = object,
             w.band = w.band,
             range = range,
             norm = norm,
             pc.out = pc.out,
             label.qty = label.qty,
             span = span,
             wls.target = wls.target,
             annotations = annotations,
             geom = geom,
             text.size = text.size,
             idfactor = idfactor,
             facets = facets,
             ylim = ylim,
             object.label = object.label,
             na.rm = na.rm,
             ...) +
      autotitle(object = object,
                time.format = time.format,
                tz = tz,
                object.label = object.label,
                annotations = annotations)
  }

#' @rdname autoplot.cps_spct
#'
#' @param plot.data character Data to plot. Default is "as.is" plotting one line
#'   per spectrum. When passing "mean", "median", "sum", "prod", "var", "sd",
#'   "se" as argument all the spectra must contain data at the same wavelength
#'   values.
#'
#' @export
#'
autoplot.cps_mspct <-
  function(object,
           ...,
           range = NULL,
           norm = "skip",
           unit.out = NULL,
           pc.out = FALSE,
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
                                        range = NULL,
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
    if (is.cps_spct(z) && any(c("cps", "cps_1") %in% names(z))) {
      autoplot(object = z,
               range = NULL,
               norm = norm,
               pc.out = pc.out,
               idfactor = idfactor,
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else {
      z <- as.generic_spct(z)
      autoplot(object = z,
               y.name = paste("cps", plot.data, sep = "."),
               range = NULL,
               norm = norm,
               pc.out = pc.out,
               idfactor = idfactor,
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    }
  }

