#' Create a complete ggplot for an irradiation calibration spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a
#' calibration_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot object, and can be further manipulated.
#'   When \code{spct} has more than one column with spectral data, each of these
#'   columns is normalized individually.
#'
#' @param spct a calibration_spct object
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
#' @param norm numeric normalization wavelength (nm) or character string "max"
#'   for normalization at the wavelength of highest peak.
#' @param text.size numeric size of text in the plot decorations.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried. If \code{idfactor=NA} no aesthetic is mapped
#'   to the spectra and the user needs to use 'ggplot2' functions to manually
#'   map an aesthetic or use facets for the spectra.
#' @param facets logical or integer Indicating if facets are to be created for
#'   the levels of \code{idfactor} when \code{spct} contain multiple spectra in
#'   long form.
#' @param na.rm logical.
#' @param ylim numeric y axis limits,
#' @param ... currently ignored.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
cal_plot <- function(spct,
                     w.band,
                     range,
                     pc.out,
                     label.qty,
                     span,
                     wls.target,
                     annotations,
                     norm,
                     text.size,
                     idfactor,
                     facets,
                     ylim,
                     na.rm,
                     ...) {
  if (!is.calibration_spct(spct)) {
    stop("cal_plot() can only plot calibration_spct objects.")
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

  mult.cols <- names(spct)[grep("^irrad.mult", names(spct))]
  num.mult.cols <- length(mult.cols)
  # if individual spectra have multiple columns we force facets
  if (!as.logical(facets) && num.mult.cols > 1L && getMultipleWl(spct) > 1L) {
    message("Usings facets because spectra contain multiple scans.")
    facets <- TRUE
  }
  #  other.cols <- setdiff(names(x), mult.cols)
  if (is.null(norm)) {
    # we will use the original data
    scale.factor <- 1
  } else {
    for (col in mult.cols) {
      if (is.character(norm)) {
        if (norm %in% c("max", "maximum")) {
          idx <- which.max(spct[[col]])
        } else {
          warning("Invalid character '", norm, "'value in 'norm'")
          return(ggplot())
        }
        scale.factor <- 1 / as.numeric(spct[idx, col])
        norm <- as.numeric(spct[idx, "w.length"])
      } else if (is.numeric(norm) && norm >= min(spct) && norm <= max(spct)) {
        scale.factor <- 1 / interpolate_spct(spct, norm)[[col]]
      } else if (is.numeric(norm)) {
        warning("'norm = ", norm, "' value outside spectral data range of ",
                round(min(spct)), " to ", round(max(spct)), " (nm)")
        return(ggplot())
      } else {
        stop("'norm' should be numeric or character")
      }
      spct[[col]] <-  spct[[col]] * scale.factor
    }
  }

  if (scale.factor != 1) {
    if (!pc.out) {
      multiplier.label <- "rel."
      #      scale.factor <- 1 * scale.factor
    } else {
      multiplier.label <- "%"
      scale.factor <- 100 * scale.factor
    }
    if (is.numeric(norm)) {
      norm <- signif(norm, digits = 4)
    }
    s.counts.label <-
      bquote(Coefficients~~k[italic(lambda)]/k( .(norm))~~(.(multiplier.label)))
    counts.label <- ""
  } else {
    s.counts.label <-
      expression(Coefficients~~k[italic(lambda)]~~(J~m^{-2}~nm^{-1}~n^{-1}))
    counts.label <- ""
  }

  if (num.mult.cols > 1L) {
    # remove raw_spct class before melting as it invalidates expectations
    rmDerivedSpct(spct)
    spct <- tidyr::gather_(data = spct,
                           key_col = "scan",
                           value_col = "irrad.mult",
                           gather_cols = mult.cols)
    setCalibrationSpct(spct, multiple.wl = NULL) # guessed from data
    plot <- ggplot(spct) + aes_(x = ~w.length, y = ~irrad.mult, linetype = ~scan)
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations,
                          num.columns = num.mult.cols)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  } else {
    plot <- ggplot(spct) + aes_(x = ~w.length, y = ~irrad.mult)
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          annotations = annotations)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  }

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(spct[["irrad.mult"]], 0, na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(spct[["irrad.mult"]], 0, na.rm = TRUE))

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.min < -0.01 * y.max) {
      plot <- plot + geom_hline(yintercept = 0,
                                linetype = "dashed", colour = "red")
    } else {
      plot <- plot + geom_hline(yintercept = 0,
                                linetype = "dashed", colour = "black")
    }
  }

  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.counts.label)

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

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels",
                         "summaries", "colour.guide", "reserve.space"),
                       annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- range(spct)
  }

  plot <- plot + scale_y_continuous(limits = y.limits)
  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))

}

#' Create a complete ggplot for an irradiation calibration spectrum.
#'
#' These methods return a ggplot object with an annotated plot of a
#' calibration_spct object or of the spectra contained in a calibration_mspct
#' object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#' The object returned is a ggplot object, and can be further manipulated.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @param object a calibration_spct object or a calibration_mspct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param unit.out character IGNORED.
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
#' @param annotations a character vector ("summaries" is ignored). For details
#'   please see sections Plot Annotations and Title Annotations.
#' @param time.format character Format as accepted by
#'   \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param norm numeric normalization wavelength (nm) or character string "max"
#'   for normalization at the wavelength of highest peak.
#' @param text.size numeric size of text in the plot decorations.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the default
#'   "spct.idx" is tried. If \code{idfactor=NA} no aesthetic is mapped to the
#'   spectra and the user needs to use 'ggplot2' functions to manually map an
#'   aesthetic or use facets for the spectra.
#' @param facets logical or integer Indicating if facets are to be created for
#'   the levels of \code{idfactor} when \code{spct} contain multiple spectra in
#'   long form.
#' @param ylim numeric y axis limits,
#' @param object.label character The name of the object being plotted.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @export
#'
#' @family autoplot methods
#'
autoplot.calibration_spct <-
  function(object, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PAR())),
           range = NULL,
           unit.out = "counts",
           pc.out = FALSE,
           label.qty = "mean",
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           time.format = "",
           tz = "UTC",
           norm = NULL,
           text.size = 2.5,
           idfactor = NULL,
           facets = FALSE,
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- waveband(object)
      } else if (is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  waveband(range, wb.name = "Total")
      }
    }

    cal_plot(spct = object,
             w.band = w.band,
             range = range,
             label.qty = label.qty,
             span = span,
             wls.target = wls.target,
             pc.out = pc.out,
             annotations = annotations,
             norm = norm,
             text.size = text.size,
             idfactor = idfactor,
             facets = facets,
             na.rm = na.rm,
             ylim = ylim,
             ...) +
      autotitle(object = object,
                   time.format = time.format,
                   tz = tz,
                   object.label = object.label,
                   annotations = annotations)
  }

#' @rdname autoplot.calibration_spct
#'
#' @param plot.data character Data to plot. Default is "as.is" plotting one line
#'   per spectrum. When passing "mean", "median", "sum", "prod", var", "sd",
#'   "se" as argument all the spectra must contain data at the same wavelength
#'   values.
#'
#' @export
#'
autoplot.calibration_mspct <-
  function(object,
           ...,
           range = NULL,
           idfactor = TRUE,
           facets = FALSE,
           plot.data = "as.is") {
    # We trim the spectra to avoid unnecessary computations later
    if (!is.null(range)) {
      object <- trim_wl(object, range = range, use.hinges = TRUE, fill = NULL)
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
    autoplot(object = z, range = NULL, idfactor = idfactor, facets = facets, ...)
  }

