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
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
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
                     by.group,
                     geom,
                     text.size,
                     idfactor,
                     facets,
                     ylim,
                     object.label,
                     na.rm) {
  if (!photobiology::is.cps_spct(spct)) {
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
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- photobiology::trim_wl(w.band, range = range(spct))
  }
  cps.cols <- names(spct)[grep("^cps", names(spct))]
  num.cps.cols <- length(cps.cols)
  # if individual spectra have multiple columns we force facets
  if (!as.logical(facets) && num.cps.cols > 1L &&
      photobiology::getMultipleWl(spct) > 1L) {
    message("Usings facets because spectra contain multiple scans.")
    facets <- TRUE
  }

  if (photobiology::is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.cps.label <-
      expression(Pixel~~response~~rate~~k %*% N[lambda]~~("rel."))
    cps.label <- ""
  } else if (photobiology::is_normalized(spct)) {
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
    spct <- photobiology::spct_wide2long(spct = spct, idfactor = "scan")
    plot <-
      ggplot2::ggplot(spct,
                      ggplot2::aes(x = .data[["w.length"]],
                                   y = .data[["cps"]],
                                   linetype = .data[["scan"]]))
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          map.linetype = !facets && !by.group,
                          annotations = annotations,
                          num.columns = num.cps.cols)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  } else {
    plot <-
      ggplot2::ggplot(spct,
                      ggplot2::aes(x = .data[["w.length"]],
                                   y = .data[["cps"]]))
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          map.linetype = !facets && !by.group,
                          annotations = annotations)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["cps"]] <- ifelse(spct[["cps"]] < y.min,
                            NA_real_,
                            spct[["cps"]])
  } else {
    y.min <- min(spct[["cps"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["cps"]] <- ifelse(spct[["cps"]] > y.max,
                            NA_real_,
                            spct[["cps"]])
  } else {
    y.max <- max(spct[["cps"]], y.min, 0, na.rm = TRUE)
  }

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.min < (-0.01 * y.max)) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
    } else {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + ggplot2::geom_line(na.rm = na.rm)
  plot <- plot + ggplot2::labs(x = expression("Wavelength, "*lambda~(nm)),
                               y = s.cps.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot +
    ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            by.group = by.group,
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
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- range(spct)
  }

  if (pc.out) {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = y.breaks,
                                  limits = y.limits)
  } else {
    plot <-
      plot + ggplot2::scale_y_continuous(breaks = y.breaks,
                                         limits = y.limits)
  }
  plot + ggplot2::scale_x_continuous(limits = x.limits,
                                     breaks = scales::pretty_breaks(n = 7))

}

#' Plot one or more detector-counts-per-second spectra.
#'
#' These methods return a ggplot object with an annotated plot of a
#' \code{cps_spct} or a \code{cps_mspct} object.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object a cps_spct object.
#' @param unit.out character IGNORED.
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
#' autoplot(normalize(white_led.cps_spct, norm = "max"))
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
                              default = list(photobiologyWavebands::UVC(),
                                             photobiologyWavebands::UVB(),
                                             photobiologyWavebands::UVA(),
                                             photobiologyWavebands::PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           unit.out = NULL,
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           label.qty = "mean",
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           by.group = FALSE,
           geom = "line",
           time.format = "",
           tz = "UTC",
           text.size = 2.5,
           idfactor = NULL,
           facets = FALSE,
           plot.data = "as.is",
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)
    object <- apply_normalization(object, norm)
    idfactor <- check_idfactor_arg(object, idfactor)
    object <- rename_idfactor(object, idfactor)

    if (plot.data != "as.is") {
      return(
        autoplot(object = photobiology::subset2mspct(object),
                 w.band = w.band,
                 range = range,
                 unit.out = unit.out,
                 pc.out = pc.out,
                 label.qty = label.qty,
                 span = span,
                 wls.target = wls.target,
                 annotations = annotations,
                 by.group = by.group,
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

    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)

    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- photobiology::waveband(object)
      } else if (photobiology::is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-
          photobiology::waveband(range, wb.name = "Total")
      }
    }

    cps_plot(spct = object,
             w.band = w.band,
             range = range,
             pc.out = pc.out,
             label.qty = label.qty,
             span = span,
             wls.target = wls.target,
             annotations = annotations,
             by.group = by.group,
             geom = geom,
             text.size = text.size,
             idfactor = idfactor,
             facets = facets,
             ylim = ylim,
             object.label = object.label,
             na.rm = na.rm) +
      autotitle(object = object,
                time.format = time.format,
                tz = tz,
                object.label = object.label,
                annotations = annotations)
  }

#' @rdname autoplot.cps_spct
#'
#' @export
#'
autoplot.cps_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           unit.out = NULL,
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           by.group = FALSE,
           idfactor = TRUE,
           facets = FALSE,
           plot.data = "as.is",
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)
    object <- apply_normalization(object, norm)
    idfactor <- check_idfactor_arg(object, idfactor = idfactor, default = TRUE)

    # We trim the spectra to avoid unnecessary computations later
    if (!is.null(range)) {
      object <- photobiology::trim_wl(object,
                                      range = range,
                                      use.hinges = TRUE,
                                      fill = NULL)
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
    if (photobiology::is.cps_spct(z) &&
        any(c("cps", "cps_1") %in% names(z))) {
      autoplot(object = z,
               range = NULL, # trimmed above
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else {
      z <- photobiology::as.generic_spct(z)
      autoplot(object = z,
               y.name = paste("cps", plot.data, sep = "."),
               range = NULL, # trimmed above
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    }
  }

