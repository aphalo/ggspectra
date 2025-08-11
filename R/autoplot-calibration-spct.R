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
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
#' @param norm numeric Normalization wavelength (nm) or character string "max",
#'   or "min" for normalization at the corresponding wavelength, "update" to
#'   update the normalization after modifying units of expression, quantity
#'   or range but respecting the previously used criterion, or "skip" to force
#'   return of \code{object} unchanged. Always skipped for
#'   \code{plot.qty == "all"}, which is the default.
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
                     by.group,
                     geom,
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
    if ("summaries" %in% annotations) {
      # boxes or segments display summarised wavelengths
      w.band <- photobiology::trim_wl(w.band,
                                      range = photobiology::wl_range(spct))
    } else {
      # boxes and segments display wavebands' definitions if they fit in plot
      w.band <- photobiology::trim_wl(w.band, range = range)
    }
  }
  # replace NULL and NAs in range
  if (is.null(range)) {
    range <- range(spct[["w.length"]], na.rm = TRUE)
  } else {
    if (is.na(range[1])) {
      range[1] <- min(spct[["w.length"]], na.rm = TRUE)
    }
    if (is.na(range[2])) {
      range[2] <- max(spct[["w.length"]], na.rm = TRUE)
    }
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
      bquote(Coefficients~~k[italic(lambda)]~~(J~m^{-2}~nm^{-1}~n^{-1}))
    counts.label <- ""
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["irrad.mult"]] <- ifelse(spct[["irrad.mult"]] < y.min,
                                   NA_real_,
                                   spct[["irrad.mult"]])
  } else {
    y.min <- min(spct[["irrad.mult"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["irrad.mult"]] <- ifelse(spct[["irrad.mult"]] > y.max,
                                   NA_real_,
                                   spct[["irrad.mult"]])
  } else {
    y.max <- max(spct[["irrad.mult"]], 0, y.min, na.rm = TRUE)
  }

  if (num.mult.cols > 1L) {
    spct <- photobiology::spct_wide2long(spct = spct, idfactor = "scan")
    plot <- ggplot(spct) + aes(x = .data[["w.length"]], y = .data[["irrad.mult"]], linetype = .data[["scan"]])
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          map.linetype = !facets && !by.group,
                          annotations = annotations,
                          num.columns = num.mult.cols)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  } else {
    plot <- ggplot(spct) + aes(x = .data[["w.length"]], y = .data[["irrad.mult"]])
    temp <- find_idfactor(spct = spct,
                          idfactor = idfactor,
                          facets = facets,
                          map.linetype = !facets && !by.group,
                          annotations = annotations)
    plot <- plot + temp$ggplot_comp
    annotations <- temp$annotations
  }

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

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = bquote("Wavelength, "*lambda~(nm)), y = s.counts.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot + scale_fill_identity() + scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = range[2],
                            x.min = range[1],
                            annotations = annotations,
                            by.group = by.group,
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
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct$w.length, range, na.rm = TRUE) - photobiology::wl_expanse(spct) * 0.025,
                  max(spct$w.length, range, na.rm = TRUE) + 1) # +1 needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max * 1.05)
    x.limits <- c(min(spct$w.length, range, na.rm = TRUE) - 1,
                  max(spct$w.length, range, na.rm = TRUE) + 1)
  }

  plot <- plot + scale_y_continuous(limits = y.limits)
  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))

}

#' Plot one or more irradiance-calibration spectra.
#'
#' These methods return a ggplot object with an annotated plot of the spectral
#' data contained in a \code{calibration_spct} or a \code{calibration_mspct}
#' object.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object a calibration_spct object or a calibration_mspct object.
#' @param unit.out character IGNORED.
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{calibration_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @examples
#' # to be added
#'
#' @export
#'
#' @family autoplot methods
#'
autoplot.calibration_spct <-
  function(object, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           unit.out = "ignored",
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           label.qty = "mean",
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           by.group = FALSE,
           geom = "line",
           time.format = "",
           tz = "UTC",
           norm = NA,
           text.size = 2.5,
           idfactor = NULL,
           facets = FALSE,
           plot.data = "as.is",
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)
    object <- apply_normalization(object, norm)
    idfactor <- check_idfactor_arg(object, idfactor = idfactor)

    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)

    if (photobiology::getMultipleWl(object) > 1L) {
      if (plot.data == "as.is") {
        if (!facets) {
          # with a multiple spectra per panel do not include summaries
          annotations <-
            decode_annotations(c("-", "summaries"), annotations)
        }
      } else {
        # compute parallel summaries across spectra
        return(
          autoplot(object = subset2mspct(object),
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
    }

    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- waveband(object)
      } else if (is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  waveband(range, wb.name = "Total")
      }
    }
    if (is.null(range)) {
      range <- rep(NA_real_, 2)
    } else if (photobiology::is.waveband(range) ||
               photobiology::is.any_spct(range)) {
      range <- photobiology::wl_range(range)
    } else if (is.numeric(range) &&
               (length(range) > 2L || !anyNA(range))) {
      range <- range(range, na.rm = TRUE)
    }
    if (!length(range) == 2L || !is.numeric(range)) {
      warning("Ignoring bad 'range' argument")
      range <- rep(NA_real_, 2)
    }

    cal_plot(spct = object,
             w.band = w.band,
             range = range,
             label.qty = label.qty,
             span = span,
             wls.target = wls.target,
             pc.out = pc.out,
             annotations = annotations,
             by.group = by.group,
             geom = geom,
             norm = FALSE, # cal_plot needs to be updated
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
#' @export
#'
autoplot.calibration_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           unit.out = "ignored",
           norm = NA,
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           by.group = FALSE,
           plot.data = "as.is",
           idfactor = TRUE,
           facets = FALSE,
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)
    object <- apply_normalization(object, norm)
    idfactor <- check_idfactor_arg(object, idfactor = idfactor, default = TRUE)

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
    if (is.calibration_spct(z) && "irrad.mult" %in% names(z)) {
      autoplot(object = z,
               range = range, # trimmed above, needed for expansion
               unit.out = unit.out,
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else {
      z <- as.generic_spct(z)
      autoplot(object = z,
               y.name = paste("irrad.mult", plot.data, sep = "."),
               range = range, # trimmed above, needed for expansion
               unit.out = unit.out,
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    }
  }

