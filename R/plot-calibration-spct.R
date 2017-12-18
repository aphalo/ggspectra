#' Plot method for spectral irradiation calibrations.
#'
#' This function returns a ggplot object with an annotated plot of a
#' calibration_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'   When spct has more than one column with spectral data, each of these
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
#' @param annotations a character vector
#' @param norm numeric normalization wavelength (nm) or character string "max"
#'   for normalization at the wavelength of highest peak.
#' @param text.size numeric size of text in the plot decorations.
#' @param na.rm logical.
#' @param ... other arguments
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
                     annotations,
                     norm,
                     text.size,
                     na.rm,
                     ...) {
  if (!is.calibration_spct(spct)) {
    stop("cal_plot() can only plot calibration_spct objects.")
  }
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  mult.cols <- names(spct)[grep("^irrad.mult", names(spct))]
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

  spct <- reshape2::melt(spct,
                         id.vars = "w.length",
                         measure.vars = mult.cols,
                         variable.name = "scan",
                         value.name = "irrad.mult")
  setCalibrationSpct(spct, multiple.wl = length(mult.cols))
  y.max <- max(spct[["irrad.mult"]], 0, na.rm = TRUE)
  y.min <- min(spct[["irrad.mult"]], 0, na.rm = TRUE)
  plot <- ggplot(spct) + aes_(linetype = ~scan)

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
                            summary.label = counts.label,
                            text.size = text.size,
                            na.rm = TRUE)

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels",
                         "summaries", "colour.guide", "reserve.space"),
                       annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
    x.limits <- c(min(spct) - spread(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- range(spct)
  }

  plot <- plot + scale_y_continuous(limits = y.limits)
  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))

}


#' Plot method for spectral irradiation calibrations.
#'
#' This function returns a ggplot object with an annotated plot of a
#' calibration_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#' The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param x a calibration_spct object
#' @param ... other arguments passed along, such as \code{label.qty}
#' @param w.band a single waveband object or a list of waveband objects
#' @param range an R object on which range() returns a vector of length 2,
#' with min annd max wavelengths (nm)
#' @param unit.out character IGNORED
#' @param pc.out logical, if TRUE use percents instead of fraction of one
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels, one of "mean", "total", "contribution", and "relative".
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element.
#' @param annotations a character vector ("summaries" is ignored)
#' @param time.format character Format as accepted by \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param norm numeric normalization wavelength (nm) or character string "max"
#' for normalization at the wavelength of highest peak.
#' @param text.size numeric size of text in the plot decorations.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @export
#'
#' @keywords hplot
#'
#' @family plot functions
#'
plot.calibration_spct <-
  function(x, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PAR())),
           range = NULL,
           unit.out = "counts",
           pc.out = FALSE,
           label.qty = "mean",
           span = NULL,
           annotations = NULL,
           time.format = "",
           tz = "UTC",
           norm = NULL,
           text.size = 2.5,
           na.rm = TRUE) {
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- waveband(x)
      } else if (is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  waveband(range, wb.name = "Total")
      }
    }

    cal_plot(spct = x,
             w.band = w.band,
             range = range,
             label.qty = label.qty,
             span = span,
             pc.out = pc.out,
             annotations = annotations,
             norm = norm,
             text.size = text.size,
             na.rm = na.rm,
             ...) +
      ggtitle_spct(x = x,
                   time.format = time.format,
                   tz = tz,
                   x.name = deparse(substitute(x)),
                   annotations = annotations)
  }


