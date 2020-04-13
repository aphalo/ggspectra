#' Create a complete ggplot for light-source spectra.
#'
#' This function returns a ggplot object with an annotated plot of a source_spct
#' object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a source_spct object.
#' @param w.band list of waveband objects.
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
#' @param annotations a character vector.
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried. If \code{idfactor=NA} no aesthetic is mapped
#'   to the spectra and the user needs to use 'ggplot2' functions to manually
#'   map an aesthetic or use facets for the spectra.
#' @param ylim numeric y axis limits,
#' @param na.rm logical.
#' @param ... currently ignored.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
e_plot <- function(spct,
                   w.band,
                   range,
                   label.qty,
                   span,
                   wls.target,
                   annotations,
                   text.size,
                   chroma.type,
                   idfactor,
                   ylim,
                   na.rm,
                   ...) {
  if (!is.source_spct(spct)) {
    stop("e_plot() can only plot source_spct objects.")
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  q2e(spct, byref=TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }
  duration.label <- NA
  if (is_normalized(spct) || is_scaled(spct)) {
    s.irrad.label <- "Spectral~~energy~~exposure~~E(lambda)~~(relative~~units)"
    irrad.label.total <- "atop(E, (relative~~units))"
    irrad.label.avg <- "atop(bar(E(lambda)), (relative~~units))"
    scale.factor <- 1
  } else {
    time.unit <- getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char == "second")  {
      s.irrad.label <- "Spectral~~energy~~irradiance~~E(lambda)~~(W~m^{-2}~nm^{-1})"
      irrad.label.total  <- "atop(E, (W~m^{-2}))"
      irrad.label.avg  <- "atop(bar(E(lambda)), (W~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else if (time.unit.char == "day") {
      s.irrad.label <- "Spectral~~energy~~exposure~~E(lambda)~~(MJ~d^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (MJ~d^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(E(lambda)), (MJ~d^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e-6
    } else if (time.unit.char == "hour") {
      s.irrad.label <- "Spectral~~energy~~exposure~~E(lambda)~~(kJ~h^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (kJ~h^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(E(lambda)), (kJ~h^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e-3
    } else if (time.unit.char == "duration") {
      s.irrad.label <- "Spectral~~energy~~fluence~~E(lambda)~~(kJ~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (kJ~m^{-2}))"
      irrad.label.avg <- "atop(bar(E(lambda)), (kJ~m^{-2}~nm^{-1}))"
      duration.label <- paste("Length of exposure:",
                              ifelse(lubridate::is.duration(time.unit),
                              as.character(time.unit), "unknown"))
      scale.factor <- 1e-3
    } else if (time.unit.char == "exposure") {
      s.irrad.label <- "Spectral~~energy~~fluence~~E(lambda)~~(J~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (J~m^{-2}))"
      irrad.label.avg <- "atop(bar(E(lambda)), (J~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else {
      s.irrad.label <- "Spectral~~energy~~exposure~~E(lambda)~~(arbitrary~~units)"
      irrad.label.total <- "atop(E, (arbitrary~~units))"
      irrad.label.avg <- "atop(bar(E(lambda)), (arbitrary~~units))"
      scale.factor <- 1
    }
  }
  if (label.qty == "total") {
    irrad.label <- irrad.label.total
  } else if (label.qty %in% c("average", "mean")) {
    irrad.label <- irrad.label.avg
  }  else if (label.qty == "contribution") {
    irrad.label <- "atop(Contribution~~to~~total, E~~(fraction))"
  } else if (label.qty == "contribution.pc") {
    irrad.label <- "atop(Contribution~~to~~total, E~~(percent))"
  } else if (label.qty == "relative") {
    irrad.label <- "atop(Relative~~to~~sum, E~~(fraction))"
  } else if (label.qty == "relative.pc") {
    irrad.label <- "atop(Relative~~to~~sum, E~~(percent))"
  } else {
    irrad.label <- ""
  }
  if (is_effective(spct)) {
    s.irrad.label <- sub("E", "E[eff]", s.irrad.label, fixed = TRUE)
    irrad.label <- sub("E", "E[eff]", irrad.label, fixed = TRUE)
    irrad.label.total <- sub("E", "E[eff]", irrad.label.total, fixed = TRUE)
    irrad.label.avg <- sub("E", "E[eff]", irrad.label.avg, fixed = TRUE)
  }
  s.irrad.label <- parse(text = s.irrad.label)
  spct[["s.e.irrad"]] <- spct[["s.e.irrad"]] * scale.factor

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(c(spct[["s.e.irrad"]], 0), na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(c(spct[["s.e.irrad"]], 0), na.rm = TRUE))

  plot <- ggplot(spct, aes_(x = ~w.length, y = ~s.e.irrad))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.min < (-0.01 * y.max)) {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
    } else {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
    }
  }

  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.irrad.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot + scale_fill_identity() + scale_color_identity()

  if (label.qty == "total") {
    label.qty <- "irrad"
  } else if (label.qty %in% c("mean", "average")) {
    label.qty <- "sirrad"
  }

  plot <- plot + decoration(w.band = w.band,
                            unit.out = "energy",
                            time.unit = getTimeUnit(spct),
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = irrad.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (is_effective(spct)) {
    plot <- plot +  annotate("text",
                             x = midpoint(spct),
                             y = y.max,
                             label = paste("BSWF:", getBSWFUsed(spct)),
                             vjust = -0.5, size = rel(3),
                             na.rm = TRUE)
  }

  if (!is.na(duration.label)) {
    plot <- plot +  annotate("text",
                             x = min(spct),
                             y = y.max,
                             label = duration.label,
                             vjust = -0.5,
                             hjust = 0,
                             size = rel(3),
                             na.rm = TRUE)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max * 1.05)
    x.limits <- range(spct)
  }
  if (abs(y.min) < 5e-2 && (abs(y.max - 1) < 5.e-2)) {
    plot <- plot +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = y.limits)
  } else {
    plot <- plot +
      scale_y_continuous(limits = y.limits)
  }

  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))
}

#' Create a complete ggplot for light-source spectra.
#'
#' This function returns a ggplot object with an annotated plot of a source_spct
#' object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a source_spct object.
#' @param w.band list of waveband objects.
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
#' @param annotations a character vector
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried. If \code{idfactor=NA} no aesthetic is mapped
#'   to the spectra and the user needs to use 'ggplot2' functions to manually
#'   map an aesthetic or use facets for the spectra.
#' @param ylim numeric y axis limits,
#' @param na.rm logical.
#' @param ... currently ignored.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
q_plot <- function(spct,
                   w.band,
                   range,
                   label.qty,
                   span,
                   wls.target,
                   annotations,
                   text.size,
                   chroma.type,
                   idfactor,
                   ylim,
                   na.rm,
                   ...) {
  if (!is.source_spct(spct)) {
    stop("q_plot() can only plot source_spct objects.")
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  e2q(spct, byref = TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  duration.label <- NA
  if (is_normalized(spct) || is_scaled(spct)) {
    s.irrad.label <- "Spectral~~photon~~exposure~~Q(lambda)~~(relative~~units)"
    irrad.label.total <- "atop(Q, (relative~~units))"
    irrad.label.avg <- "atop(bar(Q(lambda)), (relative~~units))"
    scale.factor <- 1
  } else {
    time.unit <- getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char=="second") {
      s.irrad.label <- "Spectral~~photon~~irradiance~~Q(lambda)~~(mu*mol~s^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total  <- "atop(Q, (mu*mol~s^{-1}~m^{-2}))"
      irrad.label.avg  <- "atop(bar(Q(lambda)), (mu*mol~s^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e6
    } else if (time.unit.char=="day") {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q(lambda)~~(mol~d^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~d^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q(lambda)), (mol~d^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else if (time.unit.char=="hour") {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q(lambda)~~(mmol~h^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mmol~h^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q(lambda)), (mmol~h^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e3
    } else if (time.unit.char=="duration" || lubridate::is.duration(time.unit)) {
      s.irrad.label <- "Spectral~~photon~~fluence~~Q(lambda)~~(mol~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q(lambda)), (mol~m^{-2}~nm^{-1}))"
      duration.label <- paste("Length of exposure:",
                              ifelse(lubridate::is.duration(time.unit),
                                     as.character(time.unit), "unknown"))
      scale.factor <- 1
    } else if (time.unit.char=="exposure" || lubridate::is.duration(time.unit)) {
      s.irrad.label <- "Spectral~~photon~~fluence~~Q(lambda)~~(mol~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q(lambda)), (mol~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q(lambda)~~(arbitrary~~units)"
      irrad.label.total <- "atop(Q, (arbitrary~~units))"
      irrad.label.avg <- "atop(bar(Q(lambda)), (arbitrary~~units))"
      scale.factor <- 1
    }
  }
  if (label.qty == "total") {
    irrad.label <- irrad.label.total
  } else if (label.qty %in% c("average", "mean")) {
    irrad.label <- irrad.label.avg
  } else if (label.qty == "contribution") {
    irrad.label <- "atop(Contribution~~to~~total, Q~~(fraction))"
  } else if (label.qty == "contribution.pc") {
    irrad.label <- "atop(Contribution~~to~~total, Q~~(percent))"
  } else if (label.qty == "relative") {
    irrad.label <- "atop(Relative~~to~~sum, Q~~(fraction))"
  } else if (label.qty == "relative.pc") {
    irrad.label <- "atop(Relative~~to~~sum, Q~~(percent))"
  } else {
    irrad.label <- ""
  }
  if (is_effective(spct)) {
    s.irrad.label <- sub("Q", "Q[eff]", s.irrad.label, fixed = TRUE)
    irrad.label <- sub("Q", "Q[eff]", irrad.label, fixed = TRUE)
    irrad.label.total <- sub("Q", "Q[eff]", irrad.label.total, fixed = TRUE)
    irrad.label.avg <- sub("Q", "Q[eff]", irrad.label.avg, fixed = TRUE)
  }
  s.irrad.label <- parse(text = s.irrad.label)
  spct[["s.q.irrad"]] <- spct[["s.q.irrad"]] * scale.factor

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(c(spct[["s.q.irrad"]], 0), na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(c(spct[["s.q.irrad"]], 0), na.rm = TRUE))

  plot <- ggplot(spct, aes_(x = ~w.length, y = ~s.q.irrad))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.min < (-0.01 * y.max)) {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
    } else {
      plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
    }
  }

  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.irrad.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot + scale_fill_identity() + scale_color_identity()

  if (label.qty == "total") {
    label.qty <- "irrad"
  } else if (label.qty %in% c("mean", "average")) {
    label.qty <- "sirrad"
  }

  plot <- plot + decoration(w.band = w.band,
                            unit.out = "photon",
                            time.unit = getTimeUnit(spct),
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = irrad.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (is_effective(spct)) {
    plot <- plot +  annotate("text",
                             x = midpoint(spct),
                             y = y.max,
                             label = paste("BSWF:", getBSWFUsed(spct)),
                             vjust = -0.5, size = rel(3),
                             na.rm = TRUE)
  }

  if (!is.na(duration.label)) {
    plot <- plot +  annotate("text",
                             x = min(spct),
                             y = y.max,
                             label = duration.label,
                             vjust = -0.5,
                             hjust = 0,
                             size = rel(3),
                             na.rm = TRUE)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max * 1.05)
    x.limits <- range(spct)
  }

  if (abs(y.min) < 5e-2 && (abs(y.max - 1) < 5.e-2)) {
    plot <- plot +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = y.limits)
  } else {
    plot <- plot + scale_y_continuous(limits = y.limits)
  }

  plot + scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))
}

#' Create a complete ggplot for a light-source spectrum.
#'
#' These methods return a ggplot object with an annotated plot of a source_spct
#' object or of the spectra contained in a source_mspct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot object, and can be further manipulated and
#'   added to.
#'
#' @param object a source_spct or a source_mspct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param unit.out character string indicating type of radiation units to use
#'   for plotting: "photon" or its synomin "quantum", or "energy".
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
#' @param time.format character Format as accepted by
#'   \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the default
#'   "spct.idx" is tried.
#' @param ylim numeric y axis limits,
#' @param object.label character The name of the object being plotted.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @export
#'
#' @keywords hplot
#'
#' @examples
#'
#' autoplot(sun.spct)
#' autoplot(sun.spct, unit.out = "photon")
#'
#' @family autoplot methods
#'
autoplot.source_spct <-
  function(object, ...,
           w.band=getOption("photobiology.plot.bands",
                            default = list(UVC(), UVB(), UVA(), PAR())),
           range=NULL,
           unit.out=getOption("photobiology.radiation.unit", default = "energy"),
           label.qty = NULL,
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           time.format = "",
           tz = "UTC",
           text.size = 2.5,
           chroma.type = "CMF",
           idfactor = NULL,
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    if (is.null(label.qty)) {
      if (is_normalized(object) || is_scaled(object)) {
        label.qty = "contribution"
      } else {
        label.qty = "total"
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

    if (unit.out %in% c("photon", "quantum")) {
      out.ggplot <- q_plot(spct = object, w.band = w.band, range = range,
                           label.qty = label.qty,
                           span = span,
                           wls.target = wls.target,
                           annotations = annotations,
                           text.size = text.size,
                           chroma.type = chroma.type,
                           idfactor = idfactor,
                           ylim = ylim,
                           na.rm = na.rm,
                           ...)
    } else if (unit.out == "energy") {
      out.ggplot <- e_plot(spct = object, w.band = w.band, range = range,
                           label.qty = label.qty,
                           span = span,
                           wls.target = wls.target,
                           annotations = annotations,
                           text.size = text.size,
                           chroma.type = chroma.type,
                           idfactor = idfactor,
                           ylim = ylim,
                           na.rm = na.rm,
                           ...)
    } else {
      stop("Invalid 'radiation.unit' argument value: '", unit.out, "'")
    }
    out.ggplot +
      autotitle(object = object,
                   object.label = object.label,
                   time.format = time.format,
                   tz = tz,
                   annotations = annotations)
  }

#' @rdname autoplot.source_spct
#'
#' @param plot.data character Data to plot. Default is "as.is" plotting
#'   one line per spectrum. When passing "mean" or "median" as
#'   argument all the spectra must contain data at the same wavelength values.
#'
#' @export
#'
autoplot.source_mspct <-
  function(object,
           ...,
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           plot.data = "as.is") {
    # We trim the spectra to avoid unnecesary computaions later
    if (!is.null(range)) {
      object <- trim_wl(object, range = range, use.hinges = TRUE, fill = NULL)
    }
    # conversion before binding and summaries
    if (unit.out == "energy") {
      data <- q2e(object, action = "replace")
    } else if (unit.out %in% c("photon", "quantum")) {
      data <- e2q(object, action = "replace")
    } else {
      stop("Invalid 'unit.out' argument value: '", unit.out, "'")
    }
    # we convert the collection of spectra into a single spectrum object
    # containing a summary spectrum or multiple spectra in long form.
    z <- switch(plot.data,
                mean = photobiology::s_mean(object),
                median = photobiology::s_median(object),
                as.is = photobiology::rbindspct(object)
    )
    autoplot(object = z, range = NULL, ...)
  }

## internal

#' Convert lubridate duration objects to a string if possible
#'
#' @param time.unit lubridate::duration object or character
#'
#' @keywords internal
#'
duration2character <- function(time.unit) {
  if (is.character(time.unit)) return(time.unit)
  if (!lubridate::is.duration(time.unit)) return("unknown")
  if (time.unit == lubridate::duration(1, "seconds")) return("second")
  if (time.unit == lubridate::duration(1, "hours")) return("hour")
  if (time.unit == lubridate::duration(1, "days")) return("day")
  "duration"
}
