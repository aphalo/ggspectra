#' Create a complete ggplot for response spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a response_spct object.
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
#' @param text.size numeric size of text in the plot decorations.
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the
#'   default "spct.idx" is tried.
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
e_rsp_plot <- function(spct,
                       w.band,
                       range,
                       pc.out,
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
  if (!is.response_spct(spct)) {
    stop("e_Rsp_plot() can only plot response_spct objects.")
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  q2e(spct, action="replace", byref=TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  exposure.label <- NA

  if (is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.rsp.label <- expression(Spectral~~energy~~response~~k %*% R[E~lambda]~~("rel."))
    rsp.label.total <- "atop(k %*% R[E], (\"rel.\"))"
    rsp.label.avg <- "atop(bar(k %*% R[E~lambda]), (\"rel.\"))"
  } else if (is_normalized(spct)) {
    if (!pc.out) {
      multiplier.label <- "rel."
    } else {
      multiplier.label <- "%"
    }
    norm.ls <- photobiology::getNormalization(spct)
    norm.wl <- round(norm.ls[["norm.wl"]], digits = 1)
    s.rsp.label <-
      bquote(Spectral~~energy~~response~~R[E~lambda]/R[E~lambda==.(norm.wl)]~~(.(multiplier.label)))
    rsp.label.total  <- bquote(atop(integral(R[E]/R[E~lambda==.(norm.wl)], min, max), (.(multiplier.label))))
    rsp.label.avg  <- bquote(atop(bar(R[E~lambda]/R[E](lambda==.(norm.wl))), (.(multiplier.label))))
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    time.unit <- getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char=="second") {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(resp.~~unit~~s^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[E], (resp.~~unit~~s^{-1}))"
      rsp.label.avg  <- "atop(bar(R[E~lambda]), (resp.~~unit~~s^{-1}~nm^{-1}))"
    } else if (time.unit.char=="day") {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(resp.~~unit~~d^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[E], (resp.~~unit~~d^{-1}))"
      rsp.label.avg  <- "atop(bar(R[E~lambda]), (resp.~~unit~~d^{-1}~nm^{-1}))"
    } else if (time.unit.char=="hour") {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(resp.~~unit~~h^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[E], (resp.~~unit~~h^{-1}))"
      rsp.label.avg  <- "atop(bar(R[E~lambda]), (resp.~~unit~~h^{-1}~nm^{-1}))"
    } else if (time.unit.char=="duration") {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(resp.~~unit~nm^{-1}))
      rsp.label.total  <- "atop(R[E], (resp.~~unit))"
      rsp.label.avg  <- "atop(bar(R[E~lambda]), (resp.~~unit~nm^{-1}))"
      exposure.label <- paste("Length of time:",
                              ifelse(lubridate::is.duration(time.unit),
                                     as.character(time.unit), "unknown"))
    } else if (time.unit.char=="exposure") {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(resp.~~unit~nm^{-1}))
      rsp.label.total  <- "atop(R[E], (resp.~~unit))"
      rsp.label.avg  <- "atop(bar(R[E~lambda]), (resp.~~unit~nm^{-1}))"
    } else {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E~lambda]~~(arbitrary~~units))
      rsp.label.total <- "atop(R[E], (arbitrary~~units))"
      rsp.label.avg <- "atop(bar(R[E~lambda]), (arbitrary~~units))"
    }
  }

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(c(spct[["s.e.response"]], 0), na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(c(spct[["s.e.response"]], 0), na.rm = TRUE))

  if (label.qty == "total") {
    rsp.label <- "integral(R[E](lambda))"
  } else if (label.qty %in% c("average", "mean")) {
    rsp.label <- "bar(R[E](lambda))"
  } else if (label.qty == "contribution") {
    rsp.label <- "atop(Contribution~~to~~total, R[E]~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    rsp.label <- "atop(Contribution~~to~~total, R[E]~~(\"%\"))"
  } else if (label.qty == "relative") {
    rsp.label <- "atop(Relative~~to~~sum, R[E]~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    rsp.label <- "atop(Relative~~to~~sum, R[E]~~(\"%\"))"
  } else {
    rsp.label <- ""
  }

  plot <- ggplot(spct, aes_(~w.length, ~s.e.response))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

  # We want data plotted on top of the boundary lines
  # Negative response is valid!
  if ("boundaries" %in% annotations) {
    plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
  }

  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.rsp.label)

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
                            summary.label = rsp.label,
                            text.size = text.size,
                            na.rm = TRUE)

  if (!is.na(exposure.label)) {
    plot <- plot +  annotate("text",
                             x = min(spct),
                             y = y.max,
                             label = exposure.label,
                             vjust = -0.5,
                             hjust = 0,
                             size = rel(3),
                             na.rm = TRUE )
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.max * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
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

#' Create a complete ggplot for response spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a response_spct object.
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
q_rsp_plot <- function(spct,
                       w.band,
                       range,
                       pc.out,
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
  if (!is.response_spct(spct)) {
    stop("q_Rsp_plot() can only plot response_spct objects.")
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  e2q(spct, action="replace", byref=TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  exposure.label <- NA

  if (is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    s.rsp.label <- expression(Spectral~~photon~~response~~k %*% R[Q~lambda]~~("rel."))
    rsp.label.total <- "atop(k %*% R[Q], (\"rel.\"))"
    rsp.label.avg <- "atop(bar(k %*% R[Q~lambda]), (\"rel.\"))"
  } else if (is_normalized(spct)) {
    if (!pc.out) {
      multiplier.label <- "rel."
     } else {
      multiplier.label <- "%"
    }
    norm.ls <- photobiology::getNormalization(spct)
      norm.wl <- round(norm.ls[["norm.wl"]], digits = 1)
      s.rsp.label <-
        bquote(Spectral~~photon~~response~~R[Q~lambda]/R[Q~lambda==.(norm.wl)]~~(.(multiplier.label)))
      rsp.label.total  <- bquote(atop(integral(R[Q~lambda], min, max), (.(multiplier.label))))
      rsp.label.avg  <- bquote(atop(bar(R[Q~lambda]/R[Q~lambda==.(norm.wl)]), (.(multiplier.label))))
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized cps_spct objects.")
      pc.out <- FALSE
    }
    time.unit <- getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char=="second") {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(resp.~~unit~~s^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[Q], (resp.~~unit~~s^{-1}))"
      rsp.label.avg  <- "atop(bar(R[Q~lambda]), (resp.~~unit~~s^{-1}~nm^{-1}))"
    } else if (time.unit.char=="day") {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(resp.~~unit~~d^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[Q], (resp.~~unit~~d^{-1}))"
      rsp.label.avg  <- "atop(bar(R[Q~lambda]), (resp.~~unit~~d^{-1}~nm^{-1}))"
    } else if (time.unit.char=="hour") {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(resp.~~unit~~h^{-1}~nm^{-1}))
      rsp.label.total  <- "atop(R[Q], (resp.~~unit~~h^{-1}))"
      rsp.label.avg  <- "atop(bar(R[Q~lambda]), (resp.~~unit~~h^{-1}~nm^{-1}))"
    } else if (time.unit.char=="duration") {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(resp.~~unit~nm^{-1}))
      rsp.label.total  <- "atop(R[Q], (resp.~~unit))"
      rsp.label.avg  <- "atop(bar(R[Q~lambda]), (resp.~~unit~nm^{-1}))"
      exposure.label <- paste("Length of time:",
                              ifelse(lubridate::is.duration(time.unit),
                                     as.character(time.unit), "unknown"))
    } else if (time.unit.char=="exposure") {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(resp.~~unit~nm^{-1}))
      rsp.label.total  <- "atop(R[Q], (resp.~~unit))"
      rsp.label.avg  <- "atop(bar(R[Q~lambda]), (resp.~~unit~nm^{-1}))"
    } else {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q~lambda]~~(arbitrary~~units))
      rsp.label.total <- "atop(R[Q], (arbitrary~~units))"
      rsp.label.avg <- "atop(bar(R[Q~lambda]), (arbitrary~~units))"
    }
  }

  y.min <- ifelse(!is.na(ylim[1]),
                  ylim[1],
                  min(c(spct[["s.q.response"]], 0), na.rm = TRUE))
  y.max <- ifelse(!is.na(ylim[2]),
                  ylim[2],
                  max(c(spct[["s.q.response"]], 0), na.rm = TRUE))

  if (label.qty == "total") {
    rsp.label <- "integral(R[Q](lambda))"
  } else if (label.qty %in% c("average", "mean")) {
    rsp.label <- "bar(R[Q](lambda))"
  } else if (label.qty == "contribution") {
    rsp.label <- "atop(Contribution~~to~~total, R[Q]~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    rsp.label <- "atop(Contribution~~to~~total, R[Q]~~(\"%\"))"
  } else if (label.qty == "relative") {
    rsp.label <- "atop(Relative~~to~~sum, R[Q]~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    rsp.label <- "atop(Relative~~to~~sum, R[Q]~~(\"%\"))"
  } else {
    rsp.label <- ""
  }

  plot <- ggplot(spct, aes_(x = ~w.length, y = ~s.q.response))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

  # We want data plotted on top of the boundary lines
  # Negative response is valid!
  if ("boundaries" %in% annotations) {
    plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
  }

  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = "Wavelength (nm)", y = s.rsp.label)

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
                            summary.label = rsp.label,
                            text.size = text.size,
                            na.rm = TRUE)

  if (!is.na(exposure.label)) {
    plot <- plot +  annotate("text",
                             x = min(spct),
                             y = y.max,
                             label = exposure.label,
                             vjust = -0.5,
                             hjust = 0,
                             size = rel(3),
                             na.rm = TRUE )
  }

  if (abs(y.max - 1) < 0.02 && abs(y.min) < 0.02) {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    y.breaks <- scales::pretty_breaks(n = 5)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
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

#' Create a complete ggplot for a response spectrum.
#'
#' These methods return a ggplot object with an annotated plot of a
#' response_spct object or of the spectra contained in a response_mspct object.
#'
#' Note that scales are expanded so as to make space for the annotations. The
#' object returned is a ggplot object, and can be further manipulated and added
#' to.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @param object a response_spct object or a response_mspct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param norm numeric Normalization wavelength (nm) or character string "max",
#'   or "min" for normalization at the corresponding wavelength, "update" to
#'   update the normalization after modifying units of expression, quantity
#'   or range but respecting the previously used criterion, or "skip" to force
#'   return of \code{object} unchanged.
#' @param unit.out character string indicating type of radiation units to use
#'   for plotting: "photon" or its synomin "quantum", or "energy".
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
#' @param annotations a character vector. For details please see sections Plot
#'   Annotations and Title Annotations.
#' @param time.format character Format as accepted by
#'   \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param norm numeric normalization wavelength (nm) or character string \code{"max"}
#'   for normalization at the wavelength of highest peak, or \code{NULL} for
#'   plotting the spectrum as is.
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
#' @export
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{response_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @examples
#'
#' autoplot(photodiode.spct)
#' autoplot(photodiode.spct, unit.out = "photon")
#' autoplot(photodiode.spct, annotations = "")
#' autoplot(photodiode.spct, norm = "skip")
#' autoplot(photodiode.spct, norm = 400)
#'
#' two_sensors.mspct <-
#'  response_mspct(list("Photodiode" = photodiode.spct,
#'                      "Coupled charge device" = ccd.spct))
#' autoplot(two_sensors.mspct, normalize = TRUE, unit.out = "photon")
#' autoplot(two_sensors.mspct, normalize = TRUE, idfactor = "Spectra")
#' autoplot(two_sensors.mspct, normalize = TRUE, facets = 2)
#'
#' @family autoplot methods
#'
autoplot.response_spct <-
  function(object, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PAR())),
           range = NULL,
           norm = getOption("ggspectra.norm",
                            default = "max"),
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           pc.out = FALSE,
           label.qty = NULL,
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
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    # normalization needs to be redone if unit.out has changed
    object <- photobiology::normalize(x = object,
                                      range = range,
                                      norm = norm,
                                      unit.out = unit.out,
                                      na.rm = na.rm)
    if (is.null(label.qty)) {
      if (photobiology::is_normalized(object) ||
          photobiology::is_scaled(object)) {
        label.qty = "contribution"
      } else {
        label.qty = "total"
      }
    }
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- photobiology::waveband(object)
      } else if (photobiology::is.waveband(range)) {
        w.band <- range
      } else {
        w.band <- photobiology::waveband(range, wb.name = "Total")
      }
    }

    if (unit.out=="photon" || unit.out == "quantum") {
      out.ggplot <- q_rsp_plot(spct = object,
                               w.band = w.band,
                               range = range,
                               pc.out = pc.out,
                               label.qty = label.qty,
                               span = span,
                               wls.target = wls.target,
                               annotations = annotations,
                               norm = norm,              # read from object
                               text.size = text.size,
                               idfactor = idfactor,
                               facets = facets,
                               ylim = ylim,
                               na.rm = na.rm,
                               ...)
    } else if (unit.out=="energy") {
      out.ggplot <- e_rsp_plot(spct = object,
                               w.band = w.band,
                               range = range,
                               pc.out = pc.out,
                               label.qty = label.qty,
                               span = span,
                               wls.target = wls.target,
                               annotations = annotations, norm = norm,
                               text.size = text.size,
                               idfactor = idfactor,
                               facets = facets,
                               ylim = ylim,
                               na.rm = na.rm,
                               ...)
    } else {
      stop("Invalid 'unit.out' argument: '", unit.out, "'")
    }
    out.ggplot +
      autotitle(object = object,
                time.format = time.format,
                tz = tz,
                object.label = object.label,
                annotations = annotations)
  }

#' @rdname autoplot.response_spct
#'
#' @param plot.data character Data to plot. Default is "as.is" plotting one line
#'   per spectrum. When passing "mean", "median", "sum", "prod", "var", "sd",
#'   "se" as argument all the spectra must contain data at the same wavelength
#'   values.
#'
#' @export
#'
autoplot.response_mspct <-
  function(object,
           ...,
           range = NULL,
           norm = getOption("ggspectra.norm",
                            default = "max"),
           unit.out = getOption("photobiology.radiation.unit", default="energy"),
           plot.data = "as.is",
           idfactor = TRUE,
           na.rm = TRUE) {
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
                                        norm = norm,
                                        unit.out = unit.out,
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
    autoplot(object = z, range = NULL, idfactor = idfactor,
             norm = norm, unit.out = unit.out, na.rm = na.rm, ...)
  }

