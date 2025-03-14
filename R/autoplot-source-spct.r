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
#' @param annotations a character vector.
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
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
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
e_plot <- function(spct,
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
                   chroma.type,
                   idfactor,
                   facets,
                   ylim,
                   na.rm) {
  if (!photobiology::is.source_spct(spct)) {
    stop("e_plot() can only plot source_spct objects.")
  }
  spct[["s.q.irrad"]] <- NULL
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
  duration.label <- NA
  if (photobiology::is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }
    s.irrad.label <- "Spectral~~energy~~irradiance~~k %*% E[lambda]~~(\"rel.\")"
    irrad.label.total <- "atop(k %*% E, (\"rel.\"))"
    irrad.label.avg <- "atop(bar(E[lambda]), (\"rel.\"))"
    scale.factor <- 1
  } else if (photobiology::is_normalized(spct)) {
    if (!pc.out) {
      multiplier.label <- "rel."
    } else {
      multiplier.label <- "%"
    }
    norm <- round(photobiology::getNormalization(spct)[["norm.wl"]], digits = 1)
    s.irrad.label <- bquote(Spectral~~energy~~irradiance~~E[lambda]/E[lambda==.(norm)]~~(.(multiplier.label)))
    irrad.label.total <- "atop(E, (\"rel.\"))"
    irrad.label.avg <- bquote(atop(bar(E[lambda]), E[lambda==.(norm)]))
    scale.factor <- 1
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }
    time.unit <- photobiology::getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char == "second")  {
      s.irrad.label <- "Spectral~~energy~~irradiance~~E[lambda]~~(W~m^{-2}~nm^{-1})"
      irrad.label.total  <- "atop(E, (W~m^{-2}))"
      irrad.label.avg  <- "atop(bar(E[lambda]), (W~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else if (time.unit.char == "day") {
      s.irrad.label <- "Spectral~~energy~~exposure~~E[lambda]~~(MJ~d^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (MJ~d^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(E[lambda]), (MJ~d^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e-6
    } else if (time.unit.char == "hour") {
      s.irrad.label <- "Spectral~~energy~~exposure~~E[lambda]~~(kJ~h^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (kJ~h^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(E[lambda]), (kJ~h^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e-3
    } else if (time.unit.char == "duration") {
      s.irrad.label <- "Spectral~~energy~~fluence~~E[lambda]~~(kJ~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (kJ~m^{-2}))"
      irrad.label.avg <- "atop(bar(E[lambda]), (kJ~m^{-2}~nm^{-1}))"
      duration.label <- paste("Length of exposure:",
                              ifelse(lubridate::is.duration(time.unit),
                              as.character(time.unit), "unknown"))
      scale.factor <- 1e-3
    } else if (time.unit.char == "exposure") {
      s.irrad.label <- "Spectral~~energy~~fluence~~E[lambda]~~(J~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(E, (J~m^{-2}))"
      irrad.label.avg <- "atop(bar(E[lambda]), (J~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else {
      s.irrad.label <- "Spectral~~energy~~fluence~~E[lambda]~~(arbitrary~~units)"
      irrad.label.total <- "atop(E, (arbitrary~~units))"
      irrad.label.avg <- "atop(bar(E[lambda]), (arbitrary~~units))"
      scale.factor <- 1
    }
  }
  if (label.qty == "total") {
    irrad.label <- irrad.label.total
  } else if (label.qty %in% c("average", "mean")) {
    irrad.label <- irrad.label.avg
  }  else if (label.qty == "contribution") {
    irrad.label <- "atop(Contribution~~to~~total, E~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    irrad.label <- "atop(Contribution~~to~~total, E~~(\"%\"))"
  } else if (label.qty == "relative") {
    irrad.label <- "atop(Relative~~to~~sum, E~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    irrad.label <- "atop(Relative~~to~~sum, E~~(\"%\"))"
  } else {
    irrad.label <- ""
  }
  if (photobiology::is_effective(spct)) {
    s.irrad.label <- sub("E[lambda]", "E[lambda]^{eff}", s.irrad.label, fixed = TRUE)
    irrad.label <- sub("E", "E^{eff}", irrad.label, fixed = TRUE)
    irrad.label.total <- sub("E", "E^{eff}", irrad.label.total, fixed = TRUE)
    irrad.label.avg <- sub("E[lambda]", "E[lambda]^{eff}", irrad.label.avg, fixed = TRUE)
  }
  s.irrad.label <- parse(text = s.irrad.label)
  spct[["s.e.irrad"]] <- spct[["s.e.irrad"]] * scale.factor

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["s.e.irrad"]] <- ifelse(spct[["s.e.irrad"]] < y.min,
                                  NA_real_,
                                  spct[["s.e.irrad"]])
  } else {
    y.min <- min(spct[["s.e.irrad"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["s.e.irrad"]] <- ifelse(spct[["s.e.irrad"]] > y.max,
                                  NA_real_,
                                  spct[["s.e.irrad"]])
  } else {
    y.max <- max(spct[["s.e.irrad"]], y.min, 0, na.rm = TRUE)
  }

  plot <- ggplot2::ggplot(spct,
                          ggplot2::aes(x = .data[["w.length"]],
                                       y = .data[["s.e.irrad"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

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
  plot <- plot + ggplot2::labs(x = expression("Wavelength, "*lambda~(nm)), y = s.irrad.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <- plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  if (label.qty == "total") {
    label.qty <- "irrad"
  } else if (label.qty %in% c("mean", "average")) {
    label.qty <- "sirrad"
  }

  plot <- plot + decoration(w.band = w.band,
                            unit.out = "energy",
                            time.unit = photobiology::getTimeUnit(spct),
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = irrad.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (photobiology::is_effective(spct)) {
    plot <- plot +
      ggplot2::annotate("text",
                        x = photobiology::midpoint(spct),
                        y = y.max,
                        label = paste("BSWF:", photobiology::getBSWFUsed(spct)),
                        vjust = -0.5, size = ggplot2::rel(3),
                        na.rm = TRUE)
  }

  if (!is.na(duration.label)) {
    plot <- plot +
      ggplot2::annotate("text",
                        x = min(spct),
                        y = y.max,
                        label = duration.label,
                        vjust = -0.5,
                        hjust = 0,
                        size = ggplot2::rel(3),
                        na.rm = TRUE)
  }

  if (abs(y.max - 1) < 0.02 && abs(y.min) < 0.02) {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    y.breaks <- scales::pretty_breaks(n = 5)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max * 1.05)
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
  plot + ggplot2::scale_x_continuous(limits = x.limits, breaks = scales::pretty_breaks(n = 7))
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
#' @param annotations a character vector
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
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
#' @param facets logical Flag indicating if facets are to be created for the
#'   levels of \code{idfactor} when \code{spct} contain multiple spectra in long
#'   form.
#' @param ylim numeric y axis limits,
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
q_plot <- function(spct,
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
                   chroma.type,
                   idfactor,
                   facets,
                   ylim,
                   na.rm) {
  if (!photobiology::is.source_spct(spct)) {
    stop("q_plot() can only plot source_spct objects.")
  }
  spct[["s.e.irrad"]] <- NULL
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

  duration.label <- NA
  if (photobiology::is_scaled(spct)) {
    if (pc.out) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }
    s.irrad.label <- "Spectral~~photon~~exposure~~k %*% Q[lambda]~~(\"rel.\")"
    irrad.label.total <- "atop(k %*% Q, (\"rel.\"))"
    irrad.label.avg <- "atop(bar(Q[lambda]), (\"rel.\"))"
    scale.factor <- 1
  } else  if (photobiology::is_normalized(spct)) {
    if (!pc.out) {
      multiplier.label <- "rel."
    } else {
      multiplier.label <- "%"
    }
    norm <- round(photobiology::getNormalization(spct)[["norm.wl"]], digits = 1)
    s.irrad.label <- bquote(Spectral~~photon~~exposure~~Q[lambda]/Q[lambda==.(norm)]~~(.(multiplier.label)))
    irrad.label.total <- "atop(Q, (\"rel.\"))"
    irrad.label.avg <- bquote(atop(bar(Q[lambda]), Q[lambda==.(norm)]))
    scale.factor <- 1
  } else {
    if (pc.out) {
      warning("Percent scale supported only for normalized source_spct objects.")
      pc.out <- FALSE
    }
    time.unit <- photobiology::getTimeUnit(spct)
    if (!length(time.unit)) {
      time.unit <- "unkonwn"
    }
    time.unit.char <- duration2character(time.unit)
    if (time.unit.char=="second") {
      s.irrad.label <- "Spectral~~photon~~irradiance~~Q[lambda]~~(mu*mol~s^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total  <- "atop(Q, (mu*mol~s^{-1}~m^{-2}))"
      irrad.label.avg  <- "atop(bar(Q[lambda]), (mu*mol~s^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e6
    } else if (time.unit.char=="day") {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q[lambda]~~(mol~d^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~d^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q[lambda]), (mol~d^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else if (time.unit.char=="hour") {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q[lambda]~~(mmol~h^{-1}~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mmol~h^{-1}~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q[lambda]), (mmol~h^{-1}~m^{-2}~nm^{-1}))"
      scale.factor <- 1e3
    } else if (time.unit.char=="duration" || lubridate::is.duration(time.unit)) {
      s.irrad.label <- "Spectral~~photon~~fluence~~Q[lambda]~~(mol~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q[lambda]), (mol~m^{-2}~nm^{-1}))"
      duration.label <- paste("Length of exposure:",
                              ifelse(lubridate::is.duration(time.unit),
                                     as.character(time.unit), "unknown"))
      scale.factor <- 1
    } else if (time.unit.char=="exposure" || lubridate::is.duration(time.unit)) {
      s.irrad.label <- "Spectral~~photon~~fluence~~Q[lambda]~~(mol~m^{-2}~nm^{-1})"
      irrad.label.total <- "atop(Q, (mol~m^{-2}))"
      irrad.label.avg <- "atop(bar(Q[lambda]), (mol~m^{-2}~nm^{-1}))"
      scale.factor <- 1
    } else {
      s.irrad.label <- "Spectral~~photon~~exposure~~Q[lambda]~~(arbitrary~~units)"
      irrad.label.total <- "atop(Q, (arbitrary~~units))"
      irrad.label.avg <- "atop(bar(Q[lambda]), (arbitrary~~units))"
      scale.factor <- 1
    }
  }
  if (label.qty == "total") {
    irrad.label <- irrad.label.total
  } else if (label.qty %in% c("average", "mean")) {
    irrad.label <- irrad.label.avg
  } else if (label.qty == "contribution") {
    irrad.label <- "atop(Contribution~~to~~total, Q~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    irrad.label <- "atop(Contribution~~to~~total, Q~~(\"%\"))"
  } else if (label.qty == "relative") {
    irrad.label <- "atop(Relative~~to~~sum, Q~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    irrad.label <- "atop(Relative~~to~~sum, Q~~(\"%\"))"
  } else {
    irrad.label <- ""
  }
  if (photobiology::is_effective(spct)) {
    s.irrad.label <- sub("Q[lambda]", "Q[lambda]^{eff}", s.irrad.label, fixed = TRUE)
    irrad.label <- sub("Q", "Q^{eff}", irrad.label, fixed = TRUE)
    irrad.label.total <- sub("Q", "Q^{eff}", irrad.label.total, fixed = TRUE)
    irrad.label.avg <- sub("Q[lambda]", "Q[lambda]^{eff}", irrad.label.avg, fixed = TRUE)
  }
  s.irrad.label <- parse(text = s.irrad.label)
  spct[["s.q.irrad"]] <- spct[["s.q.irrad"]] * scale.factor

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["s.q.irrad"]] <- ifelse(spct[["s.q.irrad"]] < y.min,
                                  NA_real_,
                                  spct[["s.q.irrad"]])
  } else {
    y.min <- min(spct[["s.q.irrad"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["s.q.irrad"]] <- ifelse(spct[["s.q.irrad"]] > y.max,
                                  NA_real_,
                                  spct[["s.q.irrad"]])
  } else {
    y.max <- max(spct[["s.q.irrad"]], y.min, 0, na.rm = TRUE)
  }

  plot <-
    ggplot2::ggplot(spct,
                    ggplot2::aes(x = .data[["w.length"]],
                                 y = .data[["s.q.irrad"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp$ggplot_comp
  annotations <- temp$annotations

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
  plot <- plot +
    ggplot2::labs(x = expression("Wavelength, "*lambda~(nm)), y = s.irrad.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  if (label.qty == "total") {
    label.qty <- "irrad"
  } else if (label.qty %in% c("mean", "average")) {
    label.qty <- "sirrad"
  }

  plot <- plot + decoration(w.band = w.band,
                            unit.out = "photon",
                            time.unit = photobiology::getTimeUnit(spct),
                            y.max = y.max,
                            y.min = y.min,
                            x.max = max(spct),
                            x.min = min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = irrad.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (photobiology::is_effective(spct)) {
    plot <- plot +  ggplot2::annotate("text",
                                      x = photobiology::midpoint(spct),
                                      y = y.max,
                                      label = paste("BSWF:", photobiology::getBSWFUsed(spct)),
                                      vjust = -0.5, size = ggplot2::rel(3),
                                      na.rm = TRUE)
  }

  if (!is.na(duration.label)) {
    plot <- plot + ggplot2::annotate("text",
                                     x = min(spct),
                                     y = y.max,
                                     label = duration.label,
                                     vjust = -0.5,
                                     hjust = 0,
                                     size = ggplot2::rel(3),
                                     na.rm = TRUE)
  }

  if (abs(y.max - 1) < 0.02 && abs(y.min) < 0.02) {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    y.breaks <- scales::pretty_breaks(n = 5)
  }

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max * 1.05)
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

#' Plot one or more light-source spectra.
#'
#' These methods return a ggplot object with an annotated plot of the spectral
#' data contained in a \code{source_spct} or a \code{source_mspct} object.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @param object a source_spct or a source_mspct object.
#' @param ... in the case of collections of spectra, additional arguments passed
#'   to the plot methods for individual spectra, otherwise currently ignored.
#' @param w.band a single waveband object or a list of waveband objects.
#' @param range an R object on which \code{range()} returns a vector of length
#'   2, with minimum and maximum wavelengths (nm).
#' @param norm numeric or character. Normalization to apply before plotting, If
#'   \code{object} is already normalized, the normalization is updated when a
#'   unit conversion applied.
#' @param unit.out character string indicating type of radiation units to use
#'   for plotting: \code{"photon"} or its synonym \code{"quantum"}, or
#'   \code{"energy"}.
#' @param pc.out logical, if \code{TRUE} use percent instead of fraction of one
#'   for normalized spectral data.
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels, one of \code{"mean"}, \code{"total"}, \code{"contribution"},
#'   and \code{"relative"}.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centred at that
#'   element.
#' @param wls.target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings \code{"half.maximum"} and \code{"half.range"} are
#'   also accepted as arguments. A list with \code{numeric} and/or
#'   \code{character} values is also accepted.
#' @param annotations a character vector. For details please see sections Plot
#'   \strong{Annotations} and \strong{Title Annotations}.
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
#' @param time.format character Format as accepted by
#'   \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of \code{"CMF"} (color matching function) or
#'   \code{"CC"} (color coordinates) or a
#'   \code{\link[photobiology]{chroma_spct}} object.
#' @param idfactor character Name of an index \code{factor} used to identify
#'   each spectrum when multiple spectra are included in a plot. It is used as
#'   title to the guide in the plot and can include embedded spaces and new
#'   lines.
#' @param facets logical or integer Indicating if facets are to be created for
#'   the levels of \code{idfactor} when \code{spct} contain multiple spectra in
#'   long form.
#' @param plot.data character Data to plot. Default is \code{"as.is"} plotting
#'   one line per spectrum. When passing \code{"mean"}, \code{"median"},
#'   \code{"sum"}, \code{"prod"}, \code{"var"}, \code{"sd"}, \code{"se"} as
#'   argument all the spectra must contain data at the same wavelength values.
#' @param ylim numeric y axis limits,
#' @param object.label character The name of the object being plotted.
#' @param na.rm logical.
#'
#' @details The plot object returned is a ggplot (an object of class
#'   \code{"gg"}) and it can be added to or modified as any other ggplot. The
#'   axis labels are encoded as \emph{plotmath} expressions as they contain
#'   superscripts and special characters. In 'ggplot2', plotmath expressions do
#'   not obey theme settings related to text fonts, except for \code{size}.
#'
#'   Scale limits are expanded so as to make space for the annotations. If
#'   annotations are disabled, limits are not expanded unless
#'   \code{reserve.space} is passed to parameter \code{annotations}.
#'
#'   The generic of the \code{\link[ggplot2]{autoplot}} method is defined in
#'   package 'ggplot2'. Package 'ggspectra' defines specializations for the
#'   different classes for storage of spectral data defined in package
#'   \code{\link[photobiology]{photobiology}}.
#'
#'   For details about normalization and arguments to parameter \code{norm},
#'   please, see \code{\link[photobiology]{normalize}}. If \code{norm = NA},
#'   the default, \code{normalize()} is not called. All other values passed
#'   as argument to \code{norm} result in a call to \code{normalize()} with
#'   this value as its argument. In the case of objects
#'   created with 'photobiology' (<= 0.10.9) \code{norm = "undo"} is not
#'   supported. Be aware that calls to \code{normalize()} remove any scaling
#'   previously applied with \code{\link[photobiology]{fscale}} methods.
#'
#'   For multiple spectra in long form spectral objects, with \code{idfactor
#'   = NULL}, the default, the name of the factor is retrieved from metadata. If
#'   the character string passed as argument to \code{idfactor} does not match
#'   the one retrieved from the object, results in renaming of the pre-existing
#'   factor. The default for collections of spectra is to create a factor named
#'   \code{"spct.idx"}, but if a different name is passed, it will be used
#'   instead.
#'
#' @return A \code{ggplot} object with a number of layers that depends on the
#'   data and annotations. The \code{data} member retains its original class
#'   and metadata attributes.
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{source_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @export
#'
#' @examples
#'
#' autoplot(sun.spct)
#' autoplot(sun.spct, geom = "spct")
#' autoplot(sun.spct, unit.out = "photon")
#' autoplot(normalize(sun.spct))
#' autoplot(normalize(sun.spct), pc.out = TRUE)
#'
#' # multiple spectra in long form
#' autoplot(sun_evening.spct)
#' autoplot(sun_evening.spct, facets = 1) # one column
#' autoplot(sun_evening.spct, facets = 2) # two columns
#' autoplot(sun_evening.spct, plot.data = "mean")
#' autoplot(sun_evening.spct, idfactor = "Sequence")
#'
#' # multiple spectra as a collection
#' autoplot(sun_evening.mspct)
#' autoplot(sun_evening.mspct, facets = 1) # one column
#' autoplot(sun_evening.mspct, facets = 2) # two columns
#' autoplot(sun_evening.mspct, plot.data = "mean")
#' autoplot(sun_evening.mspct, idfactor = "Time")
#'
#' @family autoplot methods
#'
autoplot.source_spct <-
  function(object,
           ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(photobiologyWavebands::UVC(),
                                             photobiologyWavebands::UVB(),
                                             photobiologyWavebands::UVA(),
                                             photobiologyWavebands::PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           pc.out = getOption("ggspectra.pc.out", default = FALSE),
           label.qty = NULL,
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           by.group = FALSE,
           geom = "line",
           time.format = "",
           tz = "UTC",
           text.size = 2.5,
           chroma.type = "CMF",
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

    if (photobiology::getMultipleWl(object) > 1L && plot.data != "as.is") {
      return(
        ggplot2::autoplot(object = photobiology::subset2mspct(object),
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
                          chroma.type = chroma.type,
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
                default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    # Change units if needed, and update normalization
    object <- switch(unit.out,
                     photon = photobiology::e2q(object, action = "replace"),
                     energy = photobiology::q2e(object, action = "replace"))

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
    if (photobiology::is.waveband(w.band)) {
      w.band <- list(w.band)
    }
    labels <- sapply(w.band, labels)[1, ]
    if (unit.out %in% c("photon", "quantum")) {
      # change "PhR" label into "PAR" because we compute photon irradiance
      wb.PAR <- grep("^PhR$", labels)
      if (length(wb.PAR)) {
        w.band[[wb.PAR]] <-
          photobiology::waveband(x = c(400, 700), wb.name = "PAR")
      }
    }

    if (unit.out %in% c("photon", "quantum")) {
      out.ggplot <- q_plot(spct = object,
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
                           chroma.type = chroma.type,
                           idfactor = idfactor,
                           facets = facets,
                           ylim = ylim,
                           na.rm = na.rm)
    } else if (unit.out == "energy") {
      out.ggplot <- e_plot(spct = object,
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
                           chroma.type = chroma.type,
                           idfactor = idfactor,
                           facets = facets,
                           ylim = ylim,
                           na.rm = na.rm)
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
#' @export
#'
autoplot.source_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
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
    # we ensure the units are correct
    object <- switch(unit.out,
                     photon = photobiology::e2q(object, action = "replace"),
                     energy = photobiology::q2e(object, action = "replace"))
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

    col.name <- c(photon = "s.q.irrad", energy = "s.e.irrad")
    if (photobiology::is.source_spct(z) && any(col.name %in% names(z))) {
      ggplot2::autoplot(object = z,
                        range = NULL, # trimmed above
                        unit.out = unit.out,
                        pc.out = pc.out,
                        idfactor = NULL, # use idfactor already set in z
                        by.group = by.group,
                        facets = facets,
                        object.label = object.label,
                        na.rm = na.rm,
                        ...)
    } else {
      z <- photobiology::as.generic_spct(z)
      ggplot2::autoplot(object = z,
                        y.name = paste(col.name[unit.out], plot.data, sep = "."),
                        range = NULL, # trimmed above
                        pc.out = pc.out,
                        idfactor = NULL, # use idfactor already set in z
                        by.group = by.group,
                        facets = facets,
                        object.label = object.label,
                        na.rm = na.rm,
                        ...)
    }
  }

