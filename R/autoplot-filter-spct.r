#' Create a complete ggplot for a filter spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a filter_spct
#' object showing absorptance.
#'
#' @param spct a filter_spct object.
#' @param w.band list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param pc.out logical, if TRUE use percents instead of fraction of one.
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels.
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
#' @param geom character.
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
Afr_plot <- function(spct,
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
                     na.rm,
                     ...) {
  if (!photobiology::is.filter_spct(spct)) {
    stop("Afr_plot() can only plot filter_spct objects.")
  }

  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  force(spct)
  spct <- photobiology::any2Afr(spct, action = "add")
  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <-
      photobiology::trim_wl(w.band, range = photobiology::wl_range(spct))
  }

  if (photobiology::is_scaled(spct)) {
    if (pc.out) {
      warning("Percent not supported for scaled spectral data.")
      pc.out <- FALSE
    }
    scale.factor <- 1
    s.Afr.label <- expression(Spectral~~absorptance~~italic(A)[lambda]~~("rel."))
    Afr.label.total  <- "atop(italic(A), (rel))"
    Afr.label.avg  <- "atop(bar(italic(A)[lambda]), (rel))"
  } else if (photobiology::is_normalized(spct)) {
    warning("Plotting of normalized absorptance not supported")
    return(ggplot2::ggplot())
  } else if (pc.out) {
    scale.factor <- 100
    s.Afr.label <- expression(Spectral~~absorptance~~italic(A)[lambda]~~("%"))
    Afr.label.total  <- "atop(italic(A), (total %*% 100))"
    Afr.label.avg  <- "atop(bar(italic(A)[lambda]), (\"%\"))"
  } else {
    scale.factor <- 1
    s.Afr.label <- expression(Spectral~~absorptance~~italic(A)[lambda]~~("/1"))
    Afr.label.total  <- "atop(italic(A), (\"/1\"))"
    Afr.label.avg  <- "atop(bar(italic(A)[lambda]), (\"/1\"))"
  }
  if (label.qty == "total") {
    Afr.label <- Afr.label.total
  } else if (label.qty %in% c("average", "mean")) {
    Afr.label <- Afr.label.avg
  } else if (label.qty == "contribution") {
    Afr.label <- "atop(Contribution~~to~~total, italic(A)~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    Afr.label <- "atop(Contribution~~to~~total, italic(A)~~(\"%\"))"
  } else if (label.qty == "relative") {
    Afr.label <- "atop(Relative~~to~~sum, italic(A)~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    Afr.label <- "atop(Relative~~to~~sum, italic(A)~~(\"%\"))"
  } else {
    Afr.label <- ""
  }

  if (any(!is.na(ylim))) {
    y.breaks <- scales::pretty_breaks(n = 6)
  } else {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["Afr"]] <- ifelse(spct[["Afr"]] < y.min,
                            NA_real_,
                            spct[["Afr"]])
  } else {
    y.min <- min(spct[["Afr"]], 0, na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["Afr"]] <- ifelse(spct[["Afr"]] > y.max,
                            NA_real_,
                            spct[["Afr"]])
  } else {
    y.max <- max(spct[["Afr"]], 1, y.min, na.rm = TRUE)
  }

  plot <-
    ggplot2::ggplot(spct,
                    ggplot2::aes(x = .data[["w.length"]], y = .data[["Afr"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp[["ggplot_comp"]]
  annotations <- temp[["annotations"]]

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.max > 1.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "black")
    }
    if (y.min < -0.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <-
      plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <-
    plot + ggplot2::geom_line(na.rm = na.rm)
  plot <-
    plot + ggplot2::labs(x = expression("Wavelength, "*lambda~(nm)),
                         y = s.Afr.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            label.mult = scale.factor,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = photobiology::wl_max(spct),
                            x.min = photobiology::wl_min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = Afr.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (!is.null(annotations) &&
      length(intersect(c("labels", "summaries", "colour.guide", "reserve.space"),
                       annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(photobiology::wl_min(spct) -
                    photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- photobiology::wl_range(spct)
  }

  if (pc.out) {
    plot <-
      plot + ggplot2::scale_y_continuous(labels = scales::percent,
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

#' Create a complete ggplot for a filter spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a filter_spct
#' object showing transmittance.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a filter_spct object
#' @param w.band list of waveband objects
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm)
#' @param pc.out logical, if TRUE use percents instead of fraction of one
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels
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
#' @param geom character.
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
T_plot <- function(spct,
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
                   na.rm,
                   ylim,
                   ...) {
  if (!photobiology::is.filter_spct(spct)) {
    stop("T_plot() can only plot filter_spct objects.")
  }
  if (!is.null(geom) && !geom %in% c("area", "line", "spct")) {
    warning("'geom = ", geom, "' not supported, using default instead.")
    geom <- NULL
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  force(spct)
  spct <- photobiology::any2T(spct, action = "replace")
  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  Tfr.type <- photobiology::getTfrType(spct)
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = photobiology::wl_range(spct))
  }
  if (!length(Tfr.type)) {
    Tfr.type <- "unknown"
  }
  Tfr.tag <- switch(Tfr.type,
                    internal = "int",
                    total = "tot",
                    unknown = "",
                    NA_character_)
  Tfr.name <- switch(Tfr.type,
                     internal = "Internal",
                     total = "Total",
                     unknown = "Unknown-type",
                     NA_character_)

  if (photobiology::is_scaled(spct)) {
    if (pc.out) {
      warning("Percent not supported for scaled spectral data.")
      pc.out <- FALSE
    }
    scale.factor <- 1
    s.Tfr.label <- bquote(.(Tfr.name)~~spectral~~transmittance~~k %*% T[lambda]^{.(Tfr.tag)}~~("rel."))
    Tfr.label.total  <- paste("k %*% T^{", Tfr.tag,"}", sep = "")
    Tfr.label.avg  <- paste("bar(k %*% T[lambda]^{", Tfr.tag, "})", sep = "")
  } else if (photobiology::is_normalized(spct)) {
    if (pc.out) {
      warning("Percent not supported for normalized spectral data.")
      pc.out <- FALSE
    }
    scale.factor <- 1
    norm <- round(photobiology::getNormalization(spct)[["norm.wl"]], 1)
    s.Tfr.label <- bquote(.(Tfr.name)~~spectral~~transmittance~~T[lambda]^{.(Tfr.tag)}/T[lambda==.(norm)]^{.(Tfr.tag)}~~("rel."))
    Tfr.label.total  <- paste("atop(T^{", Tfr.tag,
                              "}, T[lambda == ", norm, "]^{", Tfr.tag, "}",
                              sep = "")
    Tfr.label.avg  <- paste("atop(bar(T[lambda]^{", Tfr.tag,
                            "}), T[lambda == ", norm, "]^{", Tfr.tag, "}",
                            sep = "")
  } else if (!pc.out) {
    scale.factor <- 1
    s.Tfr.label <- bquote(.(Tfr.name)~~spectral~~transmittance~~T[lambda]^{.(Tfr.tag)}~~("/1"))
    Tfr.label.total  <- paste("atop(T^{", Tfr.tag, "} (\"/1\"))", sep = "")
    Tfr.label.avg  <- paste("atop(bar(T[lambda]^{", Tfr.tag, "}), (\"/1\"))", sep = "")
  } else if (pc.out) {
    scale.factor <- 100
    s.Tfr.label <- bquote(.(Tfr.name)~~spectral~~transmittance~~T[lambda]^{.(Tfr.tag)}~~("%"))
    Tfr.label.total  <- paste("atop(T^{", Tfr.tag, "}, (total %*% 100))", sep = "")
    Tfr.label.avg  <- paste("atop(bar(T[lambda]^{",  Tfr.tag, "}), (\"%\"))", sep = "")
  }
  if (label.qty == "total") {
    Tfr.label <- Tfr.label.total
  } else if (label.qty %in% c("average", "mean")) {
    Tfr.label <- Tfr.label.avg
  } else if (label.qty == "contribution") {
    Tfr.label <- "atop(Contribution~~to~~total, T~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    Tfr.label <- "atop(Contribution~~to~~total, T~~(\"%\"))"
  } else if (label.qty == "relative") {
    Tfr.label <- "atop(Relative~~to~~sum, T~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    Tfr.label <- "atop(Relative~~to~~sum, T~~(\"%\"))"
  } else {
    Tfr.label <- ""
  }

  if (any(!is.na(ylim))) {
    y.breaks <- scales::pretty_breaks(n = 6)
  } else {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["Tfr"]] <- ifelse(spct[["Tfr"]] < y.min,
                            NA_real_,
                            spct[["Tfr"]])
  } else {
    y.min <- min(c(spct[["Tfr"]], 0), na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["Tfr"]] <- ifelse(spct[["Tfr"]] > y.max,
                            NA_real_,
                            spct[["Tfr"]])
  } else {
    y.max <- max(c(spct[["Tfr"]], 1), na.rm = TRUE)
  }

  plot <-
    ggplot2::ggplot(spct,
                    ggplot2::aes(x = .data[["w.length"]], y = .data[["Tfr"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp[["ggplot_comp"]]
  annotations <- temp[["annotations"]]

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.max > 1.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "black")
    }
    if (y.min < -0.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + ggplot2::geom_line(na.rm = na.rm)
  plot <- plot + labs(x = expression("Wavelength, "*lambda~(nm)), y = s.Tfr.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            label.mult = scale.factor,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = photobiology::wl_max(spct),
                            x.min = photobiology::wl_min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = Tfr.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (!is.null(annotations) &&
      length(intersect(c("labels", "summaries", "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- range(spct)
  }
  if (pc.out) {
    plot <-
      plot + ggplot2::scale_y_continuous(labels = scales::percent, breaks = y.breaks,
                                         limits = y.limits)
  } else {
    plot <-
      plot + ggplot2::scale_y_continuous(breaks = y.breaks,
                                               limits = y.limits)
  }

  plot +
    ggplot2::scale_x_continuous(limits = x.limits,
                                breaks = scales::pretty_breaks(n = 7))

}

#' Create a complete ggplot for a filter spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a filter_spct
#' object showing spectral absorbance.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a filter_spct object
#' @param w.band list of waveband objects
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm)
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels
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
#' @param geom character.
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
A_plot <- function(spct,
                   w.band,
                   range,
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
                   na.rm,
                   ylim,
                   ...) {
  if (!photobiology::is.filter_spct(spct)) {
    stop("A_plot() can only plot filter_spct objects.")
  }
  if (!is.null(geom) && !geom %in% c("area", "line", "spct")) {
    warning("'geom = ", geom, "' not supported, using default instead.")
    geom <- NULL
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  spct <- photobiology::any2A(spct, action = "replace")
  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- photobiology::trim_wl(w.band, range = photobiology::wl_range(spct))
  }
  Tfr.type <- photobiology::getTfrType(spct)
  if (!length(Tfr.type)) {
    Tfr.type <- "unknown"
  }
  Tfr.tag <- switch(Tfr.type,
                    internal = "int",
                    total = "tot",
                    unknown = "",
                    NA_character_)
  Tfr.name <- switch(Tfr.type,
                     internal = "Internal",
                     total = "Total",
                     unknown = "Unknown-type",
                     NA_character_)
  if (photobiology::is_scaled(spct)) {
    s.A.label <- bquote(.(Tfr.name)~~spectral~~absorbance~~k %*% A[lambda]^{.(Tfr.tag)}~~("rel."))
    A.label.total  <- paste("k %*% A^{", Tfr.tag, "}", sep = "")
    A.label.avg  <- paste("bar(k %*% A[lambda]^{", Tfr.tag, "})", sep = "")
  } else if (photobiology::is_normalized(spct)) {
    norm <- round(getNormalization(spct)[["norm.wl"]], 1)
    s.A.label <- bquote(.(Tfr.name)~~spectral~~absorbance~~A[lambda]^{.(Tfr.tag)}/A[lambda==.(norm)]^{.(Tfr.tag)}~~("rel."))
    A.label.total  <- paste("atop(A^{", Tfr.tag,
                              "}, A[lambda == ", norm, "]^{", Tfr.tag, "}",
                              sep = "")
    A.label.avg  <- paste("atop(bar(A[lambda]^{", Tfr.tag,
                            "}), A[lambda == ", norm, "]^{", Tfr.tag, "}",
                            sep = "")
  } else {
    s.A.label <- bquote(.(Tfr.name)~~spectral~~absorbance~~A[lambda]^{.(Tfr.tag)}~~(AU))
    A.label.total  <- paste("atop(A^{", Tfr.tag, "}, (AU %*% nm))", sep = "")
    A.label.avg  <- paste("atop(bar(A[lambda]^{", Tfr.tag, "}), (AU))", sep = "")
  }

  if (label.qty == "total") {
    A.label <- A.label.total
  } else if (label.qty %in% c("average", "mean")) {
    A.label <- A.label.avg
  } else if (label.qty == "contribution") {
    A.label <- "atop(Contribution~~to~~total, A~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    A.label <- "atop(Contribution~~to~~total, A~~(\"%\"))"
  } else if (label.qty == "relative") {
    A.label <- "atop(Relative~~to~~sum, A~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    A.label <- "atop(Relative~~to~~sum, A~~(\"%\"))"
  } else {
    A.label <- ""
  }

  y.breaks <- scales::pretty_breaks(n = 6)

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["A"]] <- ifelse(spct[["A"]] < y.min,
                          NA_real_,
                          spct[["A"]])
  } else {
    y.min <- min(c(spct[["A"]], 0), na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["A"]] <- ifelse(spct[["A"]] > y.max,
                          NA_real_,
                          spct[["A"]])
  } else {
    y.max <- max(spct[["A"]], na.rm = TRUE)
  }

  plot <-
    ggplot2::ggplot(spct,
                    ggplot2::aes(x = .data[["w.length"]], y = .data[["A"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp[["ggplot_comp"]]
  annotations <- temp[["annotations"]]

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.max > 6) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 6,
                                   linetype = "dashed", colour = "red")
    }
    if (y.min < -0.01) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + geom_line(na.rm = na.rm)
  plot <- plot + labs(x = expression("Wavelength, "*lambda~(nm)), y = s.A.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            y.max = min(y.max, 6),
                            y.min = y.min,
                            x.max = photobiology::wl_max(spct),
                            x.min = photobiology::wl_min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = A.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "summaries",
                         "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, min(y.max, 6) * 1.25)
    x.limits <- c(min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, min(y.max, 6))
    x.limits <- range(spct)
  }
  plot <- plot + ggplot2::scale_y_continuous(limits = y.limits)
  plot + ggplot2::scale_x_continuous(limits = x.limits,
                                     breaks = scales::pretty_breaks(n = 7))

}

#' Create a complete ggplot for a reflector spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a reflector_spct
#' reflectance.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a filter_spct object
#' @param w.band list of waveband objects
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm)
#' @param pc.out logical, if TRUE use percents instead of fraction of one
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels
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
#' @param geom character.
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
R_plot <- function(spct,
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
                   na.rm,
                   ...) {
  if (!photobiology::is.reflector_spct(spct)) {
    stop("R_plot() can only plot reflector_spct objects.")
  }
  if (!is.null(geom) && !geom %in% c("area", "line", "spct")) {
    warning("'geom = ", geom, "' not supported, using default instead.")
    geom <- NULL
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  # delete other columns to optimize object size and code performance
  extra.columns <- intersect(colnames(spct), c("Tfr", "Afr", "A"))
  for (c in extra.columns) {
    spct[[c]] <- NULL
  }
  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- photobiology::trim_wl(w.band, range = range(spct))
  }
  Rfr.type <- photobiology::getRfrType(spct)
  if (length(Rfr.type) == 0) {
    Rfr.type <- "unknown"
  }

  Rfr.tag <- switch(Rfr.type,
                    specular = "spc",
                    total = "tot",
                    unknown = "",
                    NA_character_)
  Rfr.name <- switch(Rfr.type,
                     specular = "Specular",
                     total = "Total",
                     unknown = "Unknown-type",
                     NA_character_)

  if (photobiology::is_scaled(spct)) {
    scale.factor <- 1
    s.Rfr.label <- bquote(.(Rfr.name)~~spectral~~reflectance~~k %*% R[lambda]^{.(Rfr.tag)}~~("rel."))
    Rfr.label.total  <- paste("k %*% R^{", Rfr.tag, "}", sep = "")
    Rfr.label.avg  <- paste("bar(k %*% R[lambda]^{", Rfr.tag, "})", sep = "")
  } else if (photobiology::is_normalized(spct)) {
    norm <- round(photobiology::getNormalization(spct)[["norm.wl"]], 1)
    s.Rfr.label <- bquote(.(Rfr.name)~~spectral~~reflectance~~R[lambda]^{.(Rfr.tag)}/R[lambda==.(norm)]^{.(Rfr.tag)}~~("rel."))
    Rfr.label.total  <- paste("atop(R^{", Rfr.tag,
                            "}, R[lambda == ", norm, "]^{", Rfr.tag, "})",
                            sep = "")
    Rfr.label.avg  <- paste("atop(bar(R[lambda]^{", Rfr.tag,
                          "}), R[lambda == ", norm, "]^{", Rfr.tag, "})",
                          sep = "")
  } else  if (!pc.out) {
    scale.factor <- 1
      s.Rfr.label <- bquote(.(Rfr.name)~~spectral~~reflectance~~R[lambda]^{.(Rfr.tag)}~~("/1"))
      Rfr.label.total  <- paste("atop(R^{", Rfr.tag, "}, (\"/1\"))", sep = "")
      Rfr.label.avg  <- paste("atop(bar(R[lambda]^{", Rfr.tag, "}), (\"/1\"))", sep = "")
  } else if (pc.out) {
    scale.factor <- 100
      s.Rfr.label <- bquote(.(Rfr.name)~~spectral~~reflectance~~R[lambda]^{.(Rfr.tag)}~~("%"))
      Rfr.label.total  <- paste("atop(R^{", Rfr.tag, "}, (total %*% 100))", sep = "")
      Rfr.label.avg  <- paste("atop(bar(R[lambda]^{", Rfr.tag, "}), (\"%\"))", sep = "")
  }
  if (label.qty == "total") {
    Rfr.label <- Rfr.label.total
  } else if (label.qty %in% c("average", "mean")) {
    Rfr.label <- Rfr.label.avg
  } else if (label.qty == "contribution") {
    Rfr.label <- "atop(Contribution~~to~~total, R~~(\"/1\"))"
  } else if (label.qty == "contribution.pc") {
    Rfr.label <- "atop(Contribution~~to~~total, R~~(\"%\"))"
  } else if (label.qty == "relative") {
    Rfr.label <- "atop(Relative~~to~~sum, R~~(\"/1\"))"
  } else if (label.qty == "relative.pc") {
    Rfr.label <- "atop(Relative~~to~~sum, R~~(\"%\"))"
  } else {
    Rfr.label <- ""
  }

  if (any(!is.na(ylim))) {
    y.breaks <- scales::pretty_breaks(n = 6)
  } else {
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  }

  if (!is.na(ylim[1])) {
    y.min <- ylim[1]
    spct[["Rfr"]] <- ifelse(spct[["Rfr"]] < y.min,
                            NA_real_,
                            spct[["Rfr"]])
  } else {
    y.min <- min(c(spct[["Rfr"]], 0), na.rm = TRUE)
  }

  if (!is.na(ylim[2])) {
    y.max <- ylim[2]
    spct[["Rfr"]] <- ifelse(spct[["Rfr"]] > y.max,
                            NA_real_,
                            spct[["Rfr"]])
  } else {
    y.max <- max(c(spct[["Rfr"]], 1), na.rm = TRUE)
  }

  plot <- ggplot2::ggplot(spct, aes(x = .data[["w.length"]], y = .data[["Rfr"]]))
  temp <- find_idfactor(spct = spct,
                        idfactor = idfactor,
                        facets = facets,
                        map.linetype = !facets && !by.group,
                        annotations = annotations)
  plot <- plot + temp[["ggplot_comp"]]
  annotations <- temp[["annotations"]]

  # We want data plotted on top of the boundary lines
  if ("boundaries" %in% annotations) {
    if (y.max > 1.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 1,
                                   linetype = "dashed", colour = "black")
    }
    if (y.min < -0.005) {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "red")
    } else {
      plot <-
        plot + ggplot2::geom_hline(yintercept = 0,
                                   linetype = "dashed", colour = "black")
    }
  }

  if (!is.null(geom) && geom %in% c("area", "spct")) {
    plot <- plot + geom_spct(fill = "black", colour = NA, alpha = 0.2)
  }
  plot <- plot + ggplot2::geom_line(na.rm = na.rm)
  plot <- plot + labs(x = expression("Wavelength, "*lambda~(nm)),
                      y = s.Rfr.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  plot <- plot + decoration(w.band = w.band,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = photobiology::wl_max(spct),
                            x.min = photobiology::wl_min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = Rfr.label,
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)

  if (!is.null(annotations) &&
      length(intersect(c("labels", "summaries", "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <- c(min(spct) - photobiology::wl_expanse(spct) * 0.025, NA) # NA needed because of rounding errors
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- photobiology::wl_range(spct)
  }
  if (pc.out) {
    plot <-
      plot + ggplot2::scale_y_continuous(labels = scales::percent,
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

#' Create a complete ggplot for a object spectrum.
#'
#' This function returns a ggplot object with an annotated plot of an object_spct
#' displaying spectral transmittance, absorptance and reflectance.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot object, and can be further manipulated.
#'
#' @param spct an object_spct object.
#' @param w.band list of waveband objects.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param pc.out logical, if TRUE use percents instead of fraction of one.
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels.
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
#' @param geom character.
#' @param stacked logical.
#' @param text.size numeric size of text in the plot decorations.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param na.rm logical.
#' @param ylim numeric y axis limits,
#'
#' @return a \code{ggplot} object.
#'
#' @keywords internal
#'
O_plot <- function(spct,
                   w.band,
                   range,
                   pc.out,
                   label.qty,
                   span,
                   wls.target,
                   annotations,
                   by.group,
                   geom,
                   stacked,
                   text.size,
                   chroma.type,
                   facets,
                   na.rm,
                   ylim) {
  if (!photobiology::is.object_spct(spct)) {
    stop("O_plot() can only plot object_spct objects.")
  }
  if (photobiology::getMultipleWl(spct) > 1L && (is.null(facets) || !facets)) {
    warning("Only one object spectrum per panel supported")
    facets <- TRUE
  } else if (is.null(facets)) {
    facets <- FALSE
  }
  if (stacked) {
    # we need to ensure that none of Tfr, Rfr and Afr are negative
    spct <- photobiology::clean(spct)
  }
  if (!is.null(geom) && !geom %in% c("area", "line", "spct")) {
    warning("'geom = ", geom, "' not supported, using default instead.")
    geom <- NULL
  }
  if (is.null(ylim) || !is.numeric(ylim)) {
    ylim <- rep(NA_real_, 2L)
  }
  if (stacked && !all(is.na(ylim))) {
    warning("'ylim' not supported for stacked plots!")
  }
  if (!is.null(range)) {
    spct <- photobiology::trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- photobiology::trim_wl(w.band,
                                    range = photobiology::wl_range(spct))
  }
  Rfr.type <- getRfrType(spct)
  if (length(Rfr.type) == 0) {
    Rfr.type <- "unknown"
  }
  Tfr.type <- getTfrType(spct)
  if (length(Tfr.type) == 0) {
    Tfr.type <- "unknown"
  }
  if (Rfr.type != "total") {
    warning("Only 'total' reflectance can be meaningfully plotted in a combined plot")
  }
  if (Tfr.type == "internal") {
#    warning("Internal transmittance converted to total transmittance")
    spct <- photobiology::convertTfrType(spct, Tfr.type = "total")
  }
  s.Rfr.label <- expression(atop(Spectral~~reflectance~R[lambda]~~absorptance~~A[lambda], and~~transmittance~T[lambda]))
  spct[["Afr"]] <- 1.0 - spct[["Tfr"]] - spct[["Rfr"]]
  if (any((spct[["Afr"]]) < -0.01)) {
    message("Bad data or fluorescence.")
    if (stacked) {
      warning("Changing mode to not stacked")
      stacked <- FALSE
    }
  }

  if (stacked) {
    y.max <- 1.01 # take care of rounding off
    y.min <- -0.01 # take care of rounding off
    y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
  } else {
    if (!is.na(ylim[1])) {
      y.min <- ylim[1]
      spct[["Rfr"]] <- ifelse(spct[["Rfr"]] < y.min,
                              NA_real_,
                              spct[["Rfr"]])
      spct[["Tfr"]] <- ifelse(spct[["Tfr"]] < y.min,
                              NA_real_,
                              spct[["Tfr"]])
      spct[["Afr"]] <- ifelse(spct[["Afr"]] < y.min,
                              NA_real_,
                              spct[["Afr"]])
    } else {
      y.min <- min(0, spct[["Rfr"]], spct[["Tfr"]], spct[["Afr"]], na.rm = TRUE)
    }

    if (any(!is.na(ylim))) {
      y.breaks <- scales::pretty_breaks(n = 6)
    } else {
      y.breaks <- c(0, 0.25, 0.5, 0.75, 1)
    }

    if (!is.na(ylim[2])) {
      y.max <- ylim[2]
      spct[["Rfr"]] <- ifelse(spct[["Rfr"]] > y.max,
                              NA_real_,
                              spct[["Rfr"]])
      spct[["Tfr"]] <- ifelse(spct[["Tfr"]] > y.max,
                              NA_real_,
                              spct[["Tfr"]])
      spct[["Afr"]] <- ifelse(spct[["Afr"]] > y.max,
                              NA_real_,
                              spct[["Afr"]])
    } else {
      y.max <- max(1, y.min, spct[["Rfr"]], spct[["Tfr"]], spct[["Afr"]], na.rm = TRUE)
    }
  }

  idfactor <-
    photobiology::getIdFactor(spct) # needed as we will get a tibble back
  molten.tb <-
    photobiology::spct_wide2long(spct, idfactor = "variable", rm.spct.class = TRUE)

  plot <-
    ggplot2::ggplot(molten.tb, aes(x = .data[["w.length"]], y = .data[["value"]]))
  attributes(plot[["data"]]) <- c(attributes(plot[["data"]]),
                                  photobiology::get_attributes(spct))
  if (stacked) {
    if (is.null(geom) || geom %in% c("spct", "area")) {
      plot <-
        plot + geom_area(aes(alpha = .data[["variable"]]),
                         fill = "black", colour = NA)
      plot <-
        plot +
        ggplot2::scale_alpha_manual(values = c(Tfr = 0.4,
                                               Rfr = 0.25,
                                               Afr = 0.55),
                                    breaks = c("Rfr", "Afr", "Tfr"),
                                    labels = c(Tfr = expression(T[lambda]),
                                               Afr = expression(A[lambda]),
                                               Rfr = expression(R[lambda])),
                                    guide = guide_legend(title = NULL))
    } else {
      plot <-
        plot + ggplot2::geom_line(aes(linetype = .data[["variable"]]),
                                  position = ggplot2::position_stack())
      plot <-
        plot +
        ggplot2::scale_linetype(labels = c(Tfr = expression(T[lambda]),
                                           Afr = expression(A[lambda]),
                                           Rfr = expression(R[lambda])),
                                guide = guide_legend(title = NULL))
    }
  } else {
    if (!is.null(geom) && geom %in% c("spct", "area")) {
      plot <- plot + geom_spct(aes(group = .data[["variable"]]),
                               fill = "black", colour = NA, alpha = 0.2)
    }
    plot <-
      plot +
      ggplot2::geom_line(aes(linetype = .data[["variable"]]))
    plot <-
      plot +
      ggplot2::scale_linetype(labels = c(Tfr = expression(T[lambda]),
                                         Afr = expression(A[lambda]),
                                         Rfr = expression(R[lambda])),
                              guide = ggplot2::guide_legend(title = NULL))
  }
  if (is.numeric(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets = vars(.data[[idfactor]]),
                          ncol = as.integer(facets))
  } else if (facets) {
    plot <- plot +
      ggplot2::facet_wrap(facets = vars(.data[[idfactor]]))
  }
  plot <-
    plot + ggplot2::labs(x = expression("Wavelength, "*lambda~(nm)), y = s.Rfr.label)

  if (length(annotations) == 1 && annotations == "") {
    return(plot)
  }

  plot <-
    plot + ggplot2::scale_fill_identity() + ggplot2::scale_color_identity()

  valid.annotations <- c("labels", "boxes", "segments", "colour.guide", "reserve.space")
  if (!stacked) {
    valid.annotations <- c(valid.annotations, "peaks", "valleys", "peak.labels", "valley.labels")
  }
  annotations <- intersect(annotations, valid.annotations)

  plot <- plot + decoration(w.band = w.band,
                            y.max = y.max,
                            y.min = y.min,
                            x.max = photobiology::wl_max(spct),
                            x.min = photobiology::wl_min(spct),
                            annotations = annotations,
                            by.group = by.group,
                            label.qty = label.qty,
                            span = span,
                            wls.target = wls.target,
                            summary.label = "",
                            text.size = text.size,
                            chroma.type = chroma.type,
                            na.rm = TRUE)
  if (!is.null(annotations) &&
      length(intersect(c("boxes", "segments", "labels", "colour.guide", "reserve.space"), annotations)) > 0L) {
    y.limits <- c(y.min, y.min + (y.max - y.min) * 1.25)
    x.limits <-
      c(photobiology::wl_min(spct) - photobiology::wl_expanse(spct) * 0.025, NA)
  } else {
    y.limits <- c(y.min, y.max)
    x.limits <- photobiology::wl_range(spct)
  }
  if (pc.out) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent,
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

# autoplot methods -------------------------------------------------------------

#' Plot one or more "filter" spectra.
#'
#' These methods return a ggplot object of an annotated plot from spectral data
#' contained in a \code{filter_spct} or a \code{filter_mspct} object. Data can
#' be expressed as absorbance, absorptance or transmittance.
#'
#' @note The plotting of absorbance is an exception to scale limits as the
#'   \emph{y}-axis is not extended past 6 a.u. In the case of absorbance, values
#'   larger than 6 a.u. are rarely meaningful due to stray light during
#'   measurement. However, when transmittance values below the detection limit
#'   are rounded to zero, and later converted into absorbance, values Inf a.u.
#'   result, disrupting the plot. Scales are further expanded so as to make
#'   space for the annotations.
#'
#'   If \code{idfactor = NULL}, the default for single spectra, the name of the
#'   factor is retrieved from metadata or if no metadata found, the default
#'   \code{"spct.idx"} is tried. The default for multiple spectra is to create a
#'   factor named \code{"spct.idx"}, but if a different name is passed, it will
#'   be used instead, possibly renaminig a pre-existing one.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object a filter_spct object or a filter_mspct object.
#' @param plot.qty character string one of "transmittance" or "absorbance".
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{filter_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @export
#'
#' @examples
#' # one spectrum
#' autoplot(yellow_gel.spct)
#' autoplot(yellow_gel.spct, geom = "spct")
#' autoplot(yellow_gel.spct, plot.qty = "transmittance")
#' autoplot(yellow_gel.spct, plot.qty = "absorbance")
#' autoplot(yellow_gel.spct, pc.out = TRUE)
#' autoplot(yellow_gel.spct, annotations = c("+", "wls"))
#'
#' # spectra for two filters in long form
#' autoplot(two_filters.spct)
#' autoplot(two_filters.spct, idfactor = TRUE)
#' autoplot(two_filters.spct, idfactor = "Spectra")
#' autoplot(two_filters.spct, facets = TRUE)
#'
#' # spectra for two filters as a collection
#' autoplot(two_filters.mspct)
#' autoplot(two_filters.mspct, idfactor = "Spectra")
#' autoplot(two_filters.mspct, facets = TRUE)
#'
#' @family autoplot methods
#'
autoplot.filter_spct <-
  function(object, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = getOption("photobiology.filter.qty",
                                default = "transmittance"),
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
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

    if (plot.data != "as.is") {
      return(
        autoplot(object = photobiology::subset2mspct(object),
                 w.band = w.band,
                 range = range,
                 plot.qty = plot.qty,
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
    if (is.null(label.qty)) {
      if (photobiology::is_normalized(object) ||
          photobiology::is_scaled(object)) {
        label.qty = "contribution"
      } else {
        label.qty = "average"
      }
    }
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- photobiology::waveband(object)
      } else if (photobiology::is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  photobiology::waveband(range, wb.name = "Total")
      }
    }

    if (plot.qty == "transmittance") {
      out.ggplot <- T_plot(spct = object,
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
    } else if (plot.qty == "absorbance") {
      out.ggplot <- A_plot(spct = object,
                           w.band = w.band,
                           range = range,
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
    } else if (plot.qty == "absorptance") {
      out.ggplot <- Afr_plot(spct = object,
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
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
    out.ggplot +
      autotitle(object = object,
                time.format = time.format,
                tz = tz,
                object.label = object.label,
                annotations = annotations)
  }

#' @rdname autoplot.filter_spct
#'
#' @export
#'
autoplot.filter_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = getOption("photobiology.filter.qty",
                                default = "transmittance"),
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
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
      object <- photobiology::trim_wl(object, range = range, use.hinges = TRUE, fill = NULL)
    }
    # we convert spectra to the quantity to be plotted
    object <- switch(plot.qty,
                     transmittance = any2T(object, action = "replace"),
                     absorbance = any2A(object, action = "replace"),
                     # we need to discard T´Rfr if present
                     absorptance = msmsply(any2Afr(object, action = "replace"),
                                           `[`, i = TRUE, j =  c("w.length", "Afr")))
    # we convert the collection of spectra into a single spectrum object
    # containing a summary spectrum or multiple spectra in long form.
    z <- switch(plot.data,
                as.is = photobiology::rbindspct(object,
                                                idfactor = idfactor),
                mean = photobiology::s_mean(object),
                median = photobiology::s_median(object),
                sum = photobiology::s_sum(object),
                prod = photobiology::s_prod(object),
                var = photobiology::s_var(object),
                sd = photobiology::s_sd(object),
                se = photobiology::s_se(object)
    )
    col.name <- c(transmittance = "Tfr", absorptance = "Afr", absorbance = "A")
    if (photobiology::is.filter_spct(z) && col.name[plot.qty] %in% names(z)) {
      autoplot(object = z,
               range = NULL, # trimmed above
               plot.qty = plot.qty,
               pc.out = pc.out,
               idfactor = NULL, # use idfactor already set in z
               by.group = by.group,
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else {
      Tfr.type <- getTfrType(z)
      z <- as.generic_spct(z)
      autoplot(object = z,
               y.name = paste(col.name[plot.qty], plot.data, sep = "."),
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

#' Plot one or more reflector spectra.
#'
#' These methods return a ggplot object for an annotated plot from spectral data
#' stored in a \code{reflector_spct} or a \code{reflector_mspct} object.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object a reflector_spct object or a reflector_mspct object.
#' @param plot.qty character string (currently ignored).
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{reflector_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @export
#'
#' @examples
#'
#' autoplot(Ler_leaf_rflt.spct)
#' autoplot(Ler_leaf_rflt.spct, geom = "spct")
#' autoplot(Ler_leaf_rflt.spct, annotations = c("+", "valleys"))
#'
#' two_leaves.mspct <-
#'  reflector_mspct(list("Arabidopsis leaf 1" = Ler_leaf_rflt.spct,
#'                       "Arabidopsis leaf 2" = Ler_leaf_rflt.spct / 2))
#' autoplot(two_leaves.mspct)
#' autoplot(two_leaves.mspct, idfactor = "Spectra")
#' autoplot(two_leaves.mspct, facets = 2)
#'
#' @family autoplot methods
#'
autoplot.reflector_spct <-
  function(object, ...,
           w.band=getOption("photobiology.plot.bands",
                            default = list(UVC(), UVB(), UVA(), PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = getOption("photobiology.reflector.qty",
                                default = "reflectance"),
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
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

    if (plot.data != "as.is") {
      return(
        autoplot(object = photobiology::subset2mspct(object),
                 w.band = w.band,
                 range = range,
                 plot.qty = plot.qty,
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

    if (is.null(label.qty)) {
      if (photobiology::is_normalized(object) ||
          photobiology::is_scaled(object)) {
        label.qty = "contribution"
      } else {
        label.qty = "average"
      }
    }
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- photobiology::waveband(object)
      } else if (photobiology::is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  photobiology::waveband(range, wb.name = "Total")
      }
    }
    if (plot.qty == "reflectance") {
      out.ggplot <- R_plot(spct = object,
                           w.band = w.band,
                           range = range,
                           pc.out = pc.out,
                           label.qty = label.qty,
                           span = span,
                           wls.target = wls.target,
                           annotations = annotations,
                           by.group = by.group,
                           text.size = text.size,
                           geom = geom,
                           chroma.type = chroma.type,
                           idfactor = idfactor,
                           facets = facets,
                           ylim = ylim,
                           na.rm = na.rm,
                           ...)
    } else {
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
    out.ggplot +
      autotitle(object = object,
                time.format = time.format,
                tz = tz,
                object.label = object.label,
                annotations = annotations)
  }

#' @rdname autoplot.reflector_spct
#'
#' @export
#'
autoplot.reflector_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = getOption("photobiology.reflector.qty",
                                default = "reflectance"),
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
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
    if (photobiology::is.reflector_spct(z) && "Rfr" %in% names(z)) {
      autoplot(object = z,
               range = NULL, # trimmed above
               plot.qty = plot.qty,
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
               y.name = paste("Rfr", plot.data, sep = "."),
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

#' Plot one or more "object" spectra.
#'
#' These methods return a ggplot object with an annotated plot of an
#' \code{object_spct} or an \code{object_spct} object. This objects contain
#' spectral transmittance, reflectance and possibly absorptance data. As these
#' quantities add up to one, only two are needed.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#' @inherit autoplot.source_spct
#'
#' @param object an object_spct object
#' @param plot.qty character string, one of "all", "transmittance",
#'   "absorbance", "absorptance", or "reflectance".
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}. The default \code{NULL}
#'   selects between them based on \code{stacked}.
#' @param stacked logical Whether to use \code{position_stack()} or
#'   \code{position_identity()}.
#'
#' @note In the case of multiple spectra contained in the argument to
#'   \code{object} plotting is for \code{plot.qty = "all"} is always done using
#'   facets. Other plot quantities are handled by the methods for
#'   \code{filter_spct} and \code{reflector_spct} objects after on-the-fly
#'   conversion and the use of facets is possible but not the default.
#'
#'   If \code{idfactor = NULL}, the default for single spectra, the name of the
#'   factor is retrieved from metadata or if no metadata found, the default
#'   "spct.idx" is tried. The default for multiple spectra is to create a factor
#'   named "spct.idx", but if a different name is passed, it will be used
#'   instead, possibly renaminig a pre-existing one.
#'
#' @export
#'
#' @seealso \code{\link[photobiology]{normalize}},
#'   \code{\link[photobiology]{object_spct}},
#'   \code{\link[photobiology]{waveband}},
#'   \code{\link[photobiologyWavebands]{photobiologyWavebands-package}} and
#'   \code{\link[ggplot2]{autoplot}}
#'
#' @examples
#'
#' low_res.spct <- thin_wl(Ler_leaf.spct,
#'                         max.wl.step = 20,
#'                         max.slope.delta = 0.01,
#'                         col.names = "Tfr")
#' autoplot(low_res.spct)
#' autoplot(low_res.spct, geom = "line")
#'
#' two_leaves.mspct <-
#'   object_mspct(list("Arabidopsis leaf 1" = low_res.spct,
#'                     "Arabidopsis leaf 2" = low_res.spct))
#' autoplot(two_leaves.mspct, idfactor = "Spectra")
#'
#' @family autoplot methods
#'
autoplot.object_spct <-
  function(object,
           ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PhR())),
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = "all",
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
           label.qty = NULL,
           span = NULL,
           wls.target = "HM",
           annotations = NULL,
           by.group = FALSE,
           geom = NULL,
           time.format = "",
           tz = "UTC",
           stacked = plot.qty == "all",
           text.size = 2.5,
           chroma.type = "CMF",
           idfactor = NULL,
           facets = NULL,
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
        autoplot(object = photobiology::subset2mspct(object, idfactor = idfactor),
                 w.band = w.band,
                 range = range,
                 plot.qty = plot.qty,
                 pc.out = pc.out,
                 label.qty = label.qty,
                 span = span,
                 wls.target = wls.target,
                 annotations = annotations,
                 by.group = by.group,
                 geom = geom,
                 time.format = time.format,
                 tz = tz,
                 stacked = stacked,
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

    if (is.null(plot.qty)) {
      plot.qty <- "all"
    }
    if (is.null(facets)) {
      facets <- plot.qty == "all" && photobiology::getMultipleWl(object) > 1L
    }

    if (plot.qty == "all") {
      # stacked area plot
      annotations.default <-
        getOption("photobiology.plot.annotations",
                  default = c("boxes", "labels", "colour.guide", "peaks"))
      annotations <- decode_annotations(annotations,
                                        annotations.default)
      if (is.null(label.qty)) {
        if (photobiology::is_normalized(object) ||
            photobiology::is_scaled(object)) {
          label.qty = "contribution"
        } else {
          label.qty = "average"
        }
      }
      if (length(w.band) == 0) {
        if (is.null(range)) {
          w.band <- photobiology::waveband(object)
        } else if (photobiology::is.waveband(range)) {
          w.band <- range
        } else {
          w.band <-  photobiology::waveband(range, wb.name = "Total")
        }
      }
      out.ggplot <- O_plot(spct = object,
                           w.band = w.band,
                           range = range,
                           pc.out = pc.out,
                           label.qty = label.qty,
                           span = span,
                           wls.target = wls.target,
                           annotations = annotations,
                           by.group = by.group,
                           geom = geom,
                           stacked = stacked,
                           text.size = text.size,
                           chroma.type = chroma.type,
                           facets = facets,
                           ylim = ylim,
                           na.rm = na.rm)
      out.ggplot +
        autotitle(object = object,
                  time.format = time.format,
                  tz = tz,
                  object.label = object.label,
                  annotations = annotations)
    } else {
      # Line plots for components: we convert object and call respective method
      if (plot.qty == "reflectance") {
        object <- photobiology::as.reflector_spct(object)
      } else if (plot.qty %in%
                   c("absorbance", "absorptance", "transmittance")) {
        object <- photobiology::as.filter_spct(object)
      } else {
        stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
      }
      autoplot(object = object,
               w.band = w.band,
               range = range,
               plot.qty = plot.qty,
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
               ylim = ylim,
               object.label = object.label,
               na.rm = na.rm)
    }
  }

#' @rdname autoplot.object_spct
#'
#' @export
#'
autoplot.object_mspct <-
  function(object,
           ...,
           range = getOption("ggspectra.wlrange", default = NULL),
           norm = NA,
           plot.qty = getOption("photobiology.filter.qty",
                                default = "all"),
           pc.out = getOption("ggspectra.pc.out",
                              default = FALSE),
           by.group = FALSE,
           plot.data = "as.is",
           idfactor = TRUE,
           facets = plot.qty == "all",
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    force(object.label)
    object <- apply_normalization(object, norm)
    idfactor <- check_idfactor_arg(object, idfactor = idfactor, default = TRUE)

    # facets will be forced later for "all" with a warning
    if (plot.qty == "reflectance") {
      object <- photobiology::as.reflector_mspct(object)
    } else if (plot.qty != "all") {
      object <- photobiology::as.filter_mspct(object)
    }
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
    col.name <- c(transmittance = "Tfr", absorptance = "Afr", reflectance = "Rfr")
    if ((photobiology::is.object_spct(z) && sum(col.name %in% names(z)) >= 2) ||
        (photobiology::is.filter_spct(z) && any(c("Tfr", "Afr", "A")) %in% names(z)) ||
        (photobiology::is.reflector_spct(z) && "Rfr" %in% names(z)))  {
      autoplot(object = z,
               range = NULL, # trimmed above
               plot.qty = plot.qty,
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else if (photobiology::is.filter_spct(z) && !any(col.name %in% names(z))) {
      z <- photobiology::as.generic_spct(z)
      autoplot(object = z,
               y.name = paste(col.name[plot.qty], plot.data, sep = "."),
               range = NULL, # trimmed above
               pc.out = pc.out,
               by.group = by.group,
               idfactor = NULL, # use idfactor already set in z
               facets = facets,
               object.label = object.label,
               na.rm = na.rm,
               ...)
    } else if (photobiology::is.reflector_spct(z) && !"Rfr" %in% names(z)) {
      z <- photobiology::as.generic_spct(z)
      autoplot(object = z,
               y.name = paste("Rfr", plot.data, sep = "."),
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
