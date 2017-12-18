#' Plot method for response spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a response_spct object
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
#' @param ... other arguments passed to e_response()
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
                       annotations,
                       norm,
                       text.size,
                       na.rm,
                       ...) {
  if (!is.response_spct(spct)) {
    stop("e_Rsp_plot() can only plot response_spct objects.")
  }
  q2e(spct, action="replace", byref=TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  exposure.label <- NA
  if (is.null(pc.out) || is.null(norm)) {
    # no rescaling needed
    if (is_normalized(spct) || is_scaled(spct)) {
      s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(relative~~units))
      rsp.label.total <- "atop(R[E], (relative~~units))"
      rsp.label.avg <- "atop(bar(R[E](lambda)), (relative~~units))"
      scale.factor <- 1
    } else {
      time.unit <- getTimeUnit(spct)
      if (!length(time.unit)) {
        time.unit <- "unkonwn"
      }
      if (time.unit=="second" || time.unit == lubridate::duration(1, "seconds")) {
        s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(resp.~~unit~~s^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[E], (resp.~~unit~~s^{-1}))"
        rsp.label.avg  <- "atop(bar(R[E](lambda)), (resp.~~unit~~s^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="day" || time.unit == lubridate::duration(1, "days")) {
        s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(resp.~~unit~~d^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[E], (resp.~~unit~~d^{-1}))"
        rsp.label.avg  <- "atop(bar(R[E](lambda)), (resp.~~unit~~d^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="hour" || time.unit == lubridate::duration(1, "hours")) {
        s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(resp.~~unit~~h^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[E], (resp.~~unit~~h^{-1}))"
        rsp.label.avg  <- "atop(bar(R[E](lambda)), (resp.~~unit~~h^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="exposure" || lubridate::is.duration(time.unit)) {
        s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(resp.~~unit~nm^{-1}))
        rsp.label.total  <- "atop(R[E], (resp.~~unit))"
        rsp.label.avg  <- "atop(bar(R[E](lambda)), (resp.~~unit~nm^{-1}))"
        exposure.label <- paste("Length of time:",
                                ifelse(lubridate::is.duration(time.unit),
                                       as.character(time.unit), "unknown"))
        scale.factor <- 1
      } else {
        s.rsp.label <- expression(Spectral~~energy~~response~~R[E](lambda)~~(arbitrary~~units))
        rsp.label.total <- "atop(R[E], (arbitrary~~units))"
        rsp.label.avg <- "atop(bar(R[E](lambda)), (arbitrary~~units))"
        scale.factor <- 1
      }
    }
  } else {
    # rescaling needed
    if (!is.null(norm)) {
      if (is.character(norm)) {
        if (norm %in% c("max", "maximum")) {
          idx <- which.max(spct[["s.e.response"]])
        } else if (norm %in% c("min", "minimum")) {
          idx <- which.min(spct[["s.e.response"]])
        } else if (norm %in% c("mean", "average")) {
          summary.value <- average_spct(spct)
          idx <- "summary"
        } else if (norm %in% c("total", "integral")) {
          summary.value <- integrate_spct(spct)
          idx <- "summary"
        } else {
          warning("Invalid character '", norm, "'value in 'norm'")
          return(ggplot())
        }
        if (idx == "summary") {
          scale.factor <- 1 / summary.value
        } else {
          scale.factor <- 1 / as.numeric(spct[idx, "s.e.response"])
          norm <- as.numeric(spct[idx, "w.length"])
        }
      } else if (is.numeric(norm) && norm >= min(spct) && norm <= max(spct)) {
        scale.factor <- 1 / interpolate_spct(spct, norm)[["s.e.response"]]
      } else if (is.numeric(norm)) {
        warning("'norm = ", norm, "' value outside spectral data range of ",
                min(spct), " to ", round(max(spct)), " (nm)")
        return(ggplot())
      } else {
        stop("'norm' should be numeric or character")
      }
    }
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
    s.rsp.label <-
      bquote(Spectral~~energy~~response~~R[E]( italic(lambda) )/R[E]( .(norm))~~(.(multiplier.label)))
    rsp.label.total  <- bquote(atop(integral(R[E](lambda), min, max), (.(multiplier.label))))
    rsp.label.avg  <- bquote(atop(bar(R[E](lambda)/R[E](lambda = norm)), (.(multiplier.label))))
  }
  spct[["s.e.response"]] <- spct[["s.e.response"]] * scale.factor
  y.max <- max(c(spct[["s.e.response"]], 0), na.rm = TRUE)
  y.min <- min(c(spct[["s.e.response"]], 0), na.rm = TRUE)

  if (label.qty == "total") {
    rsp.label <- "integral(R[E](lambda))"
  } else if (label.qty %in% c("average", "mean")) {
    rsp.label <- "bar(R[E](lambda))"
  } else if (label.qty == "contribution") {
    rsp.label <- "atop(Contribution~~to~~total, R[E]~~(fraction))"
  } else if (label.qty == "contribution.pc") {
    rsp.label <- "atop(Contribution~~to~~total, R[E]~~(percent))"
  } else if (label.qty == "relative") {
    rsp.label <- "atop(Relative~~to~~sum, R[E]~~(fraction))"
  } else if (label.qty == "relative.pc") {
    rsp.label <- "atop(Relative~~to~~sum, R[E]~~(percent))"
  } else {
    rsp.label <- ""
  }

  plot <- ggplot(spct, aes_(~w.length, ~s.e.response))

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
    x.limits <- c(min(spct) - spread(spct) * 0.025, NA) # NA needed because of rounding errors
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

#' Plot a response spectrum.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param spct a response_spct object
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
#' @param ... other arguments passed to q_response()
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
                       annotations,
                       norm,
                       text.size,
                       na.rm,
                       ...) {
  if (!is.response_spct(spct)) {
    stop("q_Rsp_plot() can only plot response_spct objects.")
  }
  e2q(spct, action="replace", byref=TRUE)
  if (!is.null(range)) {
    spct <- trim_wl(spct, range = range)
  }
  if (!is.null(w.band)) {
    w.band <- trim_wl(w.band, range = range(spct))
  }

  exposure.label <- NA
  if (is.null(pc.out) || is.null(norm)) {
    # no rescaling needed
    if (is_normalized(spct) || is_scaled(spct)) {
      s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(relative~~units))
      rsp.label.total <- "atop(R[Q], (relative~~units))"
      rsp.label.avg <- "atop(bar(R[Q](lambda)), (relative~~units))"
      scale.factor <- 1
    } else {
      time.unit <- getTimeUnit(spct)
      if (!length(time.unit)) {
        time.unit <- "unkonwn"
      }
      if (time.unit=="second" || time.unit == lubridate::duration(1, "seconds")) {
        s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(resp.~~unit~~s^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[Q], (resp.~~unit~~s^{-1}))"
        rsp.label.avg  <- "atop(bar(R[Q](lambda)), (resp.~~unit~~s^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="day" || time.unit == lubridate::duration(1, "days")) {
        s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(resp.~~unit~~d^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[Q], (resp.~~unit~~d^{-1}))"
        rsp.label.avg  <- "atop(bar(R[Q](lambda)), (resp.~~unit~~d^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="hour" || time.unit == lubridate::duration(1, "hours")) {
        s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(resp.~~unit~~h^{-1}~nm^{-1}))
        rsp.label.total  <- "atop(R[Q], (resp.~~unit~~h^{-1}))"
        rsp.label.avg  <- "atop(bar(R[Q](lambda)), (resp.~~unit~~h^{-1}~nm^{-1}))"
        scale.factor <- 1
      } else if (time.unit=="exposure" || lubridate::is.duration(time.unit)) {
        s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(resp.~~unit~nm^{-1}))
        rsp.label.total  <- "atop(R[Q], (resp.~~unit))"
        rsp.label.avg  <- "atop(bar(R[Q](lambda)), (resp.~~unit~nm^{-1}))"
        exposure.label <- paste("Length of time:",
                                ifelse(lubridate::is.duration(time.unit),
                                       as.character(time.unit), "unknown"))
        scale.factor <- 1
      } else {
        s.rsp.label <- expression(Spectral~~photon~~response~~R[Q](lambda)~~(arbitrary~~units))
        rsp.label.total <- "atop(R[Q], (arbitrary~~units))"
        rsp.label.avg <- "atop(bar(R[Q](lambda)), (arbitrary~~units))"
        scale.factor <- 1
      }
    }
  } else {
    # rescaling needed
    if (!is.null(norm)) {
      if (is.character(norm)) {
        if (norm %in% c("max", "maximum")) {
          idx <- which.max(spct[["s.q.response"]])
        } else if (norm %in% c("min", "minimum")) {
          idx <- which.min(spct[["s.q.response"]])
        } else if (norm %in% c("mean", "average")) {
          summary.value <- average_spct(spct)
          idx <- "summary"
        } else if (norm %in% c("total", "integral")) {
          summary.value <- integrate_spct(spct)
          idx <- "summary"
        } else {
          warning("Invalid character '", norm, "'value in 'norm'")
          return(ggplot())
        }
        if (idx == "summary") {
          scale.factor <- 1 / summary.value
        } else {
          scale.factor <- 1 / as.numeric(spct[idx, "s.q.response"])
          norm <- as.numeric(spct[idx, "w.length"])
        }
      } else if (is.numeric(norm) && norm >= min(spct) && norm <= max(spct)) {
        scale.factor <- 1 / interpolate_spct(spct, norm)$s.q.response
      } else if (is.numeric(norm)) {
        warning("'norm = ", norm, "' value outside spectral data range of ",
                min(spct), " to ", round(max(spct)), " (nm)")
        return(ggplot())
      } else {
        stop("'norm' should be numeric or character")
      }
    }
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
    s.rsp.label <-
      bquote(Spectral~~photon~~response~~R[Q]( italic(lambda) )/R[Q]( .(norm))~~(.(multiplier.label)))
    rsp.label.total  <- bquote(atop(integral(R[Q](lambda), min, max), (.(multiplier.label))))
    rsp.label.avg  <- bquote(atop(bar(R[Q](lambda)/R[Q](lambda = norm)), (.(multiplier.label))))
  }
  spct[["s.q.response"]] <- spct[["s.q.response"]] * scale.factor
  y.max <- max(c(spct[["s.q.response"]], 0), na.rm = TRUE)
  y.min <- min(c(spct[["s.q.response"]], 0), na.rm = TRUE)

  if (label.qty == "total") {
    rsp.label <- "integral(R[Q](lambda))"
  } else if (label.qty %in% c("average", "mean")) {
    rsp.label <- "bar(R[Q](lambda))"
  } else if (label.qty == "contribution") {
    rsp.label <- "atop(Contribution~~to~~total, R[Q]~~(fraction))"
  } else if (label.qty == "contribution.pc") {
    rsp.label <- "atop(Contribution~~to~~total, R[Q]~~(percent))"
  } else if (label.qty == "relative") {
    rsp.label <- "atop(Relative~~to~~sum, R[Q]~~(fraction))"
  } else if (label.qty == "relative.pc") {
    rsp.label <- "atop(Relative~~to~~sum, R[Q]~~(percent))"
  } else {
    rsp.label <- ""
  }

  plot <- ggplot(spct, aes_(~w.length, ~s.q.response))

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
    x.limits <- c(min(spct) - spread(spct) * 0.025, NA) # NA needed because of rounding errors
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

#' Plot method for response spectra.
#'
#' This function returns a ggplot object with an annotated plot of a
#' response_spct object.
#'
#' @note Note that scales are expanded so as to make space for the annotations.
#'   The object returned is a ggplot objects, and can be further manipulated.
#'
#' @param x a response_spct object
#' @param ... other arguments passed along, such as \code{label.qty}
#' @param w.band a single waveband object or a list of waveband objects
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm)
#' @param unit.out character string indicating type of radiation units to use
#'   for plotting: "photon" or its synomin "quantum", or "energy"
#' @param pc.out logical, if TRUE use percents instead of fraction of one
#' @param label.qty character string giving the type of summary quantity to use
#'   for labels, one of "mean", "total", "contribution", and "relative".
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element.
#' @param annotations a character vector
#' @param time.format character Format as accepted by \code{\link[base]{strptime}}.
#' @param tz character Time zone to use for title and/or subtitle.
#' @param norm numeric normalization wavelength (nm) or character string "max"
#'   for normalization at the wavelength of highest peak.
#' @param text.size numeric size of text in the plot decorations.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @export
#'
#' @keywords hplot
#'
#' @examples
#' library(photobiology)
#' plot(photodiode.spct)
#' plot(photodiode.spct, unit.out = "photon")
#'
#'
#' @family plot functions
#'
plot.response_spct <-
  function(x, ...,
           w.band = getOption("photobiology.plot.bands",
                              default = list(UVC(), UVB(), UVA(), PAR())),
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit", default="energy"),
           pc.out = FALSE,
           label.qty = NULL,
           span = NULL,
           annotations = NULL,
           time.format = "",
           tz = "UTC",
           norm = "max",
           text.size = 2.5,
           na.rm = TRUE) {
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations,
                                      annotations.default)
    if (is.null(label.qty)) {
      if (is_normalized(x) || is_scaled(x)) {
        label.qty = "contribution"
      } else {
        label.qty = "total"
      }
    }
    if (length(w.band) == 0) {
      if (is.null(range)) {
        w.band <- waveband(x)
      } else if (is.waveband(range)) {
        w.band <- range
      } else {
        w.band <-  waveband(range, wb.name = "Total")
      }
    }

    if (unit.out=="photon" || unit.out == "quantum") {
      out.ggplot <- q_rsp_plot(spct = x,
                               w.band = w.band,
                               range = range,
                               pc.out = pc.out,
                               label.qty = label.qty,
                               span = span,
                               annotations = annotations,
                               norm = norm,
                               text.size = text.size,
                               na.rm = na.rm,
                               ...)
    } else if (unit.out=="energy") {
      out.ggplot <- e_rsp_plot(spct = x,
                               w.band = w.band,
                               range = range,
                               pc.out = pc.out,
                               label.qty = label.qty,
                               span = span,
                               annotations = annotations, norm = norm,
                               text.size = text.size,
                               na.rm = na.rm,
                               ...)
    } else {
      stop("Invalid 'unit.out' argument value: '", unit.out, "'")
    }
    out.ggplot +
      ggtitle_spct(x = x,
                   time.format = time.format,
                   tz = tz,
                   x.name = deparse(substitute(x)),
                   annotations = annotations)
  }


