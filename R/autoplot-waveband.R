#' Create a complete ggplot for a waveband descriptor.
#'
#' Construct a ggplot object with an annotated plot of a \code{waveband} object.
#'
#' @details A \code{response_spct} object is created based on the
#'   \code{waveband} object and the argument passed to parameter \code{w.length}.
#'   By default wavelengths spanning the waveband definition expanded by
#'   1 nm at each end are used. A \code{waveband} object can describe either a
#'   simple wavelength range or a (biological) spectral weighting function
#'   (BSWF). An effectiveness is a response expressed per unit of excitation,
#'   and in most cases normalised.
#'
#'   Effectiveness spectra can be plotted expressing the spectral effectiveness
#'   either per mol of photons (\eqn{1 mol^{-1} nm}) or per joule of energy
#'   (\eqn{1 J^{-1} nm}), selected through formal argument \code{unit.out}. The
#'   value of \code{unit.in} has no direct effect on the result for BSWFs,
#'   as BSWFs are defined based on a certain base of expression, which is
#'   enforced. Indirectly it affects plots as \code{unit.out} defaults to
#'   \code{unit.in}. In contrast, for wavebands which only define a wavelength
#'   range, changing the assumed reference irradiance units, changes the
#'   responsivity according to Plank's law, i.e., the four possible combinations
#'   of pairs of values for \code{unit.in} and \code{unit.out} produce four
#'   different plots. _Only in rare cases \code{unit.in} and \code{unit.out}
#'   is useful as normally the dependency on base of expresion is encoded in
#'   the \code{waveband} definition as a BSWF._
#'
#'   While \code{w.length}` provides the wavelengths of the generated
#'   response spectrum, `range` sets the wavelength range of the \eqn{x}-axis
#'   of the plot.
#'
#'   Unused named arguments are forwarded to
#'   \code{\link{autoplot.response_spct}()}, allowing control of additional plot
#'   properties.
#'
#' @inheritSection decoration Plot Annotations
#'
#' @param object a waveband object.
#' @param ... arguments passed along by name to \code{autoplot.response_spct()}.
#' @param w.length numeric vector of wavelengths (nm).
#' @param range an R object on which range() returns a vector of length 2, with
#'   min annd max wavelengths (nm).
#' @param fill value to use as response for wavelngths outside the waveband
#'   range.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element.
#' @param wls.target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings "half.maximum" and "half.range" are also accepted
#'   as arguments. A list with \code{numeric} and/or \code{character} values is
#'   also accepted.
#' @param unit.in,unit.out the type of unit we assume as reference: "energy" or
#'   "photon" based for the waveband definition and the implicit matching
#'   response plotted.
#' @param annotations a character vector. For details please see section Plot
#'   Annotations.
#' @param by.group logical flag If TRUE repeated identical annotation layers are
#'   added for each group within a plot panel as needed for animation. If
#'   \code{FALSE}, the default, single layers are added per panel.
#' @param geom character The name of a ggplot geometry, currently only
#'   \code{"area"}, \code{"spct"} and \code{"line"}.
#' @param wb.trim logical. Passed to \code{\link[photobiology]{trim_wl}}.
#'   Relevant only when the \code{waveband} extends partly outside \code{range}.
#' @param norm numeric or character Normalization wavelength (nm) or character
#'   string \code{"max"} or other criterion for normalization.
#' @param text.size numeric size of text in the plot decorations.
#' @param ylim numeric y axis limits,
#' @param object.label character The name of the object being plotted.
#' @param na.rm logical.
#'
#' @return a \code{ggplot} object.
#'
#' @seealso \code{\link{autoplot.response_spct}},
#'   \code{\link[photobiology]{waveband}}.
#'
#' @export
#'
#' @family autoplot methods
#'
#' @examples
#'
#' autoplot(waveband(c(400, 500)))
#' autoplot(waveband(c(400, 500)), w.length = c(300,600))
#' autoplot(waveband(c(400, 500)), range = c(300,600))
#' autoplot(waveband(c(400, 500)), geom = "spct")
#'
autoplot.waveband <-
  function(object,
           ...,
           w.length = NULL,
           range = NULL,
           fill = 0,
           span = NULL,
           wls.target = "HM",
           unit.in = getOption("photobiology.radiation.unit",
                               default = "energy"),
           unit.out = unit.in,
           annotations = NULL,
           by.group = FALSE,
           geom = "line",
           wb.trim = TRUE,
           norm = NA,
           text.size = 2.5,
           ylim = c(NA, NA),
           object.label = deparse(substitute(object)),
           na.rm = TRUE) {

    if (is.null(norm) || is.na(norm)) {
      norm = "update"
    }
    force(object.label)
    w.band <- object

    annotations.default <-
      getOption("photobiology.plot.annotations",
                default = c("boxes", "labels", "colour.guide"))
    annotations <- c("=", decode_annotations(annotations,
                                             annotations.default))
    if (na.rm) {
      w.length <- stats::na.omit(w.length)
    }
    if (!is.null(w.length)) {
      if (length(w.length) < 200) {
        w.length <- seq(min(w.length), max(w.length), length.out = 200)
      } else {
        w.length <- unique(sort(w.length, na.last = NA))
      }
    } else {
      w.length <- seq(wl_min(w.band) - 1, wl_max(w.band) + 1, length.out = 200)
    }

    if (is.null(range)) {
      range <- range(w.length)
    } else if (photobiology::is.waveband(range)) {
      range <- c(photobiology::wl_min(range) - 1,
                 photobiology::wl_max(range) + 1)
    } else if (photobiology::is.any_spct(range)) {
      range <- wl_range(range)
    } else if (is.numeric(range) &&
               (length(range) > 2L || !anyNA(range))) {
      range <- range(range, na.rm = TRUE)
    }
    if (!length(range) == 2L || !is.numeric(range)) {
      warning("Ignoring bad 'range' argument")
      range <- rep(NA_real_, 2)
    }

    # add "hinges" to wavelengths vector if within its range
    if (!is.null(w.band$hinges) & length(w.band$hinges) > 0) {
      hinges <- with(w.band, hinges[hinges > min(w.length) & hinges < max(w.length)])
      w.length <- c(w.length, hinges)
      w.length <- unique(sort(w.length))
    }
    s.response <-
      photobiology::calc_multipliers(w.length, w.band,
                                     unit.out = unit.in, unit.in = unit.in,
                                     use.cached.mult = getOption("photobiology.use.cached.mult",
                                                                 default = FALSE), fill = fill)
    if (unit.in == "energy") {
      spct <- photobiology::response_spct(w.length = w.length, s.e.response = s.response)
    } else if (unit.in %in% c("photon", "quantum")) {
      spct <- photobiology::response_spct(w.length = w.length, s.q.response = s.response)
    }
    autoplot(spct,
             w.band = w.band,
             range = range,
             annotations = annotations,
             by.group = by.group,
             norm = norm,
             geom = geom,
             wb.trim = wb.trim,
             span = span,
             wls.target = wls.target,
             text.size = text.size,
             ylim = ylim,
             na.rm = na.rm,
             unit.out = unit.out,
             ...) +
      autotitle(object = object,
                object.label = object.label,
                annotations = annotations)
  }
