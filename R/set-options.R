
# set and unset R options -------------------------------------------------

#' @title Set defaults for plotting
#'
#' @description Set R options controlling default arguments for some formal
#'   parameters in methods and functions from package 'ggspectra'.
#'
#' @inheritParams autoplot.source_spct
#' @inheritParams scale_y_s.e.irrad_continuous
#'
#' @details
#' The values accepted, syntax used and behaviour are the same as when passing
#' arguments to formal parameters in function and methods calls, except that
#' \code{NULL} as argument clears the R option. To restore the previous state
#' of an option, save the value returned and pass it as argument in a later
#' call to the same function.
#'
#' Changing the defaults with options, instead of affecting a single function
#' call (e.g., affecting a single plot or layer in a plot), changes the default
#' used for all subsequent function calls calls when when no argument is passed
#' explicitly. This makes it possible to easily change in one place in a script
#' the appearance of all/multiple plots. Using these functions functions instead
#' of \code{\link{options}} to set the defaults adds a validation step that
#' protects from errors triggered in subsequent function calls.
#'
#' R option \code{photobiology.plot.annotations} controls the default for formal
#' parameter \code{annotations} in \code{autoplot} methods and in
#' function \code{decoration}.
#'
#' @inheritSection decoration Plot Annotations
#' @inheritSection autotitle Title Annotations
#'
#' @return Previous value of the option, returned invisibly. This is a named
#'   list of length one as returned by \code{options}, that can be passed
#'   unchanged as argument to R function \code{options} or in a new call to the
#'   same function that returned it.
#'
#' @seealso Additional argument defaults are controlled by options also used in
#' package 'photobiology'. See \code{\link[photobiology]{energy_as_default}},
#'   \code{\link[photobiology]{using_Tfr}} and \code{\link{options}}.
#'
#' @family set and unset R options
#'
#' @export
#'
set_annotations_default <- function(annotations = NULL) {
  if (is.list(annotations) &&
      length(annotations) == 1L && names(annotations == "ggspectra.axis.symbols")) {
    axis.symbols <- axis.symbols[["ggspectra.axis.symbols"]]
  }
  if (!is.null(annotations)) {
    annotations <- stats::na.omit(annotations)
    annotations.default <-
      getOption("photobiology.plot.annotations",
                default =
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
    annotations <- decode_annotations(annotations = annotations,
                                      annotations.default = annotations.default)
  }
  options(photobiology.plot.annotations = annotations)
}

#' @rdname set_annotations_default
#'
#' @details
#' R option \code{photobiology.plot.bands} controls the default for formal
#' parameter \code{w.band} in \code{autoplot} methods and in
#' function \code{decoration}.
#'
#' @export
#'
set_w.band_default <- function(w.band = NULL) {
  if (!is.waveband(w.band) && is.list(w.band) &&
      length(w.band) == 1L && names(w.band == "photobiology.plot.bands")) {
    w.band <- w.band[["photobiology.plot.bands"]]
  }
  if (!is.null(w.band)) {
    # validation to avoid delayed errors
    if (photobiology::is.waveband(w.band)) {
      w.band <- list(w.band) # optimization: avoid repeating this step
    }
    if (!all(sapply(w.band, is.waveband))) {
      warning("'w.band' argument not NULL or waveband, default not changed.")
      return(getOption("photobiology.plot.bands"))
    }
  }
  options(photobiology.plot.bands = w.band)
}

#' @rdname set_annotations_default
#'
#' @details
#' R option \code{photobiology.math} controls the default for
#' formal parameter \code{markup.format} or \code{format} in
#' different \emph{axis label} and \emph{scale} functions.
#'
#' @export
#'
set_markup_format_default <- function(markup.format = NULL) {
  if (is.list(markup.format)) {
    markup.format <- markup.format[["photobiology.math"]]
  }
  if (!is.null(markup.format) && (length(markup.format) > 1L ||
               !markup.format %in%
               c("R", "R.expresion", "R.character", "LaTeX"))) {
    warning("'markup.format' must be NULL, \"R\", \"R.expresion\", ",
            "\"R.character\", or \"LaTeX\" but was: '",
            markup.format, "'. Default not modified!")
    return(getOption("photobiology.math"))
  }
  invisible(options(photobiology.math = markup.format))
}

#' @rdname set_annotations_default
#'
#' @details
#' R option \code{ggspectra.wlrange} controls the default for
#' formal parameter \code{range} in \code{autoplot} methods.
#'
#' @export
#'
set_plot_range_default <- function(range = NULL) {
  if (is.list(range) && length(names(range))) {
    range <- range[["ggspectra.wlrange"]]
  }
  if (!(is.null(range) || (length(range) == 2L && anyNA(range)))) {
    range <- range(range, na.rm = length(range > 2L))
  }
  invisible(options(ggspectra.wlrange = range))
}

#' @rdname set_annotations_default
#'
#' @details
#' R option \code{ggspectra.pc.out} controls the default for
#' formal parameter \code{pc.out} in \code{autoplot} methods that
#' have this formal parameter.
#'
#' @export
#'
set_pc_out_default <- function(pc.out = TRUE) {
  if (is.list(pc.out)) {
    pc.out <- pc.out[["ggspectra.pc.out"]]
  }
  stopifnot("'pc.out' must be NULL or a logical value" =
              is.null(pc.out) || is.logical(pc.out))
  invisible(options(ggspectra.pc.out = pc.out))
}

#' @rdname set_annotations_default
#'
#' @details
#' R option \code{ggspectra.axis.symbols} controls the default for
#' formal parameter \code{axis.symbols} in
#' different \emph{axis label} and \emph{scale} functions.
#'
#' @export
#'
set_axis_symbols_default <- function(axis.symbols = TRUE) {
  if (is.list(axis.symbols)) {
    axis.symbols <- axis.symbols[["ggspectra.axis.symbols"]]
  }
  stopifnot("'axis.symbols' must be NULL or a logical value" =
              is.null(axis.symbols) || is.logical(axis.symbols))
  invisible(options(ggspectra.axis.symbols = axis.symbols))
}

