#' Find 'idfactor'
#'
#' Find if an 'idfactor' is present and needed in the plot and assemble a
#' suitable plot layer to handle it.
#'
#' @param spct generic_spct or derived object
#' @param idfactor character Name of an index column in data holding a
#'   \code{factor} with each spectrum in a long-form multispectrum object
#'   corresponding to a distinct spectrum. If \code{idfactor=NULL} the name of
#'   the factor is retrieved from metadata or if no metadata found, the default
#'   "spct.idx" is tried. If \code{idfactor=NA} no aesthetic is mapped to the
#'   spectra and the user needs to use 'ggplot2' functions to manually map an
#'   aesthetic or use facets for the spectra.
#' @param annotations a character vector.
#' @param ... currently not used.
#'
#' @return A list object containing a list of ggplot components and a vector of
#'   annotations names as members.
#'
#' @keywords internal
#'
find_idfactor <- function(spct,
                         idfactor,
                         annotations,
                         ...) {
  if ((is.null(idfactor) || !is.na(idfactor)) && getMultipleWl(spct) > 1L) {
    if (is.null(idfactor)) {
      idfactor <- getIdFactor(spct)
    }
    if (is.na(idfactor)) {
      # handle objects created with 'photobiology' <= 9.20
      idfactor <- "spct.idx"
    }
    if (!exists(idfactor, spct, inherits = FALSE)) {
      message("'multiple.wl > 1' but no idexing factor found.")
      ggplot_comp <- list()
    } else {
      ggplot_comp <- list(aes_string(linetype = idfactor))
      annotations <- setdiff(annotations, "summaries")
    }
  } else { # only one spectrum
    ggplot_comp <- list()
  }
  list(annotations = annotations,
       ggplot_comp = ggplot_comp)
}
