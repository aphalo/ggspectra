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
#' @param facets logical or numeric Flag indicating if facets are to be created
#'   when the data contains multiple spectra, if numeric the number of columns.
#' @param num.columns numeric Number of data columns from multiple scans. Relevant
#'   only to \code{raw_spct} and \code{cps_spct} objects.
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
                          facets = FALSE,
                          num.columns = 1L,
                          ...) {
  if ((is.null(idfactor) || !is.na(idfactor)) && getMultipleWl(spct) > num.columns) {
    if (is.null(idfactor) || (is.logical(idfactor) && idfactor)) {
      idfactor <- getIdFactor(spct)
      if (is.na(idfactor)) {
        # handle objects created with 'photobiology' <= 9.20
        idfactor <- "spct.idx"
      }
    }
    if (!exists(idfactor, spct, inherits = FALSE)) {
      message("'multiple.wl > 1' but no idexing factor found.")
      ggplot_comp <- list()
    } else if(!facets) {
      ggplot_comp <- list(aes_string(linetype = idfactor))
      annotations <- setdiff(annotations, "summaries")
    } else {
      if (is.numeric(facets)) {
        ggplot_comp <- list(facet_wrap(facets = idfactor,
                                       ncol = as.integer(facets)))
      } else {
        ggplot_comp <- list(facet_wrap(facets = idfactor))
      }
    }
  } else { # only one spectrum or idfactor is NA
    ggplot_comp <- list()
  }
  list(annotations = annotations,
       ggplot_comp = ggplot_comp)
}
