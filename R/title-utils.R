#' Add title to a spectral plot
#'
#' Add a title to a spectral plot based on metadata stored in an spectral
#' object.
#'
#' @param x generic_spct The spectral object plotted.
#' @param x.name character The name of the object being plotted.
#' @param annotations character vector Annotations as described for
#'   \code{plot()} methods, values unrelated to title are ignored.
#' @param default.title character vector The default used for \code{annotations
#'   = "title"}.
#'
#' @details \code{ggtitle_spct()} retrieves from object \code{x} metadata and
#' passes it to \code{ggplot2::ggtitle()} as arguments for \code{title} and
#' \code{subtitle}. The specification for the tittle is passed as argument
#' to \code{annotations}, and consists in the keyword \code{title} with optional
#' modifiers selecting the kind of metatdata to use, separated by colons.
#' "objt", "class", "what", "when", "where", "inst.name", "inst.sn" and "none"
#' are recognized as modifiers to "title".
#'
#' @return The return value of \code{ggplot2::ggtitle()}.
#'
#' @examples
#' library(ggplot2)
#' library(photobiology)
#'
#' p <- ggplot(sun.spct) +
#'   geom_line()
#'
#' p + ggtitle_spct(sun.spct)
#' p + ggtitle_spct(sun.spct, annotations = "title:where:when")
#'
#' @export
#'
ggtitle_spct <- function(x,
                         x.name = deparse(substitute(x)),
                         annotations = "title",
                         default.title  = "title:objt") {

  get_title_text <- function(key) {
    switch(key,
           objt = x.name,
           class = class(x)[1],
           what = getWhatMeasured(x)[[1]],
           when = getWhenMeasured(x),
           where = paste(getWhereMeasured(x), collapse = "; "),
           inst.name = getInstrDesc(x)[["spectrometer.name"]],
           inst.sn = getInstrDesc(x)[["spectrometer.sn"]],
           none = NULL,
           {warning("Title key '", key, "' not recognized"); NULL}
    )
  }

  title.ann <- grep("^title", annotations, value = TRUE)
  if (length(title.ann) == 0) {
    return(NULL)
  } else if(title.ann[1] == "title") {
    # default title
    title.ann <- default.title
  }
  title.ann <- c(strsplit(title.ann[1], ":")[[1]][-1], "none", "none")
  title <- get_title_text(title.ann[1])
  subtitle <- get_title_text(title.ann[2])
  ggtitle(title, subtitle)
}

