#' Add title to a spectral plot
#'
#' Add a title to a spectral plot based on metadata
#'
#' @param x generic_spct The spectral object plotted.
#' @param x.name character The name of the object being plotted.
#' @param annotations character vector Annotations as described for
#'   \code{plot()} methods, values unrelated to title are ignored.
#' @param default.title character vector The default used for \code{annotations
#'   = "title"}.
#'
#' @return The return value of \code{ggplot2::ggtitle()}.
#'
#' @export
#'
ggtitle_spct <- function(x,
                         x.name = NULL,
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

