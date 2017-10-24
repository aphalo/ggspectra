#' Add title to a spectral plot
#'
#' Add a title to a spectral plot based on metadata
#'
#' @param x generic_spct The spectral object plotted.
#' @param x.name character The name of the object being plotted.
#' @param annotations character vector The annotations "sdirectives".
#' @param ... named arguments pased to \code{ggtitle()}
#'
#' @return The return value of \code{ggplot2::ggtitle()}.
#'
#' @export
#'
ggtitle_spct <- function(x,
                         x.name = NULL,
                         annotations = "title:objt") {
  title.ann <- grep("^title", annotations, value = TRUE)
  if (length(title.ann) == 0) {
    return(NULL)
  } else if(title.ann[1] == "title") {
    # default title
    title.ann <- "title:objt"
  }
  title.ann <- c(strsplit(title.ann[1], ":")[[1]][-1], "none", "none")
    title <-
      switch(title.ann[1],
             objt = x.name,
             what = getWhatMeasured(x)[[1]],
             when = getWhenMeasured(x),
             where = paste(getWhereMeasured(x), collapse = "; "),
             none = NULL,
             NULL)
    subtitle <-
      switch(title.ann[2],
             objt = x.name,
             what = getWhatMeasured(x)[[1]],
             when = getWhenMeasured(x),
             where = paste(getWhereMeasured(x), collapse = "; "),
             none = NULL,
             NULL)
  ggtitle(title, subtitle)
}

