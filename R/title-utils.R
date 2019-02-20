#' Add title to a spectral plot
#'
#' Add a title to a spectral plot based on metadata stored in an spectral
#' object.
#'
#' @param object generic_spct The spectral object plotted.
#' @param object.label character The name of the object being plotted.
#' @param annotations character vector Annotations as described for
#'   \code{plot()} methods, values unrelated to title are ignored.
#' @param time.format character Format as accepted by \code{\link[base]{strptime}}.
#' @param tz character time zone used in labels.
#' @param default.title character vector The default used for \code{annotations
#'   = "title"}.
#'
#' @details \code{ggtitle_spct()} retrieves from object \code{object} metadata and
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
#'
#' p <- ggplot(sun.spct) +
#'   geom_line()
#'
#' p + ggtitle_spct(sun.spct)
#' p + ggtitle_spct(sun.spct, annotations = "title:where:when")
#'
#' @export
#'
ggtitle_spct <- function(object,
                         object.label = deparse(substitute(object)),
                         annotations = "title",
                         time.format = "",
                         tz = lubridate::tz(getWhenMeasured(object)),
                         default.title  = "title:objt") {

  get_title_text <- function(key) {
    switch(key,
           objt = object.label,
           class = class(object)[1],
           what = getWhatMeasured(object)[[1]],
           when = strftime(x = getWhenMeasured(object),
                           format = time.format,
                           tz = tz,
                           usetz = TRUE),
           where =
           {where <- getWhereMeasured(object)
           if (!is.na(where[[1]])) {
             where[["lon"]] <- ifelse(where[["lon"]] < 0,
                                      paste(abs(where[["lon"]]), " W"),
                                      paste(where[["lon"]], " E"))
             where[["lat"]] <- ifelse(where[["lat"]] < 0,
                                      paste(abs(where[["lat"]]), " S"),
                                      paste(where[["lat"]], " N"))
             if (exists("address", where = where)) {
               where[["address"]] <- as.character(where[["address"]])
             } else {
               where[["address"]] <- character()
             }
           }
           # the order of columns in the data frame can vary
           paste(where[["lat"]], where[["lon"]], where[["address"]], sep = ", ")
           },
           inst.name = getInstrDesc(object)[["spectrometer.name"]],
           inst.sn = getInstrDesc(object)[["spectrometer.sn"]],
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
  # this is equivalent to ggtitle(tittle, subtitle)
  ggplot2::labs(title = title, subtitle = subtitle)
}

