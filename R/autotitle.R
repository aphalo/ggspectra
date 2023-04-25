#' Add title, subtitle and caption to a spectral plot
#'
#' Add a title, subtitle and caption to a spectral plot based on automatically
#' extracted metadata from an spectral object.
#'
#' @param object generic_spct The spectral object plotted.
#' @param object.label character The name of the object being plotted.
#' @param annotations character vector Annotations as described for
#'   \code{plot()} methods, values unrelated to title are ignored.
#' @param time.format character Format as accepted by
#'   \code{\link[base]{strptime}}.
#' @param tz character time zone used in labels.
#' @param default.title character vector The default used for \code{annotations
#'   = "title"}.
#'
#' @section Title Annotations: metadata retrieved from object \code{object} is
#'   paased to \code{ggplot2::ggtitle()} as arguments for \code{title},
#'   \code{subtitle} and \code{caption}. The specification for the title is
#'   passed as argument to \code{annotations}, and consists in the keyword
#'   \code{title} with optional modifiers selecting the kind of metatdata to
#'   use, separated by colons. Up to three keywords separated by colons are
#'   accepted, and correspond to title, subtitle and caption. The recognized
#'   keywords are: \code{"objt"}, \code{"class"}, \code{"what"}, \code{"when"},
#'   \code{"where"}, \code{"how"}, \code{"inst.name"}, \code{"inst.sn"},
#'   \code{"comment"} and \code{"none"} are recognized as modifiers to
#'   \code{"title"}; \code{"none"} is a placeholder. Default is
#'   \code{"title:objt"} or no title depending on the context.
#'
#' @return The return value of \code{ggplot2::labs()}.
#'
#' @examples
#'
#' p <- ggplot(sun.spct) +
#'   geom_line()
#'
#' p + autotitle(sun.spct)
#' p + autotitle(sun.spct, annotations = "title:what")
#' p + autotitle(sun.spct, annotations = "title:where:when")
#' p + autotitle(sun.spct, annotations = "title:none:none:comment")
#'
#' @export
#'
autotitle <- function(object,
                      object.label = deparse(substitute(object)),
                      annotations = "title",
                      time.format = "",
                      tz = NULL,
                      default.title  = "title:objt") {

  if (!is.character(object.label)) {
    if (is.name(object.label)) {
      object.label <- as.character(object.label)
    } else {
      object.label <- paste("anonymous '", class(object)[1], "'", sep = "")
    }
  }

  class.object <- class(object)[1]

  # this makes it possible to support collections without additional code
  # but it is a kludge, tolerable as it is rarely used
  if (is.generic_mspct(object)) {
    object <- rbindspct(object)
  }

  get_title_text <- function(key) {
    switch(key,
           objt = object.label,
           class = class.object,
           what = getWhatMeasured(object)[[1]],
           how = getHowMeasured(object)[[1]],
           when = strftime(x = getWhenMeasured(object),
                           format = time.format,
                           tz = tz,
                           usetz = TRUE),
           where =
             {
               where <- getWhereMeasured(object)
               if (!(anyNA(where[["lon"]]) | anyNA(where[["lat"]]))) {
                 where[["lon"]] <- ifelse(where[["lon"]] < 0,
                                          paste(abs(where[["lon"]]), " W"),
                                          paste(where[["lon"]], " E"))
                 where[["lat"]] <- ifelse(where[["lat"]] < 0,
                                          paste(abs(where[["lat"]]), " S"),
                                          paste(where[["lat"]], " N"))
                 where[["address"]] <- ifelse(is.na(where[["address"]]),
                                              character(),
                                              as.character(where[["address"]]))
                 # the order of columns in the data frame can vary
                 paste(where[["lat"]], where[["lon"]], where[["address"]], sep = ", ")
               } else {
                 character()
               }
             },
             inst.name = getInstrDesc(object)[["spectrometer.name"]],
             inst.sn = getInstrDesc(object)[["spectrometer.sn"]],
             comment = comment(object),
             num.spct = paste("n=", getMultipleWl(object)),
             none = NULL,
             {warning("Title key '", key, "' not recognized"); NULL}
    )
  }

  title.ann <- grep("^title", annotations, value = TRUE)
  if (length(title.ann) == 0) {
    return(NULL)
  } else if (title.ann[1] == "title") {
    # default title
    title.ann <- default.title
  }

  # we avoid calling tz() on lists as returned for multiple spectra
  when.measured <- getWhenMeasured(object)
  if (is.data.frame(when.measured)) {
    when.measured <- unique(range(when.measured[["when.measured"]]))
  }
  if (is.null(tz)) {
    if (lubridate::is.instant(when.measured[1])) {
      tz <- lubridate::tz(when.measured[1])
      if (tz == "") {
        # [2021-09-30] this may happen if the user uses lubridate::now()
        tz <- Sys.timezone()
      }
    } else {
      tz <- NA_character_
    }
  }

  # length(title.ann) > 0 is guaranteed
  if (getMultipleWl(object) > 1 &&
      grepl(":where|:what|:when|:how|:inst.name|:inst.sn", title.ann)) {
    warning("Multiple spectra: ignoring requested attr(s) in title.")
    title.ann <- gsub(":where|:what|:when|:how|:inst.name|:inst.sn", ":none", title.ann)
  }

  title.ann <- c(strsplit(title.ann[1], ":")[[1]][-1], "none", "none", "none")

  title <- get_title_text(title.ann[1])
  subtitle <- get_title_text(title.ann[2])
  caption <- get_title_text(title.ann[3])
  # this is equivalent to ggtitle(tittle, subtitle)
  ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
}

#' @rdname autotitle
#'
#' @note Method renamed as \code{autotitle()} to better reflect its function;
#' \code{ggtitle_spct()} is deprecated but will remain available for backwards
#' compatibility.
#'
#' @export
#'
ggtitle_spct <- autotitle
