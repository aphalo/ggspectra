#' Add title, subtitle and caption to a spectral plot
#'
#' Add a title, subtitle and caption to a spectral plot based on automatically
#' extracted metadata from an spectral object.
#'
#' @param object generic_spct or generic_mspct The spectral object plotted.
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
#' p + autotitle(sun.spct, object.label = "The terrestrial solar spectrum")
#' p + autotitle(sun.spct, annotations = "title:objt:class")
#' p + autotitle(sun.spct, annotations = "title:where:when:how")
#'
#' p <- ggplot(sun_evening.spct) +
#'   aes(linetype = spct.idx) +
#'   geom_line()
#'
#' p + autotitle(sun_evening.spct, annotations = "title:objt:class")
#' p + autotitle(sun_evening.spct, annotations = "title:where:when:how")
#' p + autotitle(sun_evening.spct, annotations = "title:none:none:how")
#'
#' p <- ggplot(sun_evening.mspct) +
#'   aes(linetype = spct.idx) +
#'   geom_line()
#'
#' p + autotitle(sun_evening.mspct, annotations = "title:objt:class")
#'
#' @export
#'
autotitle <- function(object,
                      object.label = deparse(substitute(object)),
                      annotations = "title",
                      time.format = NULL,
                      tz = "",
                      default.title  = "title:objt") {

  if (!is.character(object.label)) {
    if (is.name(object.label)) {
      object.label <- as.character(object.label)
    } else {
      object.label <- paste("anonymous '", class(object)[1], "'", sep = "")
    }
  }

  if (is.null(time.format) || all(time.format == "")) {
    time.format <- c("%Y-%m-%d %H:%M", "%H:%M")
  } else if (length(time.format) == 1) {
    time.format <- rep(time.format, 2)
  }

  class.object <- class(object)[1]

  # this makes it possible to support collections
  # maybe not needed with current versions 'photobiology' >= 0.11.1
  if (is.generic_mspct(object)) {
    object <- rbindspct(object)
  }

  get_title_text <- function(key) {
    switch(key,
           objt = object.label,
           class = class.object,
           what =
             {
               what <- getWhatMeasured(object)
               if (all(is.na(what))) {
                 what <- "what measured not known"
               }
               if (is.list(what)) {
                 what <- unique(what)
                 if (length(what) == 1) {
                   what
                 } else {
                   ""
                 }
               } else {
                 what
               }
             },
           how =
             {
               how <- getHowMeasured(object)
               if (all(is.na(how))) {
                 how <- "how measured not known"
               } else {
                 if (is.list(how)) {
                   how <- unique(how)
                   if (length(how) == 1) {
                     how
                   } else {
                     "how measured varies"
                   }
                 } else {
                   how
                 }
               }
             },
           when =
             {
               when <- getWhenMeasured(object)
               if (all(is.na(when))) {
                 when <- "when measured not known"
               } else {
                 if (is.list(when)) {
                   when <- cbind(unname(when))
                   when <- as.POSIXct(unique(range(when)))
                   if ((when[2] - when[1]) >= lubridate::ddays(1)) {
                     time.format[2] <- time.format[1]
                   }
                 }
                 if(length(when) == 1) {
                   strftime(x = when[1],
                            format = time.format[1],
                            tz = tz,
                            usetz = TRUE)
                 } else if(length(when) == 2) {
                   paste(
                     strftime(x = when[1],
                              format = time.format[1],
                              tz = tz,
                              usetz = FALSE),
                     strftime(x = when[2],
                              format = time.format[2],
                              tz = tz,
                              usetz = TRUE),
                     sep = " to ")
                 }
               }
             },
           where =
             {
               where <- getWhereMeasured(object)
               if (all(is.na(where))) {
                 "where measured not know"
               } else {
                 if (!is.data.frame(where) && is.list(where)) {
                   where <- unique(where)
                   if (length(where) == 1) {
                     where <- where[[1]] # extract list member
                   } else {
                     where <- "where measured varies"
                   }
                 }
                 if (is.data.frame(where) &&
                     (!(anyNA(where[["lon"]]) | anyNA(where[["lat"]])))) {
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
                   where
                 }
               }
             },
           inst.name =
             {
               descriptor <- getInstrDesc(object)
               if (all(is.na(descriptor))) {
                 descriptor <- "instrument name not known"
               } else {
                 if (inherits(descriptor, "instr_desc")) {
                   inst.names <- descriptor[["spectrometer.name"]]
                 } else {
                   inst.names <- sapply(descriptor, `[[`, i = "spectrometer.name")
                   inst.names <- unique(inst.names)
                 }
                 if (length(inst.names) == 1) {
                   inst.names
                 } else {
                   "multiple instrument names"
                 }
               }
             },
           inst.sn =
             {
               descriptor <- getInstrDesc(object)
               if (all(is.na(descriptor))) {
                 descriptor <- "instrument serial number not known"
               } else {
                 if (inherits(descriptor, "instr_desc")) {
                   inst.sns <- descriptor[["spectrometer.sn"]]
                 } else {
                   inst.sns <- sapply(descriptor, `[[`, i = "spectrometer.sn")
                   inst.sns <- unique(inst.sns)
                 }
                 if (length(inst.sns) == 1) {
                   inst.sns
                 } else {
                   "multiple instrument units"
                 }
               }
             },
           comment = comment(object),
           num.spct = paste("n =", getMultipleWl(object)),
           none = NULL,
           {
             warning("Title key '", key, "' not recognized"); NULL
           }
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
