# Set options -----------------------------------------------------------

#' Set default plot annotations
#'
#' Edit option "photobiology.plot.annotations" easily. These convenience
#' functions make it easier to edit this option defined by a vector
#' of characters strings.
#'
#' @param annotations character vector Annotations to add or remove from
#'   defaults used by \code{autoplot()} methods.
#'
#' @note When removing options "title", "peaks" and "valleys" will remove all
#'   variations of these annotations.
#'
#' @return Previous value of option "photobiology.plot.annotations".
#'
#' @export
#'
add_to_annotations_default <- function(annotations = NULL) {
  default.annotations <-
    getOption("photobiology.plot.annotations",
              default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  # we need to remove options which are variations of the annotation being added
  if (any(grepl("title.*", annotations))) {
    default.annotations <- default.annotations[!grepl("title.*", default.annotations)]
  }
  if (any(grepl("peak.*", annotations))) {
    default.annotations <- default.annotations[!grepl("peak.*", default.annotations)]
  }
  if (any(grepl("valley.*", annotations))) {
    default.annotations <- default.annotations[!grepl("valley.*", default.annotations)]
  }
  options(photobiology.plot.annotations = union(default.annotations, annotations))
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
remove_from_annotations_default <- function(annotations = NULL) {
  default.annotations <-
    getOption("photobiology.plot.annotations",
              default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  if (any(grepl("title$", annotations))) {
    default.annotations <- default.annotations[!grepl("title.*", default.annotations)]
  }
  if (any(grepl("peaks$", annotations))) {
    default.annotations <- default.annotations[!grepl("peak.*", default.annotations)]
  }
  if (any(grepl("valleys$", annotations))) {
    default.annotations <- default.annotations[!grepl("valley.*", default.annotations)]
  }
  options(photobiology.plot.annotations = setdiff(default.annotations, annotations))
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
unset_annotations_default <- function() {
  options(photobiology.plot.annotations = NULL)
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
title_as_default <- function() {
  add_to_annotations_default("title")
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
no_title_as_default <- function() {
  remove_from_annotations_default("title")
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
repel_labels_as_default <- function() {
  default.annotations <-
    getOption("photobiology.plot.annotations",
              default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  default.annotations <- gsub("^peaks$", "peak.labels", default.annotations)
  default.annotations <- gsub("^valleys$", "valley.labels", default.annotations)
  options(photobiology.plot.annotations = default.annotations)
}

#' @rdname add_to_annotations_default
#'
#' @export
#'
overlap_text_as_default <- function() {
  default.annotations <-
    getOption("photobiology.plot.annotations",
              default = c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  default.annotations <- gsub("^peak.labels$", "peaks", default.annotations)
  default.annotations <- gsub("^valley.labels$", "valleys", default.annotations)
  options(photobiology.plot.annotations = default.annotations)
}

