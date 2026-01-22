#' Validate argument passed to geom
#'
#' \code{autoplot()} methods have a geom parameter that supports a subset of
#' existing geoms. This function factors this check out of the methods' code.
#'
#' @param geoms,supported.geoms,default.geoms character A vector of geom names.
#'
#' @keywords internal
#'
#' @examples
#' validate_geom_arg()
#' validate_geom_arg("line")
#' validate_geom_arg(c("line", "point"))
#' validate_geom_arg(c("bad", "point"))
#' validate_geom_arg(c("bad", "bad2"))
#'
validate_geom_arg <-
  function(geoms = character(),
           supported.geoms = c("area", "line", "spct", "col", "point"),
           default.geoms = "line") {

    stopifnot("Empty 'default.geoms' argument!" = length(default.geoms) >= 1L)

    if (length(geoms) && !all(geoms %in% supported.geoms)) {
      validated.geoms <- intersect(geoms, supported.geoms)
      warning("Discarding unsupported geoms: \"",
              paste(setdiff(geoms, supported.geoms), collapse = "\", \""),
              ifelse(length(validated.geoms),
                     "\".",
                     "\", using default instead."),
              call. = FALSE)
    } else {
      validated.geoms <- geoms
    }
    if (!length(validated.geoms)) {
      default.geoms
    } else {
      validated.geoms
    }
  }
