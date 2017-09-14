#' Layer manipulation
#'
#' Remove one or more layers from a ggplot object
#'
#' @param x an object of class \code{gg} to be operated upon.
#' @param geom_type the name of the ggproto object for the geom(s) to be removed.
#'
#' @return An edited copy of x.
#'
#' @references
#' \url{https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart}
#'
#' @note
#' Not exported as code assumes all layers are geoms
#'
#' @examples
#'
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30)
#' )
#' p <- ggplot(df, aes(gp, y)) +
#'      geom_point() +
#'      stat_summary(fun.data = "mean_se", colour = "red")
#' p
#' remove_geoms(p, "GeomPoint")
#' move_geoms(p, "GeomPoint", "top")
#' move_geoms(p, "GeomPointrange", "bottom")
#'
remove_geoms <- function(x, geom_type) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$geom)[1] == geom_type
                     })
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}

#' @rdname remove_geoms
#'
remove_stats <- function(x, stat_type) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$stat)[1] == stat_type
                     })
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}


#' @rdname remove_geoms
#'
move_geoms <- function(x, geom_type, position = "top") {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$geom)[1] == geom_type
                     })
  # Move the layers.
  if (position == "top") {
    x$layers <- c(x$layers[!selector], x$layers[selector])
  } else if (position == "bottom") {
    x$layers <- c(x$layers[selector], x$layers[!selector])
  } else {
    stop("Position must be one of 'top' or 'bottom'")
  }
  x
}

#' @rdname remove_geoms
#'
move_stats <- function(x, stat_type, position = "top") {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$stat)[1] == stat_type
                     })
  # Move the layers.
  if (position == "top") {
    x$layers <- c(x$layers[!selector], x$layers[selector])
  } else if (position == "bottom") {
    x$layers <- c(x$layers[selector], x$layers[!selector])
  } else {
    stop("Position must be one of 'top' or 'bottom'")
  }
  x
}
