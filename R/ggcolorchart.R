#' Create a color checker chart
#'
#' Color-checker-chart ggplot labelled with color names or with indexes of the
#' colors in the vector passed as first argument.
#'
#' @param colors character A vector of color definitions.
#' @param ncol integer Number of column in the checker grid.
#' @param use.names logical Force use of names or indexes.
#' @param text.size numeric Size of the text labels drawn on each color tile.
#' @param text.color character Color definition, used for text on tiles.
#' @param grid.color character Color definition, used for grid lines between
#'   tiles.
#'
#' @note Default \code{text.color} uses \code{black_or_white()} to ensure enough
#'   contrast. Default for \code{use.names} depends on number of columns in the
#'   grid, indexes are used when columns are seven or more.
#'
#' @export
#'
#' @examples
#'
#' color_chart()
#' color_chart(grep("dark", colors(), value = TRUE))
#'
color_chart <- function(colors = grDevices::colors(),
                        ncol = NULL,
                        use.names = NULL,
                        text.size = 2,
                        text.color = NULL,
                        grid.color = "white") {
  # needed if the argument passed is subset with [ ]!
  force(colors)

  len.colors <- length(colors)
  # use squarish tiles by default
  if (is.null(ncol)) {
    ncol <- max(trunc(sqrt(len.colors)), 1L)
  }
  # use color names for seven or fewer columns by default
  if (is.null(use.names)) {
    use.names <- ncol < 8
  }
  # number of rows needed to fit all colors
  nrow <- len.colors %/% ncol
  if (len.colors %% ncol != 0) {
    nrow <- nrow + 1
  }
  # we obtain names for the colors
  color.names <- names(colors)
  if (length(color.names) != length(colors)) {
    color.names <- colors
  }
  # we extend the vectors with NAs or "" to match number of tiles
  if (len.colors < ncol*nrow) {
    colors[(len.colors + 1):(ncol*nrow)] <- NA
    color.names[(len.colors + 1):(ncol*nrow)] <- ""
  }
  # we set default text color if needed
  if (is.null(text.color)) {
    text.color <- black_or_white(colors)
  }

  # we build a data frame
  colors.df <-
    data.frame(color = colors,
               color.names = color.names,
               text.color = text.color,
               x = rep(1:ncol, nrow),
               y = rep(nrow:1, rep(ncol, nrow)),
               idx = ifelse(is.na(colors),
                            "",
                            format(seq_along(colors),
                                   trim = TRUE,
                                   width = 3))
               )
  # we build the plot
  p <- ggplot(colors.df, aes_(~x, ~y, fill = ~color))
  if (use.names) {
    p <- p + aes_(label = ~color.names)
  } else {
    p <- p + aes_(label = ~idx)
  }
  p <- p +
    geom_tile(color = grid.color) +
    scale_fill_identity() +
    geom_text(size = text.size, aes_(color = ~text.color)) +
    scale_color_identity()
  p + theme_void()
}

#' Chose black vs. white color based on weighted mean of RGB channels
#'
#' Chose black or white color based on a color to be used as background.
#' Usefull when using \code{geom_text} on top of tiles or bars, or
#' \code{geom_label} with a variable fill.
#'
#' @param colors character A vector of color definitions.
#' @param threshold numeric in range 0 to 1.
#'
#' @export
#'
#' @examples
#'
#' black_or_white("red")
#' black_or_white(colors()[1:10])
#'
black_or_white <- function(colors, threshold = 0.45){

  threshold <- trunc(threshold * 255)

  lum <- function(colors) {
  sapply(colors,
         function(x) {
           y <- grDevices::col2rgb(x)
           sum(y * c(1.5, 2.5, 1)) / 5
           },
         USE.NAMES = FALSE)
  }

  ifelse(lum(colors) > threshold, "black", "white")
}
