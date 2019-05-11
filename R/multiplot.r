#' Multiple plot function
#'
#' Grid based; allows multiple plots arraged in a matrix and \code{print}ed to
#' any R device. ggplot objects can be passed in ..., or to plotlist (as a list
#' of ggplot objects)
#'
#' @param ... one or more ggplot objects.
#' @param plotlist list of ggplot objects.
#' @param ncol,cols numerical Number of columns in layout.
#' @param layout  A numeric matrix specifying the layout. If present, 'cols' is
#'   ignored.
#' @param title character vector Title of the composite plot.
#' @param title.position numeric or character, the horizontal position of the
#'   title.
#' @param title.fontsize numeric
#' @param title.fontfamily character e.g. "sans", "serif", "mono".
#' @param title.fontface character e.g. "plain", "bold", "italic", "bold.italic".
#' @param title.colour character e.g. "black", "red".
#'
#' @details ggplot objects can be passed in ..., or to plotlist (as a list of
#'   ggplot objects) If the layout is something like matrix(c(1,2,3,3), nrow=2,
#'   byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper
#'   right, and 3 will go all the way across the bottom.
#'
#' @references \url{http://www.cookbook-r.com/}
#'   \url{http://www.guru-gis.net/multiplot-function-for-ggplot/}
#'
#' @note Modified from example by Winston Chang found in the Cookbook for R
#' Licenced under CC BY-SA
#'
#' @examples
#'
#' multiplot(plot(sun.spct), plot(yellow_gel.spct), ncol = 1)
#' multiplot(plot(sun.spct), plot(yellow_gel.spct), ncol = 1,
#'           title = "The sun and a yellow filter")
#'
#' @export
#'
multiplot <- function(..., plotlist = NULL,
                      ncol = 1, cols = ncol, layout = NULL,
                      title = "",
                      title.position = "left",
                      title.fontsize = 12,
                      title.fontfamily = "sans",
                      title.fontface = "bold",
                      title.colour = "black") {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (length(title) > 0 && nchar(title[1]) > 0 ) {
    title <- title[1]
    layout <- rbind(rep(0, ncol(layout)), layout)
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout),
                                                                 heights = if (nchar(title) > 0) {
                                                                   unit(c(1, rep(0.8, nrow(layout) - 1)), "null")}
                                                                 else {
                                                                   unit(c(rep(5, nrow(layout))), "null")})))

    # Add title if supplied
    if (nchar(title) > 0) {
      if (is.character(title.position)) {
        title.position <- switch(title.position,
                        left = 0.05,
                        centre = 0.5,
                        center = 0.5,
                        right = 0.95)
      }
      if (title.position < 0.5) {
        hjust = 0
      } else if (title.position > 0.5) {
        hjust = 1
      } else {
        hjust = 0.5
      }
      grid::grid.text(label = title,
                      x = title.position,
                      hjust = hjust,
                      vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                      gp = grid::gpar(fontsize = title.fontsize,
                                      fontfamily = title.fontfamily,
                                      fontface = title.fontface,
                                      col = title.colour))
    }

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
