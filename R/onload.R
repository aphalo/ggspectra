utils::globalVariables(c(
  "from_theme",
  "colour",
  "ink",
  "fill",
  "paper",
  "fontsize",
  "linewidth",
  "linetype"
))

.onLoad <- function(libname, pkgname) {

  # if 'ggplot2' >= 4.0.0 we update the Geom "definition" to retrieve defaults
  # from the theme, as the geom element is available
  if ("element_geom" %in% getNamespaceExports("ggplot2")) {
    ggplot2::update_geom_defaults(
        GeomSpct,
        ggplot2::aes(colour = ggplot2::from_theme(colour %||% NA),
                     fill = ggplot2::from_theme(fill %||% scales::col_mix(ink, paper, 0.6)),
                     linewidth = ggplot2::from_theme(borderwidth),
                     linetype = ggplot2::from_theme(bordertype),
                     alpha = NA)
        )
    }

}
