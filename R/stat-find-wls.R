#' Find wavelength for target quantity value.
#'
#' \code{stat_find_wls} finds at which x positions values equal to a target are
#' located.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings "half.maximum" and "half.range" are also accepted
#'   as arguments. A list with \code{numeric} and/or \code{character} values is
#'   also accepted.
#' @param interpolate logical Indicating whether the nearest wavelength value in
#'   \code{x} should be returned or a value calculated by linear interpolation
#'   between wavelength values stradling the target.
#' @param label.fmt character  string giving a format definition for converting
#'   values into character strings by means of function \code{\link{sprintf}}.
#' @param x.label.fmt character  string giving a format definition for
#'   converting $x$-values into character strings by means of function
#'   \code{\link{sprintf}}.
#' @param y.label.fmt character  string giving a format definition for
#'   converting $y$-values into character strings by means of function
#'   \code{\link{sprintf}}.
#'
#' @return A data frame with one row for each match to \code{target} found in
#'   the data.
#'
#' @section Computed variables: \describe{ \item{x}{x-value at or nearest to the
#'   match to the target as numeric} \item{y}{target value or y-value nearest to
#'   the target as numeric} \item{x.label}{x-value at or nearest to the match
#'   formatted as character} \item{y.label}{target value or y-value nearest to
#'   the target formatted as character} \item{color}{color definition calculated
#'   by assuming that x-values are wavelengths expressed in nanometres.} }
#'
#' @section Default aesthetics: Set by the statistic and available to geoms.
#'   \describe{ \item{label}{..x.label..} \item{xintercept}{..x..}
#'   \item{yintercept}{..y..} \item{fill}{..color..} }
#'
#' @section Required aesthetics: Required by the statistic and need to be set
#'   with \code{aes()}. \describe{ \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity} }
#'
#' @seealso \code{\link[photobiology]{find_peaks}}.
#'
#' @details These stats use \code{geom_point} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. The
#'   default aesthetics set by these stats allow their direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#' @note These stats work nicely together with geoms \code{geom_text_repel} and
#'   \code{geom_label_repel} from package \code{\link[ggrepel]{ggrepel}} to
#'   solve the problem of overlapping labels by displacing them. To discard
#'   overlapping labels use \code{check_overlap = TRUE} as argument to
#'   \code{geom_text}. By default the labels are character values suitable to be
#'   plotted as is, but with a suitable \code{label.fmt} labels suitable for
#'   parsing by the geoms (e.g. into expressions containing greek letters or
#'   super or subscripts) can be also easily obtained.
#'
#' @examples
#' library(photobiology)
#' library(ggplot2)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(yellow_gel.spct) + geom_line() +
#'   stat_find_wls(target = c(0.25, 0.5, 0.75))
#' ggplot(yellow_gel.spct) + geom_line() +
#'   stat_find_wls(target = "half.maximum", geom = "point", colour = "red") +
#'   stat_find_wls(target = "half.maximum", geom = "text", colour = "red",
#'              hjust = 1.1, label.fmt = "%3.0f nm")
#' @export
#' @family stats functions
#'
stat_find_wls <- function(mapping = NULL, data = NULL, geom = "point",
                       target = "half.maximum", interpolate = TRUE,
                       label.fmt = "%.3g",
                       x.label.fmt = label.fmt, y.label.fmt = label.fmt,
                       position = "identity", na.rm = FALSE, show.legend = FALSE,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFindWls, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(target = target,
                  interpolate = interpolate,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @keywords internal
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatFindWls <-
  ggplot2::ggproto("StatFindWls", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            target,
                                            interpolate,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     hits.ls <- list()
                     for (t in target) {
                       hits.ls <-
                         c(hits.ls,
                           list(photobiology::find_wls(data,
                                                       .fun = `<=`,
                                                       target = t,
                                                       interpolate = interpolate,
                                                       col.name.x = "x",
                                                       col.name = "y")))
                     }
                     wls.df <- dplyr::bind_rows(hits.ls)
                     dplyr::mutate(wls.df,
                                   x.label = sprintf(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y),
                                   color = photobiology::color_of(x, type = "CMF"),
                                   BW.color = black_or_white(photobiology::color_of(x, type = "CMF")))
                   },
                   default_aes = ggplot2::aes(label = ..x.label..,
                                              fill = ..color..,
#                                              color = ..BW.color..,
                                              xintercept = ..x..,
                                              yintercept = ..y..),
                   required_aes = c("x", "y")
  )

#' Find quantity value for target wavelength value.
#'
#' \code{stat_find_qtys} finds at which y positions values equal to an x target
#' are located.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param target numeric vector indicating the spectral quantity values for
#'   which wavelengths are to be searched and interpolated if need. The
#'   \code{character} strings "half.maximum" and "half.range" are also accepted
#'   as arguments. A list with \code{numeric} and/or \code{character} values is
#'   also accepted.
#' @param interpolate logical Indicating whether the nearest wavelength value in
#'   \code{x} should be returned or a value calculated by linear interpolation
#'   between wavelength values stradling the target.
#' @param label.fmt character  string giving a format definition for converting
#'   values into character strings by means of function \code{\link{sprintf}}.
#' @param x.label.fmt character  string giving a format definition for
#'   converting $x$-values into character strings by means of function
#'   \code{\link{sprintf}}.
#' @param y.label.fmt character  string giving a format definition for
#'   converting $y$-values into character strings by means of function
#'   \code{\link{sprintf}}.
#'
#' @return A data frame with one row for each match to the target subset from
#'   the data or interpolated. As spectra are monotonic in wavelength, this
#'   statistic will never return more than one row when used with spectra.
#'
#' @section Computed variables: \describe{ \item{x}{x-value at or nearest to the
#'   match to the target as numeric} \item{y}{target value or y-value nearest to
#'   the target as numeric} \item{x.label}{x-value at or nearest to the match
#'   formatted as character} \item{y.label}{target value or y-value nearest to
#'   the target formatted as character} \item{color}{color definition calculated
#'   by assuming that x-values are wavelengths expressed in nanometres.} }
#'
#' @section Default aesthetics: Set by the statistic and available to geoms.
#'   \describe{ \item{label}{..x.label..} \item{xintercept}{..x..}
#'   \item{yintercept}{..y..} \item{fill}{..color..} }
#'
#' @section Required aesthetics: Required by the statistic and need to be set
#'   with \code{aes()}. \describe{ \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity} }
#'
#' @seealso \code{\link[photobiology]{find_peaks}}.
#'
#' @details These stats use \code{geom_point} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. The
#'   default aesthetics set by these stats allow their direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#' @note These stats work nicely together with geoms \code{geom_text_repel} and
#'   \code{geom_label_repel} from package \code{\link[ggrepel]{ggrepel}} to
#'   solve the problem of overlapping labels by displacing them. To discard
#'   overlapping labels use \code{check_overlap = TRUE} as argument to
#'   \code{geom_text}. By default the labels are character values suitable to be
#'   plotted as is, but with a suitable \code{label.fmt} labels suitable for
#'   parsing by the geoms (e.g. into expressions containing greek letters or
#'   super or subscripts) can be also easily obtained.
#'
#' @examples
#' library(photobiology)
#' library(ggplot2)
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(yellow_gel.spct) + geom_line() +
#'   stat_find_qtys(target = "half.range")
#' ggplot(yellow_gel.spct) + geom_line() +
#'   stat_find_qtys(target = c(490, 500, 510))
#' ggplot(yellow_gel.spct) + geom_line() +
#'   stat_find_qtys(target = 500, geom = "point", colour = "red") +
#'   stat_find_qtys(target = 500, geom = "text", colour = "red",
#'              hjust = 1.1, label.fmt = "Tfr = %1.2f")
#' @export
#' @family stats functions
#'
stat_find_qtys <- function(mapping = NULL, data = NULL, geom = "point",
                          target = "half.maximum", interpolate = TRUE,
                          label.fmt = "%.3g",
                          x.label.fmt = label.fmt, y.label.fmt = label.fmt,
                          position = "identity", na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFindQty, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(target = target,
                  interpolate = interpolate,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @keywords internal
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatFindQty <-
  ggplot2::ggproto("StatFindQty", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            target,
                                            interpolate,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     # By swapping the column names, we obtain qty values
                     hits.ls <- list()
                     for (t in target) {
                       hits.ls <-
                         c(hits.ls,
                           list(photobiology::find_wls(data,
                                                       .fun = `<=`,
                                                       target = t,
                                                       interpolate,
                                                       col.name.x = "y",
                                                       col.name = "x")))
                     }
                     wls.df <- dplyr::bind_rows(hits.ls)
                     dplyr::mutate(qty.df,
                                   x.label = sprintf(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y),
                                   color = photobiology::color_of(x, type = "CMF"),
                                   BW.color = black_or_white(photobiology::color_of(x, type = "CMF")))
                   },
                   default_aes = ggplot2::aes(label = ..y.label..,
                                              fill = ..color..,
                                              #                                              color = ..BW.color..,
                                              xintercept = ..x..,
                                              yintercept = ..y..),
                   required_aes = c("x", "y")
  )

