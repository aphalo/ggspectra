#' Find wavelength for target quantity value.
#'
#' \code{stat_find_wls} finds at which x positions values equal to a target are
#' located. \strong{Axis flipping is currently not supported.}
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
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt,x.label.fmt,y.label.fmt character  strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param x.label.transform,y.label.transform,x.colour.transform function Applied
#'   to \code{x} or \code{y} values when constructing the character labels or
#'   computing matching colours.
#'
#' @section Computed variables: \describe{ \item{x}{x-value at or nearest to the
#'   match to the target as numeric} \item{y}{target value or y-value nearest to
#'   the target as numeric} \item{x.label}{x-value at or nearest to the match
#'   formatted as character} \item{y.label}{target value or y-value nearest to
#'   the target formatted as character} \item{wl.color}{color definition calculated
#'   by assuming that x-values are wavelengths expressed in nanometres.} }
#'
#' @section Default aesthetics: Set by the statistic and available to geoms.
#'   \describe{ \item{label}{..x.label..} \item{xintercept}{..x..}
#'   \item{yintercept}{..y..} \item{fill}{..wl.color..} }
#'
#' @section Required aesthetics: Required by the statistic and need to be set
#'   with \code{aes()}. \describe{ \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity} }
#'
#' @seealso \code{\link[photobiology]{find_peaks}}.
#'
#' @details For each row in the subset of \code{data} matching  \code{target}
#'   a colour definition is computed assuming that after transformation with
#'   \code{x.colour.transform()} the values in \code{x} are wavelengths
#'   expressed in nanometres. Labels are constructed from \code{x} and \code{y}
#'   values after applying to them \code{x.label.transform} and
#'   \code{y.label.transform}, respectively. In most cases the
#'   \code{x.label.transform} is used to back-transform the values in \code{data}
#'   to make them agree with those displayed on the axis guides.
#'
#'   These stats use \code{geom_point} by default as it is a geometry likely to
#'   work well in almost any situation. The additional default aesthetic
#'   mappings set by these statistics allow their direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The format of the labels returned
#'   can be controlled by the user.
#'
#' @return A data frame with one row for each match to the target subset from
#'   the data or linearly interpolated between the two nearest values available.
#'   As spectra are not monotonic in the spectral quantity, this statistic can
#'   return more than one row in \code{data} per target value.
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
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   stat_find_wls(target = c(0.25, 0.5, 0.75))
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   stat_find_wls(target = "half.maximum", geom = "point", colour = "red") +
#'   stat_find_wls(target = "half.maximum", geom = "text", colour = "red",
#'              hjust = 1.1, label.fmt = "%3.0f nm")
#' @export
#' @family stats functions
#'
stat_find_wls <- function(mapping = NULL,
                          data = NULL,
                          geom = "point",
                          position = "identity",
                          ...,
                          target = "half.maximum",
                          interpolate = TRUE,
                          chroma.type = "CMF",
                          label.fmt = "%.3g",
                          x.label.fmt = label.fmt,
                          y.label.fmt = label.fmt,
                          x.label.transform = I,
                          y.label.transform = I,
                          x.colour.transform = x.label.transform,
                          na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {
  if (!(is.function(x.label.transform) &&
        is.function(y.label.transform) &&
        is.function(x.colour.transform))) {
    stop("'transform' arguments must be function defintions")
  }

  ggplot2::layer(
    stat = StatFindWls, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(target = target,
                  interpolate = interpolate,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  x.label.transform = x.label.transform,
                  y.label.transform = y.label.transform,
                  x.colour.transform = x.colour.transform,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#'
#' @export
#'
StatFindWls <-
  ggplot2::ggproto("StatFindWls", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            target,
                                            interpolate,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt,
                                            x.label.transform,
                                            y.label.transform,
                                            x.colour.transform) {
                     wls.df <- photobiology::wls_at_target(data,
                                                           x.var.name = "x",
                                                           y.var.name = "y",
                                                           target = target,
                                                           interpolate = interpolate,
                                                           na.rm = FALSE)
                     wls.df[["x.label"]] <-
                       sprintf(x.label.fmt, x.label.transform(wls.df[["x"]]))
                     wls.df[["y.label"]] <-
                       sprintf(y.label.fmt, y.label.transform(wls.df[["y"]]))
                     wls.df[["wl.color"]] <-
                       photobiology::fast_color_of_wl(x.colour.transform(wls.df[["x"]]),
                                                      chroma.type = chroma.type)
                     wls.df[["BW.color"]] <-
                       black_or_white(wls.df[["wl.color"]])
                     wls.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y),
                                              hjust = 0.5,
                                              vjust = 0.5),
                   required_aes = c("x", "y")
  )


#' Find quantity value for target wavelength value.
#'
#' \code{stat_find_qtys} finds at which y positions values equal to an x target
#' are located. \strong{Axis flipping is currently not supported.}
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
#' @param target numeric value indicating the spectral quantity value for which
#'   wavelengths are to be searched and interpolated if need. The character
#'   string "half.maximum" is also accepted as argument.
#' @param interpolate logical Indicating whether the nearest wavelength value
#'   in \code{x} should be returned or a value calculated by linear
#'   interpolation between wavelength values straddling the target.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt,x.label.fmt,y.label.fmt character  strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param x.label.transform,y.label.transform,x.colour.transform function Applied
#'   to \code{x} or \code{y} values when constructing the character labels or
#'   computing matching colours.
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
#' @return A data frame with one row for each match to the target subset from
#'   the data or linearly interpolated between the two nearest values available.
#'   As spectra are monotonic in wavelength, this statistic will never return
#'   more than one row in \code{data} per target value.
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
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   stat_find_qtys(target = "half.range")
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   stat_find_qtys(target = c(490, 500, 510))
#'
#' ggplot(yellow_gel.spct) +
#'   geom_line() +
#'   stat_find_qtys(target = 500, geom = "point", colour = "red") +
#'   stat_find_qtys(target = 500, geom = "text", colour = "red",
#'              hjust = 1.1, label.fmt = "Tfr = %1.2f")
#' @export
#' @family stats functions
#'
stat_find_qtys <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           ...,
                           target = "half.maximum",
                           interpolate = TRUE,
                           chroma.type = "CMF",
                           label.fmt = "%.3g",
                           x.label.fmt = label.fmt,
                           y.label.fmt = label.fmt,
                           x.label.transform = I,
                           y.label.transform = I,
                           x.colour.transform = x.label.transform,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {
  if (!(is.function(x.label.transform) &&
        is.function(y.label.transform) &&
        is.function(x.colour.transform))) {
    stop("'transform' arguments must be function defintions")
  }

  ggplot2::layer(
    stat = StatFindQty, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(target = target,
                  interpolate = interpolate,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  x.label.transform = x.label.transform,
                  y.label.transform = y.label.transform,
                  x.colour.transform = x.colour.transform,
                  na.rm = na.rm,
                  ...)
  )
}


#' @rdname gg2spectra-ggproto
#'
#' @export
#'
StatFindQty <-
  ggplot2::ggproto("StatFindQty", ggplot2::Stat,
                   compute_group =   function(data,
                                              scales,
                                              target,
                                              interpolate,
                                              chroma.type,
                                              label.fmt,
                                              x.label.fmt,
                                              y.label.fmt,
                                              x.label.transform,
                                              y.label.transform,
                                              x.colour.transform) {
                     # By swapping the column names, we obtain qty values instead of wls
                     if (is.numeric(target)) {
                       target <- target[target >= min(data[["x"]], na.rm = TRUE) &
                                          target <= max(data[["x"]], na.rm = TRUE)]
                     }
                     if (length(target) == 0L) {
                       # if target is NULL or out-of-range then return an empty data frame
                       rows.df <-
                         data.frame(x = numeric(),
                                    y = numeric(),
                                    x.label = character(),
                                    y.label = character(),
                                    color = character(),
                                    BW.color = character())
                     } else {
                       rows.df <- data.frame()
                       for (t in target) {
                         rows.df <-
                           rbind(rows.df,
                                 photobiology::find_wls(data,
                                                        .fun = `<=`,
                                                        target = t,
                                                        interpolate = interpolate,
                                                        col.name.x = "y",
                                                        col.name = "x"))
                       }
                       rows.df[["x.label"]] <-
                         sprintf(x.label.fmt, x.label.transform(rows.df[["x"]]))
                       rows.df[["y.label"]] <-
                         sprintf(y.label.fmt, y.label.transform(rows.df[["y"]]))
                       rows.df[["wl.color"]] <-
                         photobiology::fast_color_of_wl(x.colour.transform(rows.df[["x"]]),
                                                        chroma.type = chroma.type)
                       rows.df[["BW.color"]] <-
                         black_or_white(rows.df[["wl.color"]])
                     }
                     rows.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(y.label),
                                              fill = after_stat(wl.color),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y),
                                              hjust = 0.5,
                                              vjust = 0.5),
                   required_aes = c("x", "y")
  )

