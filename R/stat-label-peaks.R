#' Label peaks and valleys.
#'
#' \code{stat_labels_peaks} finds at which x positions local maxima are located,
#' and adds labels and colors to the data without subsetting. To find local
#' minima, you can use \code{stat_labels_valleys} instead. The variable
#' mapped to the \code{x} aesthetic is expected to contain wavelength values
#' expressed in nanometres. \strong{Axis flipping is currently not supported.}
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
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 5, meaning that a peak is bigger than two
#'   consequtive neighbors on each side. Default: 5.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: FALSE.
#' @param chroma.type character one of "CMF" (color matching function) or "CC"
#'   (color coordinates) or a \code{\link[photobiology]{chroma_spct}} object.
#' @param label.fmt,x.label.fmt,y.label.fmt character  strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param x.label.transform,y.label.transform,x.colour.transform function Applied
#'   to \code{x} or \code{y} values when constructing the character labels or
#'   computing matching colours.
#' @param label.fill character string to use for labels not at peaks or valleys
#'   being highlighted.
#'
#' @return The original data with additional computed variables added.
#'
#' @section Computed variables: \describe{ \item{x.label}{x-value at a peak (or
#'   valley) formatted as character or otherwise the
#'   value passed to \code{label.fill} which defaults to an
#'   empty string (\code{""}).}
#'   \item{y.label}{y-value at the peak (or valley) formatted as character or
#'    otherwise the value passed to \code{label.fill} which defaults to an
#'   empty string (\code{""}).}
#'   \item{wl.color}{At peaks and valleys,
#'   color definition calculated by assuming that x-values are wavelengths
#'   expressed in nanometres, otherwise, \code{rgb(1, 1, 1, 0)} (transparent
#'   white).} }
#'
#' @section Default aesthetics: Set by the statistic and available to geoms.
#'   \describe{ \item{label}{..x.label..} \item{xintercept}{..x..}
#'   \item{yintercept}{..y..} \item{color}{black_or_white(..wl.color..)}
#'   \item{fill}{..wl.color..} }
#'
#' @section Required aesthetics: Required by the statistic and need to be set
#'   with \code{aes()}. \describe{ \item{x}{numeric, wavelength in nanometres}
#'   \item{y}{numeric, a spectral quantity} }
#'
#' @seealso \code{\link{stat_peaks}}, \code{\link{stat_valleys}} and
#'   \code{\link[photobiology]{find_peaks}}, which is used internally.
#'
#' @details These statistics assemble text labels for each peak or valley and
#'   compute the colour corresponding to the wavelength of the peaks and
#'   valleys. Defaults work as long as the variable mapped to the \code{x}
#'   aesthetic contains wavelengths expressed in nanometres and the plot has
#'   an x-scale that does not apply a transformation. The three \code{transform}
#'   parameters can be used to back-transform the values when scales apply
#'   transformations so that peak/valley labels and axis labels match. Of
#'   course, \code{x.label.transform} and \code{y.label.transform} make also
#'   possible to scale the values in the labels.
#'
#'   Both statistics use \code{geom_text} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. These
#'   statistics work best with \code{geom_text_repel} and
#'   \code{geom_label_repel} from package 'ggrepel' as they are designed so that
#'   peak or valley labels will not overlap any observation in the whole data
#'   set. Default aesthetics set by these statistics allow their direct use with
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
#'
#' # ggplot() methods for spectral objects set a default mapping for x and y.
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_label_peaks(hjust = "left", span = 31, angle = 90, color = "red")
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_label_valleys(hjust = "right", span = 21, angle = 90, color = "blue")
#'
#' # using transformed scales requires the user to pass functions as arguments
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_label_peaks(hjust = "left", span = 31, angle = 90, color = "red",
#'                    x.label.transform = abs) +
#'   scale_x_reverse()
#'
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_label_peaks(hjust = "left", span = 31, angle = 90, color = "red",
#'                    x.label.transform = function(x) {10^x}) +
#'   scale_x_log10()
#'
#' # geom_label
#' ggplot(sun.spct) +
#'   geom_line() +
#'   stat_peaks(span = 41, shape = 21, size = 3) +
#'   stat_label_peaks(span = 41, geom = "label", label.fmt = "%3.0f nm") +
#'   scale_fill_identity() +
#'   scale_color_identity() +
#'   expand_limits(y = c(NA, 1))
#'
#' # using 'ggrepel' to avoid overlaps
#' # too slow for CRAN checks
#' \dontrun{
#' library(ggrepel)
#'
#' ggplot(sun.spct) + geom_line() +
#'   stat_peaks(span = 41, shape = 21, size = 3) +
#'   stat_label_peaks(span = 41, geom = "label_repel", segment.colour = "red",
#'                    nudge_y = 0.12, label.fmt = "%3.0f nm",
#'                    max.overlaps = Inf, min.segment.length = 0) +
#'   scale_fill_identity() +
#'   scale_color_identity() +
#'   expand_limits(y = c(NA, 1))
#' }
#'
#' @export
#' @family stats functions
#'
stat_label_peaks <-
  function(mapping = NULL,
           data = NULL,
           geom = "text",
           position = "identity",
           ...,
           span = 5,
           ignore_threshold = 0,
           strict = TRUE,
           chroma.type = "CMF",
           label.fmt = "%.3g",
           x.label.fmt = label.fmt,
           y.label.fmt = label.fmt,
           x.label.transform = I,
           y.label.transform = I,
           x.colour.transform = x.label.transform,
           label.fill = "",
           na.rm = TRUE,
           show.legend = FALSE,
           inherit.aes = TRUE) {
    if (!(is.function(x.label.transform) &&
          is.function(y.label.transform) &&
          is.function(x.colour.transform))) {
      stop("'transform' arguments must be function defintions")
    }

    # for performance
    if (!grepl("repel$", geom) && label.fill == "") {
      label.fill <- NA_character_
    }

    ggplot2::layer(
      stat = StatLabelPeaks, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(span = span,
                    ignore_threshold = ignore_threshold,
                    strict = strict,
                    chroma.type = chroma.type,
                    label.fmt = label.fmt,
                    x.label.fmt = x.label.fmt,
                    y.label.fmt = y.label.fmt,
                    x.label.transform = x.label.transform,
                    y.label.transform = y.label.transform,
                    x.colour.transform = x.colour.transform,
                    label.fill = label.fill,
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
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object,
#' descended from the top-level \code{Stat}, and each implements various methods
#' and fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @keywords internal
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
StatLabelPeaks <-
  ggplot2::ggproto("StatLabelPeaks", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            span,
                                            ignore_threshold,
                                            strict,
                                            chroma.type,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt,
                                            x.label.transform,
                                            y.label.transform,
                                            x.colour.transform,
                                            label.fill) {
                     if (!is.character(label.fill)) {
                       as.character(label.fill)
                     }
                     out.df <- data # previously conversion into tibble
                     if (is.null(span)) {
                       peaks.idx <- which.max(data[["y"]])
                     } else {
                       peaks.idx <-
                         photobiology::find_peaks(data[["y"]],
                                                  span = span,
                                                  ignore_threshold = ignore_threshold,
                                                  strict = strict)
                     }
                     out.df[["is_peak"]] <- FALSE
                     out.df[peaks.idx, "is_peak"] <- TRUE
                     out.df[["x.label"]] <-
                       ifelse(out.df[["is_peak"]],
                              sprintf(x.label.fmt, x.label.transform(out.df[["x"]])),
                              label.fill)
                     out.df[["y.label"]] <-
                       ifelse(out.df[["is_peak"]],
                              sprintf(y.label.fmt, y.label.transform(out.df[["y"]])),
                              label.fill)
                     out.df[["wl.color"]] <-
                       ifelse(out.df[["is_peak"]],
                              photobiology::fast_color_of_wl(x.colour.transform(out.df[["x"]]),
                                                             chroma.type = chroma.type),
                              rgb(1, 1, 1, 0))
                     out.df[["BW.color"]] <-
                       ifelse(out.df[["is_peak"]],
                              black_or_white(out.df[["wl.color"]]),
                              rgb(0, 0, 0, 0))
                     out.df[["lab.hjust"]] <- 0.5
                     out.df[["lab.vjust"]] <- -0.2
                     out.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              color = after_stat(BW.color),
                                              segment.color = "black",
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)#,
                                              # hjust = after_stat(lab.hjust),
                                              # vjust = after_stat(lab.vjust)
                   ),
                   required_aes = c("x", "y")
  )

#' @rdname stat_label_peaks
#'
#' @export
#'
stat_label_valleys <- function(mapping = NULL,
                               data = NULL,
                               geom = "text",
                               position = "identity",
                               ...,
                               span = 5,
                               ignore_threshold = 0,
                               strict = TRUE,
                               chroma.type = "CMF",
                               label.fmt = "%.3g",
                               x.label.fmt = label.fmt,
                               y.label.fmt = label.fmt,
                               x.label.transform = I,
                               y.label.transform = I,
                               x.colour.transform = x.label.transform,
                               label.fill = "",
                               na.rm = TRUE,
                               show.legend = FALSE,
                               inherit.aes = TRUE) {
  if (!(is.function(x.label.transform) &&
        is.function(y.label.transform) &&
        is.function(x.colour.transform))) {
    stop("'transform' arguments must be function defintions")
  }

  # for performance
  if (!grepl("repel$", geom) && label.fill == "") {
    label.fill <- NA_character_
  }

  ggplot2::layer(
    stat = StatLabelValleys, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  chroma.type = chroma.type,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  x.label.transform = x.label.transform,
                  y.label.transform = y.label.transform,
                  x.colour.transform = x.colour.transform,
                  label.fill = label.fill,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#'
#' @export
#'
StatLabelValleys <-
  ggplot2::ggproto("StatLabelValleys", ggplot2::Stat,
                   compute_group =   function(data,
                                              scales,
                                              span,
                                              ignore_threshold,
                                              strict,
                                              chroma.type,
                                              label.fmt,
                                              x.label.fmt,
                                              y.label.fmt,
                                              x.label.transform,
                                              y.label.transform,
                                              x.colour.transform,
                                              label.fill) {
                     if (!is.character(label.fill)) {
                       as.character(label.fill)
                     }
                     out.df <- data
                     if (is.null(span)) {
                       valleys.idx <- which.min(data[["y"]])
                     } else {
                       valleys.idx <-
                         photobiology::find_peaks(-data[["y"]],
                                                  span = span,
                                                  ignore_threshold = ignore_threshold,
                                                  strict = strict)
                     }
                     out.df[["is_valley"]] <- FALSE
                     out.df[valleys.idx, "is_valley"] <- TRUE
                     out.df[["x.label"]] <-
                       ifelse(out.df[["is_valley"]],
                              sprintf(x.label.fmt, x.label.transform(out.df[["x"]])),
                              label.fill)
                     out.df[["y.label"]] <-
                       ifelse(out.df[["is_valley"]],
                              sprintf(y.label.fmt, y.label.transform(out.df[["y"]])),
                              label.fill)
                     out.df[["wl.color"]] <-
                       ifelse(out.df[["is_valley"]],
                              photobiology::fast_color_of_wl(x.colour.transform(out.df[["x"]]),
                                                             chroma.type = chroma.type),
                              rgb(1, 1, 1, 0))
                     out.df[["BW.color"]] <- ifelse(out.df[["is_valley"]],
                                                    black_or_white(out.df[["wl.color"]]),
                                                    rgb(0, 0, 0, 0))
                     out.df[["lab.hjust"]] <- 0.5
                     out.df[["lab.vjust"]] <- 1.2
                     out.df
                   },
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              fill = after_stat(wl.color),
                                              color = after_stat(BW.color),
                                              segment.color = "black",
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)#,
 #                                             hjust = after_stat(lab.hjust),
 #                                             vjust = after_stat(lab.vjust)
                                              ),
                   required_aes = c("x", "y")
)

