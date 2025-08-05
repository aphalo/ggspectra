#' Create a new ggplot plot from spectral data.
#'
#' \code{\link[ggplot2]{ggplot}} methods initialize a ggplot object. They can be
#' used to declare the input data object for a graphic and to optionally specify
#' the set of plot aesthetics intended to be common throughout all subsequent
#' layers unless specifically overridden. The method specializations from
#' package 'ggspectra' support the classes for storage of spectral data from
#' package '\link[photobiology:photobiology-package]{photobiology}'.
#'
#' \code{ggplot} is typically used to construct a plot incrementally, using
#' the \code{+} operator to add layers to the existing ggplot object. This is
#' advantageous in that the code is explicit about which layers are added and
#' the order in which they are added. For complex graphics with multiple layers,
#' initialization with \code{ggplot} is recommended.
#'
#' We show seven common ways to invoke \code{ggplot} methods for spectra and
#' collections of spectra:
#' \itemize{
#'    \item \code{ggplot(spct)}
#'    \item \code{ggplot(spct, unit.out = <unit.to.use>)}
#'    \item \code{ggplot(spct, plot.qty = <quantity.to.plot>)}
#'    \item \code{ggplot(spct, range = <wavelength.range>)}
#'    \item \code{ggplot(spct) + aes(<other aesthetics>)}
#'    \item \code{ggplot(spct, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(spct, aes())}
#'   }
#' The first approach is recommended if all layers use the same data and the same
#' set of automatic default x and y aesthetics. The second, third and fourth use
#' automatic default x and y aesthetics but first transform or trim the spectral
#' data to be plotted. The fifth uses automatic default x and y aesthetics and
#' adds mappings for other aesthetics. These patterns can be combined as needed.
#' The sixth overrides the default automatic mapping, while the seventh
#' delays the mapping of aesthetics and can be convenient when using different
#' mappings for different geoms.
#'
#' @param data Default spectrum dataset to use for plot. If not a spectrum, the
#'   methods used will be those defined in package \code{ggplot2}. See
#'   \code{\link[ggplot2]{ggplot}}. If not specified, must be supplied in each
#'   layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. If not
#'   specified, in the case of spectral objects, a default mapping will be used.
#' @param ... Other arguments passed on to methods.
#' @param range an R object on which range() returns a vector of length 2, with
#'   min and max wavelengths (nm).
#' @param unit.out character string indicating type of units to use for
#'   plotting spectral irradiance or spectral response, \code{"photon"} or
#'   \code{"energy"}.
#' @param plot.qty character string One of \code{"transmittance"},
#'   \code{"absorptance"} or \code{"absorbance"} for \code{filter_spct} objects,
#'   and in addition to these \code{"reflectance"}, \code{"all"} or
#'   \code{"as.is"} for \code{object_spct} objects.
#' @param by.group logical flag If \code{FALSE}, the default, individual spectra
#'   are mapped to the \code{group} aesthetic to ensure separate lines are
#'   plotted. Must be set to \code{by.group = TRUE} when plots are animated with
#'   'gganimate' "by group" as the grouping for animation is NOT set using
#'   \code{aes}.
#' @param environment If a variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called. The use of
#'   these parameter has been deprecated in 'ggplot2' in favour of "tidy
#'   evaluation".
#'
#' @details When using the default automatic mapping to \emph{x} and \emph{y}
#'   aesthetics, unit or quantity conversions are done on the fly according to
#'   the arguments passed to parameters \code{unit.out} and \code{plot.qty}. In
#'   contrast, if a mapping for \emph{x} and/or \emph{y} aesthetics is passed as
#'   an argument to parameter \code{mapping}, the arguments to parameters
#'   \code{unit.out} and \code{plot.qty} are ignored and all the mapped
#'   variables should be present in the spectral object passed as argument to
#'   data.
#'
#'   The current implementation merges the default mapping for \emph{x} and
#'   \emph{y} aesthetics with the user supplied mapping if it only contains
#'   mappings to aesthetics other than \emph{x} or \emph{y} or an empty
#'   mapping. In addition, when the user does not pass an argument to
#'   \code{mapping}, not even an empty one, if the object contains
#'   multiple spectra, a mapping of the indexing factor to the \code{group}
#'   aesthetic is added. The name of the id factor is retrieved
#'   from the \code{data} object metadata.
#'
#'   Differently to objects of other spectral classes, objects of class
#'   \code{\link[photobiology]{object_spct}} contain data for multiple physical
#'   quantities. Thus, in the case of class \code{object_spct}, the special
#'   arguments \code{"all"} and \code{"as.is"} can be passed as argument to
#'   \code{plot.qty}. Where \code{all}, the defaul indicates that the data are
#'   to be converted into long form and indexed with a factor named
#'   \code{variable}, to allow stacking or faceting. In contrast, \code{"as.is"}
#'   indicates that data for the different quantities should remain in separate
#'   variables (=columns) when added to the plot object. \code{"reflectance"}
#'   passed as argument to \code{plot.qty} triggers conversion of the
#'   \code{object_spct} object passed as argument to \code{data} into a
#'   \code{\link[photobiology]{reflector_spct}} object and \code{"absorbance"},
#'   \code{"absorptance"} and \code{"reflectance"}, trigger conversion into a
#'   \code{\link[photobiology]{filter_spct}} object. After conversion the
#'   objects are forwarded to the matching \code{ggplot} method.
#'
#'   The methods for collections of spectra accept arguments
#'   through additional. When plotting collections of spectra a factor named as
#'   indicated by the argument passed to parameter \code{idfactor}, or
#'   \code{"spct.idx"} by default, is added using as levels the names of the
#'   individual members of the collection. The spectral object is forwarded
#'   to the \code{ggplot} method matching its new class.
#'
#'   \emph{Heterogeneous generic collections of spectra containing members
#'   belonging to more than one class are not supported.}
#'
#' @return A ggplot object, containing data and mapping of data to aesthetics
#'   but no plot layers.
#'
#' @seealso Method \code{link[ggspectra]{autoplot}} provides further automation
#'   of plot creation. Function \code{\link[photobiology]{rbindspct}} is used to
#'   convert collections of spectra into "long-form" spectral objects. The
#'   generic of method \code{link[ggplot2](ggplot)} is defined in package
#'   'ggplot2'.
#'
#' @export
#' @examples
#' # source
#' ggplot(sun.spct) + geom_line()
#' ggplot(sun.spct, unit.out = "photon") + geom_line()
#'
#' # multiple spectra in long form
#' ggplot(sun_evening.spct) + geom_line()
#' ggplot(sun_evening.spct, aes(linetype = spct.idx)) + geom_line()
#'
#' # collection of spectra
#' ggplot(sun_evening.mspct, idfactor = "step") +
#'   geom_line()
#' ggplot(sun_evening.mspct, idfactor = "step", aes(colour = step)) +
#'   geom_line()
#'
#' # filter
#' ggplot(yellow_gel.spct) + geom_line()
#' ggplot(yellow_gel.spct, plot.qty = "absorbance") + geom_line()
#'
#' # object
#' ggplot(Ler_leaf.spct) + facet_grid(~variable) + geom_line()
#' ggplot(Ler_leaf.spct) + aes(fill = variable) + geom_area()
#' ggplot(Ler_leaf.spct) + aes(linetype = variable) + geom_line()
#' ggplot(Ler_leaf.spct, plot.qty = "absorptance") + geom_line()
#'
#' @name ggplot
#'
ggplot.source_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           by.group = FALSE,
           environment = parent.frame()) {

    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data, action = "replace")
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.e.irrad"]])
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data, action = "replace")
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.q.irrad"]])
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.response_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data)
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.e.response"]])
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data)
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["s.q.response"]])
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.filter_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.filter.qty",
                                default = "transmittance"),
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if (plot.qty == "transmittance") {
        data <- photobiology::any2T(data, action = "replace")
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["Tfr"]])
      } else if (plot.qty == "absorptance") {
        data <- photobiology::any2Afr(data, action = "replace")
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["Afr"]])
      } else if (plot.qty == "absorbance") {
        data <- photobiology::any2A(data, action = "replace")
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["A"]])
      } else {
        stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @note plot.qty is ignored for reflectors.
#'
#' @export
#'
ggplot.reflector_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = NULL,
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["Rfr"]])
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.cps_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if ("cps" %in% names(data)) {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["cps"]])
      } else {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["cps_1"]])
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.calibration_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if ("irrad.mult" %in% names(data)) {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["irrad.mult"]])
      } else {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["irrad.mult_1"]])
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.raw_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           by.group = FALSE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    auto.map <- is.null(mapping)
    if (auto.map) {
      mapping <- ggplot2::aes()
    }
    if (auto.map || !any(c("x", "y") %in% names(mapping))) {
      if ("counts" %in% names(data)) {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["counts"]])
      } else {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["counts_1"]])
      }
      mapping <- utils::modifyList(mapping, xy.mapping)
    }
    # roundabout way of retaining the derived classes without calling any
    # private (not exported) method or function from 'ggplot2'
    data.tb <- data
    removed <- photobiology::rmDerivedSpct(data.tb)
    p <- ggplot2::ggplot(data = data.tb, mapping = mapping,
                         environment = environment)
    class(p[["data"]]) <- c(removed, class(p[["data"]]))

    # we also copy other attributes
    p[["data"]] <- photobiology::copy_attributes(data, p[["data"]])

    if (auto.map) {
      # we look for multiple spectra in long form
      num.spectra <- photobiology::getMultipleWl(p[["data"]])
      if (num.spectra > 1 && !by.group) {
        p <- p +
          ggplot2::aes(group = .data[[id_factor(p[["data"]])]])
      }
    }
    p
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.object_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.object.qty", default = "all"),
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    # If plotting a single variable, we use other methods
    if (plot.qty == "reflectance") {
      return(ggplot(data = photobiology::as.reflector_spct(data),
                    mapping = mapping,
                    ...))
    } else if (plot.qty %in% c("transmittance", "absorbance", "absorptance")) {
      return(ggplot(data = photobiology::as.filter_spct(data),
                    mapping = mapping,
                    plot.qty = plot.qty,
                    ...))
    } else if (!plot.qty %in% c("all", "as.is")) {
      stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
    }
    # compute absorptance
    data[["Afr"]] <- 1.0 - data[["Tfr"]] - data[["Rfr"]]
    if (any((data[["Afr"]]) < -0.01)) {
      message("Bad data or fluorescence.")
    }
    if (plot.qty == "as.is") {
      data.class <- class(data)
      photobiology::rmDerivedSpct(data)
      p <- ggplot2::ggplot(data = data, mapping = mapping,
                           environment = environment)
      class(p[["data"]]) <- data.class
    } else {
      num.spectra <- photobiology::getMultipleWl(data)
      if (num.spectra > 1) {
        stop("Multiple spectra in long form not supported.")
      }
      # Attributes will be lost when melting the tibble or data frame
      data.attributes <- photobiology::get_attributes(data)
      # Once molten it will not pass checks as object_spct
      molten.tb <- photobiology::spct_wide2long(data, idfactor = "variable", rm.spct.class = TRUE)
      # if not supplied create a mapping
      auto.map <- is.null(mapping)
      if (auto.map) {
        mapping <- ggplot2::aes()
      }
      if (auto.map || !any(c("x", "y") %in% names(mapping))) {
        xy.mapping <- ggplot2::aes(.data[["w.length"]], .data[["value"]])
        mapping <- utils::modifyList(mapping, xy.mapping)
      }
      # roundabout way of retaining the attributes without calling any
      # private (not exported) method or function from 'ggplot2'
      p <- ggplot2::ggplot(data = molten.tb, mapping = mapping, ...,
                           environment = environment)
      attributes(p$data) <- c(attributes(p$data), data.attributes)
    }
    p
  }

#' @rdname ggplot
#'
#' @param spct_class character Class into which a \code{generic_spct} object
#'   will be converted before plotting. The column names in data should
#'   match those expected by the class constructor (see
#'   \code{\link[photobiology]{setGenericSpct}}); other arguments should be
#'   passed by name). If the argument is \code{"generic_spct"}, \code{"tibble"}
#'   or \code{"data.frame"} no aesthetic mapping will be set auutomatically.
#'
#' @export
#'
ggplot.generic_spct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           spct_class,
           environment = parent.frame()) {

    if (!spct_class %in% photobiology::spct_classes()) {
      stop("Invalid 'spct_class' argument: \"", spct_class)
    }

    if (!is.null(range)) {
      data <- photobiology::trim_wl(x = data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }

    # this list could be defined in package 'photobiology'
    funs <- list(filter_spct = photobiology::setFilterSpct,
                 reflector_spct = photobiology::setReflectorSpct,
                 object_spct = photobiology::setObjectSpct,
                 source_spct = photobiology::setSourceSpct,
                 response_spct = photobiology::setResponseSpct,
                 chroma_spct = photobiology::setChromaSpct,
                 raw_spct = photobiology::setRawSpct,
                 cps_spct = photobiology::setCpsSpct,
                 calibration_spct = photobiology::setCalibrationSpct,
                 generic_spct = tibble::as_tibble,
                 tibble = tibble::as_tibble,
                 data.frame = as.data.frame)
    spct <- funs[[spct_class]](data, ...)

    # dispatched to one of the methods defined above
    ggplot2::ggplot(data = spct,
                    mapping = mapping,
                    environment = environment)
  }


# collections of spectra --------------------------------------------------

#' @rdname ggplot
#'
#' @inheritParams photobiology::rbindspct
#'
#' @export
#'
ggplot.generic_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           idfactor = TRUE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    spct <- photobiology::rbindspct(l = data, idfactor = idfactor)
    ggplot2::ggplot(data = spct,
                    mapping = mapping,
                    ...,
                    environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.filter_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.filter.qty",
                                default = "transmittance"),
           idfactor = TRUE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    # conversion before binding as filter properties can differ
    if (is.null(mapping) || !any(c("x", "y") %in% names(mapping))) {
      if (plot.qty == "transmittance") {
        data <- photobiology::any2T(data, action = "replace")
      } else if (plot.qty == "absorptance") {
        data <- photobiology::any2Afr(data, action = "replace")
      } else if (plot.qty == "absorbance") {
        data <- photobiology::any2A(data, action = "replace")
      } else {
        stop("Invalid 'plot.qty' argument value: '", plot.qty, "'")
      }
    }
    spct <- photobiology::rbindspct(l = data, idfactor = idfactor)
    ggplot(data = spct,
           mapping = mapping,
           plot.qty = plot.qty,
           ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.source_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           idfactor = TRUE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    # conversion before binding, to avoid multiple conversions
    if (is.null(mapping) || !any(c("x", "y") %in% names(mapping))) {
      if (unit.out == "energy") {
        data <- photobiology::q2e(data, action = "replace")
      } else if (unit.out %in% c("photon", "quantum")) {
        data <- photobiology::e2q(data, action = "replace")
      } else {
        stop("Invalid 'unit.out' argument value: '", unit.out, "'")
      }
    }
    spct <- photobiology::rbindspct(l = data, idfactor = idfactor)
    ggplot(data = spct,
           mapping = mapping,
           unit.out = unit.out,
           ...,
           environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.object_mspct <-
  function(data,
           mapping = NULL,
           ...,
           range = NULL,
           plot.qty = getOption("photobiology.object.qty",
                                default = ifelse(length(data) > 1L,
                                                 "as.is", "all")),
           idfactor = TRUE,
           environment = parent.frame()) {
    if (!is.null(range)) {
      data <- photobiology::trim_wl(data,
                                    range = range,
                                    use.hinges = TRUE,
                                    fill = NULL)
    }
    spct <- photobiology::rbindspct(l = data, idfactor = idfactor)
    ggplot(data = spct,
           mapping = mapping,
           ...,
           plot.qty = plot.qty,
           environment = environment)
  }
