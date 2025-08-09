context("autoplot-filter")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
Ler_leaf_trns.spct <- interpolate_spct(photobiology::Ler_leaf_trns.spct, length.out = length.out.spct)
Ler_leaf_trns.spct <- clean(Ler_leaf_trns.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf_trns_i.spct <- interpolate_spct(photobiology::Ler_leaf_trns_i.spct, length.out = length.out.spct)
Ler_leaf_trns_i.spct <- clean(Ler_leaf_trns_i.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf_rflt.spct <- interpolate_spct(photobiology::Ler_leaf_rflt.spct, length.out = length.out.spct)
Ler_leaf_rflt.spct <- clean(Ler_leaf_rflt.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf.spct <- clean(Ler_leaf.spct, min.Afr = 1e-5, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf.spct <- interpolate_spct(photobiology::Ler_leaf.spct, length.out = length.out.spct)
Ler_leaf.spct <- clean(Ler_leaf.spct, min.Afr = 1e-5, range.s.data = c(1e-5, 1 - 1e-5))


test_that("filter_spct default", {
  set_annotations_default()
  Tfr_as_default()

  vdiffr::expect_doppelganger("filter-default-tot-scaled",
                              autoplot(fscale(Ler_leaf_trns.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("filter-default-tot-normalized",
                              autoplot(normalize(Ler_leaf_trns.spct, norm = "max")))

  vdiffr::expect_doppelganger("filter-default-tot",
                              autoplot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("filter-default-tot2a",
                              autoplot(Ler_leaf_trns.spct, plot.qty = "absorbance"))
  # vdiffr::expect_doppelganger("filter-default-tot2afr",
  #                             autoplot(Ler_leaf_trns.spct, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-default-tot-ylim",
                              autoplot(Ler_leaf_trns.spct, ylim = c(-0.05, 0.5)))
  vdiffr::expect_doppelganger("filter-default-pc-out-tot",
                              autoplot(Ler_leaf_trns.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tot",
                              autoplot(Ler_leaf_trns.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tot",
                              autoplot(Ler_leaf_trns.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tot",
                              autoplot(Ler_leaf_trns.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-shrink",
                              autoplot(Ler_leaf_trns.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-num-shrink-long",
                              autoplot(Ler_leaf_trns.spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("filter-range-num-shrink-r",
                              autoplot(Ler_leaf_trns.spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("filter-range-num-shrink-l",
                              autoplot(Ler_leaf_trns.spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("filter-range-num-expand",
                              autoplot(Ler_leaf_trns.spct, range = c(150, 1000)))
  vdiffr::expect_doppelganger("filter-range-num-expand-long",
                              autoplot(Ler_leaf_trns.spct, range = c(150, 600, 1000)))
  vdiffr::expect_doppelganger("filter-range-num-expand-r",
                              autoplot(Ler_leaf_trns.spct, range = c(NA, 1000)))
  vdiffr::expect_doppelganger("filter-range-num-expand-l",
                              autoplot(Ler_leaf_trns.spct, range = c(150, NA)))
  vdiffr::expect_doppelganger("filter-range-wb-tot",
                              autoplot(Ler_leaf_trns.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-plus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("filter-default-scaled",
                              autoplot(fscale(Ler_leaf_trns_i.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("filter-default-normalized",
                              autoplot(normalize(Ler_leaf_trns_i.spct, norm = "max")))

  vdiffr::expect_doppelganger("filter-default",
                              autoplot(Ler_leaf_trns_i.spct))
  vdiffr::expect_doppelganger("filter-default-ylim",
                              autoplot(Ler_leaf_trns_i.spct, ylim = c(NA, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out",
                              autoplot(Ler_leaf_trns_i.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size",
                              autoplot(Ler_leaf_trns_i.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31",
                              autoplot(Ler_leaf_trns_i.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis",
                              autoplot(Ler_leaf_trns_i.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num",
                              autoplot(Ler_leaf_trns_i.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb",
                              autoplot(Ler_leaf_trns_i.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations",
                              autoplot(Ler_leaf_trns_i.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-reserve-space",
                              autoplot(Ler_leaf_trns_i.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("filter-minus-summaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "segments")))
})

test_that("filter_spct absorptance", {
  set_annotations_default()
  Afr_as_default()

  Ler_leaf_Afr_i_spct <- T2Afr(Ler_leaf_trns_i.spct)

  vdiffr::expect_doppelganger("filter-default-apt",
                              autoplot(Ler_leaf_Afr_i_spct))
  vdiffr::expect_doppelganger("filter-default-apt2t",
                              autoplot(Ler_leaf_Afr_i_spct,
                                       plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("filter-default-apt2a",
                              autoplot(Ler_leaf_Afr_i_spct,
                                       plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("filter-default-apt-ylim",
                              autoplot(Ler_leaf_Afr_i_spct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out-apt",
                              autoplot(Ler_leaf_Afr_i_spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-apt",
                              autoplot(Ler_leaf_Afr_i_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-apt",
                              autoplot(Ler_leaf_Afr_i_spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-apt",
                              autoplot(Ler_leaf_Afr_i_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-apt",
                              autoplot(Ler_leaf_Afr_i_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-apt",
                              autoplot(Ler_leaf_Afr_i_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-apt",
                              autoplot(Ler_leaf_Afr_i_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-apt",
                              autoplot(Ler_leaf_Afr_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-q",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("+", "segments")))
})

test_that("filter_spct A", {
  set_annotations_default()
  A_as_default()

  Ler_leaf_A_i_spct <- T2A(Ler_leaf_trns_i.spct, action = "replace")

  vdiffr::expect_doppelganger("filter-default-a2t",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA),
                                       plot.qty = "transmittance"))
  # vdiffr::expect_doppelganger("filter-default-a2afr",
  #                             autoplot(Ler_leaf_A_i_spct, range = c(300, NA),
  #                                      plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-default-a-scaled",
                              autoplot(fscale(Ler_leaf_A_i_spct, target = 2,
                                              range = c(400,700), set.scaled = TRUE),
                                       plot.qty = "absorbance", range = c(300, NA)))
  vdiffr::expect_doppelganger("filter-default-a-normalized",
                               autoplot(normalize(Ler_leaf_A_i_spct, norm = "max"),
                                         range = c(300, NA)))

  vdiffr::expect_doppelganger("filter-default-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA)))
  vdiffr::expect_doppelganger("filter-default-a-ylim",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), ylim = c(-0.2, 4)))
  vdiffr::expect_doppelganger("filter-text-size-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), span = 11))
  vdiffr::expect_doppelganger("filter-wb-vis-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-wb-a",
                              autoplot(Ler_leaf_A_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("+", "segments")))
})

test_that("filter_spct Tfr", {
  set_annotations_default()
  Tfr_as_default()

  Ler_leaf_t_i_spct <- any2T(Ler_leaf_trns_i.spct, action = "replace")

  vdiffr::expect_doppelganger("filter-default-tfr",
                              autoplot(Ler_leaf_t_i_spct))
  vdiffr::expect_doppelganger("filter-default-tfr-ylim",
                              autoplot(Ler_leaf_t_i_spct, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out-tfr",
                              autoplot(Ler_leaf_t_i_spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tfr",
                              autoplot(Ler_leaf_t_i_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tfr",
                              autoplot(Ler_leaf_t_i_spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tfr",
                              autoplot(Ler_leaf_t_i_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tfr",
                              autoplot(Ler_leaf_t_i_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tfr",
                              autoplot(Ler_leaf_t_i_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-tfr",
                              autoplot(Ler_leaf_t_i_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-tfr",
                              autoplot(Ler_leaf_t_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("+", "segments")))
})

test_that("filter_mspct", {
  unset_filter_qty_default()
  set_annotations_default()

  two_leaves.mspct <- filter_mspct(list(one = Ler_leaf_trns_i.spct,
                                      half = Ler_leaf_trns_i.spct / 2))
  vdiffr::expect_doppelganger("filter-mspct-default",
                              autoplot(two_leaves.mspct))
  vdiffr::expect_doppelganger("filter-mspct-default-ylim",
                              autoplot(two_leaves.mspct, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("filter-mspct-default-range",
                              autoplot(two_leaves.mspct, range = c(500, 700)))
  # use range to avoid Tfr == 0 values.
  vdiffr::expect_doppelganger("filter-mspct-default-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("filter-mspct-default-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("filter-mspct-default-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("filter-mspct-no-annotations",
                              autoplot(two_leaves.mspct, annotations = ""))
  vdiffr::expect_doppelganger("filter-mspct-reserve-space",
                              autoplot(two_leaves.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("filter-mspct-mean",
                              autoplot(two_leaves.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("filter-mspct-median",
                              autoplot(two_leaves.mspct, plot.data = "median"))
  # triggers warning
  # vdiffr::expect_doppelganger("filter-mspct-sum",
  #                             autoplot(two_leaves.mspct, plot.data = "sum"))
  testthat::expect_warning(autoplot(two_leaves.mspct, plot.data = "sum"))
  vdiffr::expect_doppelganger("filter-mspct-sum",
                               autoplot(two_leaves.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("filter-mspct-se",
                              autoplot(two_leaves.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("filter-mspct-se-ylab",
                              autoplot(two_leaves.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("filter-mspct-var",
                              autoplot(two_leaves.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("filter-mspct-var-ylab",
                              autoplot(two_leaves.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("filter-mspct-mean-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("filter-mspct-median-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"), plot.data = "median"))
  # Bug in photobiology::rowwise_filter() (fixed in photobiology >= 0.10.11) affected next two staements
  vdiffr::expect_doppelganger("filter-mspct-mean-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("filter-mspct-median-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"), plot.data = "median"))
  vdiffr::expect_doppelganger("filter-mspct-mean-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("filter-mspct-median-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"), plot.data = "median"))
})

test_that("reflector_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("reflector-default-tot-scaled",
                              autoplot(fscale(Ler_leaf_rflt.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("reflector-default-tot-normalized",
                              autoplot(normalize(Ler_leaf_rflt.spct, norm = "max")))
  vdiffr::expect_doppelganger("reflector-default-tot-normalized-759",
                              autoplot(normalize(Ler_leaf_rflt.spct, norm = 759.1)))

  vdiffr::expect_doppelganger("reflector-default-tot",
                              autoplot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("reflector-default-tot-ylim",
                              autoplot(Ler_leaf_rflt.spct, ylim = c(-0.1, 0.65)))
  vdiffr::expect_doppelganger("reflector-default-pc-out-tot",
                              autoplot(Ler_leaf_rflt.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("reflector-text-size-tot",
                              autoplot(Ler_leaf_rflt.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("reflector-span-31-tot",
                              autoplot(Ler_leaf_rflt.spct, span = 31))
  vdiffr::expect_doppelganger("reflector-wb-vis-tot",
                              autoplot(Ler_leaf_rflt.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("reflector-label-average-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("reflector-label-mean-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("reflector-range-num-shrink",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-range-num-shrink-long",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("reflector-range-num-shrink-r",
                              autoplot(Ler_leaf_rflt.spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("reflector-range-num-shrink-l",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("reflector-range-num-expand",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, 1000)))
  vdiffr::expect_doppelganger("reflector-range-num-expand-long",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, 600, 1000)))
  vdiffr::expect_doppelganger("reflector-range-num-expand-r",
                              autoplot(Ler_leaf_rflt.spct, range = c(NA, 1000)))
  vdiffr::expect_doppelganger("reflector-range-num-expand-l",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, NA)))
  vdiffr::expect_doppelganger("reflector-range-wb-tot",
                              autoplot(Ler_leaf_rflt.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("reflector-no-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = ""))
  vdiffr::expect_doppelganger("reflector-minus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("reflector-plus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("+", "boundaries")))
})

test_that("reflector_mspct", {
  set_annotations_default()
  two_leaves_Rfr.mspct <- reflector_mspct(list(one = Ler_leaf_rflt.spct,
                                        half = Ler_leaf_rflt.spct / 2))
  vdiffr::expect_doppelganger("reflector-mspct-default",
                              autoplot(two_leaves_Rfr.mspct))
  vdiffr::expect_doppelganger("reflector-mspct-default-range",
                              autoplot(two_leaves_Rfr.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-mspct-no-annotations",
                              autoplot(two_leaves_Rfr.mspct, annotations = ""))
  vdiffr::expect_doppelganger("reflector-mspct-reserve-space",
                              autoplot(two_leaves_Rfr.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("reflector-mspct-mean",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("reflector-mspct-median",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "median"))
  # triggers warning
  # vdiffr::expect_doppelganger("reflector-mspct-sum",
  #                             autoplot(two_leaves_Rfr.mspct, plot.data = "sum"))
  testthat::expect_warning(autoplot(two_leaves_Rfr.mspct, plot.data = "sum"))
  vdiffr::expect_doppelganger("reflector-mspct-sum",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("reflector-mspct-se",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("reflector-mspct-se-ylab",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("reflector-mspct-var",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("reflector-mspct-var-ylab",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("reflector-mspct-mean-Tfr",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("reflector-mspct-median-Tfr",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "median"))
})

test_that("object_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("object-default-tot",
                              autoplot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("object-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("object-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = ""))
  vdiffr::expect_doppelganger("object-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("object-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide")))
  vdiffr::expect_doppelganger("object-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("object-default-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("object-default-stk-ylim",
                              autoplot(Ler_leaf.spct, stacked = FALSE, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("object-default-pc-out-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, text.size = 3.5))
  vdiffr::expect_doppelganger("object-minus-peaks-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("-", "peaks")))
  vdiffr::expect_doppelganger("object-peaks-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("+", "peaks", "valleys")))
  vdiffr::expect_doppelganger("object-plus-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("+", "valleys")))
  vdiffr::expect_doppelganger("object-wb-vis-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = ""))
  vdiffr::expect_doppelganger("object-minus-labels-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("-", "labels")))
  vdiffr::expect_doppelganger("object-plus-segments-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("+", "segments")))
})
