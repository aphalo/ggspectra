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

  vdiffr::expect_doppelganger("flt-default-tot-scaled",
                              autoplot(fscale(Ler_leaf_trns.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("flt-default-tot-normalized",
                              autoplot(normalize(Ler_leaf_trns.spct, norm = "max")))

  vdiffr::expect_doppelganger("flt-default-tot",
                              autoplot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("flt-default-tot2a",
                              autoplot(Ler_leaf_trns.spct, plot.qty = "absorbance"))
  # vdiffr::expect_doppelganger("flt-default-tot2afr",
  #                             autoplot(Ler_leaf_trns.spct, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("flt-default-tot-ylim",
                              autoplot(Ler_leaf_trns.spct, ylim = c(-0.05, 0.5)))
  vdiffr::expect_doppelganger("flt-default-pc-out-tot",
                              autoplot(Ler_leaf_trns.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("flt-text-size-tot",
                              autoplot(Ler_leaf_trns.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("flt-span-31-tot",
                              autoplot(Ler_leaf_trns.spct, span = 31))
  vdiffr::expect_doppelganger("flt-wb-vis-tot",
                              autoplot(Ler_leaf_trns.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("flt-label-average-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("flt-label-mean-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("flt-range-num-shrink",
                              autoplot(Ler_leaf_trns.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("flt-range-num-shrink-long",
                              autoplot(Ler_leaf_trns.spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("flt-range-num-shrink-r",
                              autoplot(Ler_leaf_trns.spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("flt-range-num-shrink-l",
                              autoplot(Ler_leaf_trns.spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("flt-range-num-expand",
                              autoplot(Ler_leaf_trns.spct, range = c(150, 1000)))
  vdiffr::expect_doppelganger("flt-range-num-expand-long",
                              autoplot(Ler_leaf_trns.spct, range = c(150, 600, 1000)))
  vdiffr::expect_doppelganger("flt-range-num-expand-r",
                              autoplot(Ler_leaf_trns.spct, range = c(NA, 1000)))
  vdiffr::expect_doppelganger("flt-range-num-expand-l",
                              autoplot(Ler_leaf_trns.spct, range = c(150, NA)))
  vdiffr::expect_doppelganger("flt-range-wb-tot",
                              autoplot(Ler_leaf_trns.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("flt-no-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = ""))
  vdiffr::expect_doppelganger("flt-minus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("flt-plus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("flt-default-scaled",
                              autoplot(fscale(Ler_leaf_trns_i.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("flt-default-normalized",
                              autoplot(normalize(Ler_leaf_trns_i.spct, norm = "max")))

  vdiffr::expect_doppelganger("flt-default",
                              autoplot(Ler_leaf_trns_i.spct))
  vdiffr::expect_doppelganger("flt-default-ylim",
                              autoplot(Ler_leaf_trns_i.spct, ylim = c(NA, 1.2)))
  vdiffr::expect_doppelganger("flt-default-pc-out",
                              autoplot(Ler_leaf_trns_i.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("flt-text-size",
                              autoplot(Ler_leaf_trns_i.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("flt-span-31",
                              autoplot(Ler_leaf_trns_i.spct, span = 31))
  vdiffr::expect_doppelganger("flt-wb-vis",
                              autoplot(Ler_leaf_trns_i.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("flt-label-average",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("flt-label-mean",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("flt-range-num",
                              autoplot(Ler_leaf_trns_i.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("flt-range-wb",
                              autoplot(Ler_leaf_trns_i.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("flt-no-annotations",
                              autoplot(Ler_leaf_trns_i.spct, annotations = ""))
  vdiffr::expect_doppelganger("flt-reserve-space",
                              autoplot(Ler_leaf_trns_i.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("flt-minus-summaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("flt-minus-boxes",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("flt-plus-boundaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("flt-plus-segments",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "segments")))
})

test_that("filter_spct absorptance", {
  set_annotations_default()
  Afr_as_default()

  Ler_leaf_Afr_i_spct <- T2Afr(Ler_leaf_trns_i.spct)

  vdiffr::expect_doppelganger("flt-default-apt",
                              autoplot(Ler_leaf_Afr_i_spct))
  vdiffr::expect_doppelganger("flt-default-apt2t",
                              autoplot(Ler_leaf_Afr_i_spct,
                                       plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("flt-default-apt2a",
                              autoplot(Ler_leaf_Afr_i_spct,
                                       plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("flt-default-apt-ylim",
                              autoplot(Ler_leaf_Afr_i_spct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("flt-default-pc-out-apt",
                              autoplot(Ler_leaf_Afr_i_spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("flt-text-size-apt",
                              autoplot(Ler_leaf_Afr_i_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("flt-span-31-apt",
                              autoplot(Ler_leaf_Afr_i_spct, span = 31))
  vdiffr::expect_doppelganger("flt-wb-vis-apt",
                              autoplot(Ler_leaf_Afr_i_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("flt-label-average-apt",
                              autoplot(Ler_leaf_Afr_i_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("flt-label-mean-apt",
                              autoplot(Ler_leaf_Afr_i_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("flt-range-num-apt",
                              autoplot(Ler_leaf_Afr_i_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("flt-range-wb-apt",
                              autoplot(Ler_leaf_Afr_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("flt-no-annotations-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = ""))
  vdiffr::expect_doppelganger("flt-minus-summaries-q",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("flt-minus-boxes-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("flt-plus-boundaries-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("flt-plus-segments-apt",
                              autoplot(Ler_leaf_Afr_i_spct, annotations = c("+", "segments")))
})

test_that("filter_spct A", {
  set_annotations_default()
  A_as_default()

  Ler_leaf_A_i_spct <- T2A(Ler_leaf_trns_i.spct, action = "replace")

  vdiffr::expect_doppelganger("flt-default-a2t",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA),
                                       plot.qty = "transmittance"))
  # vdiffr::expect_doppelganger("flt-default-a2afr",
  #                             autoplot(Ler_leaf_A_i_spct, range = c(300, NA),
  #                                      plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("flt-default-a-scaled",
                              autoplot(fscale(Ler_leaf_A_i_spct, target = 2,
                                              range = c(400,700), set.scaled = TRUE),
                                       plot.qty = "absorbance", range = c(300, NA)))
  vdiffr::expect_doppelganger("flt-default-a-normalized",
                               autoplot(normalize(Ler_leaf_A_i_spct, norm = "max"),
                                         range = c(300, NA)))

  vdiffr::expect_doppelganger("flt-default-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA)))
  vdiffr::expect_doppelganger("flt-default-a-ylim",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), ylim = c(-0.2, 4)))
  vdiffr::expect_doppelganger("flt-text-size-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), text.size = 3.5))
  vdiffr::expect_doppelganger("flt-span-31-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), span = 11))
  vdiffr::expect_doppelganger("flt-wb-vis-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), w.band = VIS_bands()))
  vdiffr::expect_doppelganger("flt-label-average-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), label.qty = "average"))
  vdiffr::expect_doppelganger("flt-label-mean-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), label.qty = "mean"))
  vdiffr::expect_doppelganger("flt-range-wb-a",
                              autoplot(Ler_leaf_A_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("flt-no-annotations-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = ""))
  vdiffr::expect_doppelganger("flt-minus-summaries-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("flt-minus-boxes-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("flt-plus-boundaries-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("flt-plus-segments-a",
                              autoplot(Ler_leaf_A_i_spct, range = c(300, NA), annotations = c("+", "segments")))
})

test_that("filter_spct Tfr", {
  set_annotations_default()
  Tfr_as_default()

  Ler_leaf_t_i_spct <- any2T(Ler_leaf_trns_i.spct, action = "replace")

  vdiffr::expect_doppelganger("flt-default-tfr",
                              autoplot(Ler_leaf_t_i_spct))
  vdiffr::expect_doppelganger("flt-default-tfr-ylim",
                              autoplot(Ler_leaf_t_i_spct, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("flt-default-pc-out-tfr",
                              autoplot(Ler_leaf_t_i_spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("flt-text-size-tfr",
                              autoplot(Ler_leaf_t_i_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("flt-span-31-tfr",
                              autoplot(Ler_leaf_t_i_spct, span = 31))
  vdiffr::expect_doppelganger("flt-wb-vis-tfr",
                              autoplot(Ler_leaf_t_i_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("flt-label-average-tfr",
                              autoplot(Ler_leaf_t_i_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("flt-label-mean-tfr",
                              autoplot(Ler_leaf_t_i_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("flt-range-num-tfr",
                              autoplot(Ler_leaf_t_i_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("flt-range-wb-tfr",
                              autoplot(Ler_leaf_t_i_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("flt-no-annotations-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = ""))
  vdiffr::expect_doppelganger("flt-minus-summaries-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("flt-minus-boxes-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("flt-plus-boundaries-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("flt-plus-segments-tfr",
                              autoplot(Ler_leaf_t_i_spct, annotations = c("+", "segments")))
})

test_that("filter_mspct", {
  unset_filter_qty_default()
  set_annotations_default()

  two_leaves.mspct <- filter_mspct(list(one = Ler_leaf_trns_i.spct,
                                      half = Ler_leaf_trns_i.spct / 2))
  vdiffr::expect_doppelganger("flt-mspct-default",
                              autoplot(two_leaves.mspct))
  vdiffr::expect_doppelganger("flt-mspct-default-ylim",
                              autoplot(two_leaves.mspct, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("flt-mspct-default-range-shrink",
                              autoplot(two_leaves.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("flt-mspct-default-range-expand",
                              autoplot(two_leaves.mspct, range = c(200, 1100)))
  # use range to avoid Tfr == 0 values.
  vdiffr::expect_doppelganger("flt-mspct-default-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("flt-mspct-default-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("flt-mspct-default-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"),
                                   range = c(400,750)))
  vdiffr::expect_doppelganger("flt-mspct-no-annotations",
                              autoplot(two_leaves.mspct, annotations = ""))
  vdiffr::expect_doppelganger("flt-mspct-reserve-space",
                              autoplot(two_leaves.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("flt-mspct-mean",
                              autoplot(two_leaves.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("flt-mspct-median",
                              autoplot(two_leaves.mspct, plot.data = "median"))
  # triggers warning
  # vdiffr::expect_doppelganger("flt-mspct-sum",
  #                             autoplot(two_leaves.mspct, plot.data = "sum"))
  testthat::expect_warning(autoplot(two_leaves.mspct, plot.data = "sum"))
  vdiffr::expect_doppelganger("flt-mspct-sum",
                               autoplot(two_leaves.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("flt-mspct-se",
                              autoplot(two_leaves.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("flt-mspct-se-ylab",
                              autoplot(two_leaves.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("flt-mspct-var",
                              autoplot(two_leaves.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("flt-mspct-var-ylab",
                              autoplot(two_leaves.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("flt-mspct-mean-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("flt-mspct-median-Tfr",
                              autoplot(any2T(two_leaves.mspct, action = "replace"), plot.data = "median"))
  # Bug in photobiology::rowwise_filter() (fixed in photobiology >= 0.10.11) affected next two staements
  vdiffr::expect_doppelganger("flt-mspct-mean-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("flt-mspct-median-Afr",
                              autoplot(T2Afr(two_leaves.mspct, action = "replace"), plot.data = "median"))
  vdiffr::expect_doppelganger("flt-mspct-mean-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"), plot.data = "mean"))
  vdiffr::expect_doppelganger("flt-mspct-median-A",
                              autoplot(T2A(two_leaves.mspct, action = "replace"), plot.data = "median"))
})

test_that("reflector_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("rfl-default-tot-scaled",
                              autoplot(fscale(Ler_leaf_rflt.spct, target = 0.06,
                                              range = c(400,700), set.scaled = TRUE)))
  vdiffr::expect_doppelganger("rfl-default-tot-normalized",
                              autoplot(normalize(Ler_leaf_rflt.spct, norm = "max")))
  vdiffr::expect_doppelganger("rfl-default-tot-normalized-759",
                              autoplot(normalize(Ler_leaf_rflt.spct, norm = 759.1)))

  vdiffr::expect_doppelganger("rfl-default-tot",
                              autoplot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("rfl-default-tot-ylim",
                              autoplot(Ler_leaf_rflt.spct, ylim = c(-0.1, 0.65)))
  vdiffr::expect_doppelganger("rfl-default-pc-out-tot",
                              autoplot(Ler_leaf_rflt.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("rfl-text-size-tot",
                              autoplot(Ler_leaf_rflt.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("rfl-span-31-tot",
                              autoplot(Ler_leaf_rflt.spct, span = 31))
  vdiffr::expect_doppelganger("rfl-wb-vis-tot",
                              autoplot(Ler_leaf_rflt.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("rfl-label-average-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("rfl-label-mean-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("rfl-range-num-shrink",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("rfl-range-num-shrink-long",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("rfl-range-num-shrink-r",
                              autoplot(Ler_leaf_rflt.spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("rfl-range-num-shrink-l",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("rfl-range-num-expand",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, 1000)))
  vdiffr::expect_doppelganger("rfl-range-num-expand-long",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, 600, 1000)))
  vdiffr::expect_doppelganger("rfl-range-num-expand-r",
                              autoplot(Ler_leaf_rflt.spct, range = c(NA, 1000)))
  vdiffr::expect_doppelganger("rfl-range-num-expand-l",
                              autoplot(Ler_leaf_rflt.spct, range = c(200, NA)))
  vdiffr::expect_doppelganger("rfl-range-wb-tot",
                              autoplot(Ler_leaf_rflt.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("rfl-no-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = ""))
  vdiffr::expect_doppelganger("rfl-minus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("rfl-plus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("+", "boundaries")))
})

test_that("reflector_mspct", {
  set_annotations_default()
  two_leaves_Rfr.mspct <- reflector_mspct(list(one = Ler_leaf_rflt.spct,
                                        half = Ler_leaf_rflt.spct / 2))
  vdiffr::expect_doppelganger("rfl-mspct-default",
                              autoplot(two_leaves_Rfr.mspct))
  vdiffr::expect_doppelganger("rfl-mspct-default-range",
                              autoplot(two_leaves_Rfr.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("rfl-mspct-no-annotations",
                              autoplot(two_leaves_Rfr.mspct, annotations = ""))
  vdiffr::expect_doppelganger("rfl-mspct-reserve-space",
                              autoplot(two_leaves_Rfr.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("rfl-mspct-mean",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("rfl-mspct-median",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "median"))
  # triggers warning
  # vdiffr::expect_doppelganger("rfl-mspct-sum",
  #                             autoplot(two_leaves_Rfr.mspct, plot.data = "sum"))
  testthat::expect_warning(autoplot(two_leaves_Rfr.mspct, plot.data = "sum"))
  vdiffr::expect_doppelganger("rfl-mspct-sum",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("rfl-mspct-se",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("rfl-mspct-se-ylab",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("rfl-mspct-var",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("rfl-mspct-var-ylab",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("rfl-mspct-mean-Tfr",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("rfl-mspct-median-Tfr",
                              autoplot(two_leaves_Rfr.mspct, plot.data = "median"))
})

test_that("object_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("obj-default-tot",
                              autoplot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("obj-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("obj-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("obj-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("obj-range-num-shrink",
                              autoplot(Ler_leaf.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("obj-range-num-expand",
                              autoplot(Ler_leaf.spct, range = c(200, 1100)))
  vdiffr::expect_doppelganger("obj-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("obj-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = ""))
  vdiffr::expect_doppelganger("obj-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("obj-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide")))
  vdiffr::expect_doppelganger("obj-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("obj-default-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("obj-default-stk-ylim",
                              autoplot(Ler_leaf.spct, stacked = FALSE, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("obj-default-pc-out-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, pc.out = TRUE))
  vdiffr::expect_doppelganger("obj-text-size-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, text.size = 3.5))
  vdiffr::expect_doppelganger("obj-minus-peaks-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("-", "peaks")))
  vdiffr::expect_doppelganger("obj-peaks-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("+", "peaks", "valleys")))
  vdiffr::expect_doppelganger("obj-plus-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 401, annotations = c("+", "valleys")))
  vdiffr::expect_doppelganger("obj-wb-vis-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("obj-range-num-stk-shrink",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = c(500, 700)))
  vdiffr::expect_doppelganger("obj-range-num-stk-expand",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = c(200, 1100)))
  vdiffr::expect_doppelganger("obj-range-wb-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("obj-no-annotations-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = ""))
  vdiffr::expect_doppelganger("obj-minus-labels-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("-", "labels")))
  vdiffr::expect_doppelganger("obj-plus-segments-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("+", "segments")))
})
