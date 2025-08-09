context("autoplot-cps")
library(ggplot2)
library(photobiology)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
white_led.cps_spct <- interpolate_spct(photobiology::white_led.cps_spct,
                                       length.out = length.out.spct)

test_that("cps_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("cps-default",
                              autoplot(white_led.cps_spct))
  vdiffr::expect_doppelganger("cps-ylim1",
                              autoplot(white_led.cps_spct, ylim = c(NA, 5e5)))
  vdiffr::expect_doppelganger("cps-ylim2",
                              autoplot(white_led.cps_spct, ylim = c(-1e5, 5e5)))
  vdiffr::expect_doppelganger("cps-range-num-shrink",
                              autoplot(white_led.cps_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("cps-range-num-shrink-long",
                              autoplot(white_led.cps_spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("cps-range-num-shrink-r",
                              autoplot(white_led.cps_spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("cps-range-num-shrink-l",
                              autoplot(white_led.cps_spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("cps-range-num-expand",
                              autoplot(white_led.cps_spct, range = c(100, 1300)))
  vdiffr::expect_doppelganger("cps-range-wb",
                              autoplot(white_led.cps_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("cps-no-annotations",
                              autoplot(white_led.cps_spct, annotations = ""))
  vdiffr::expect_doppelganger("cps-reserve-space",
                              autoplot(white_led.cps_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("cps-minus-annotations",
                              autoplot(white_led.cps_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("cps-plus-annotations",
                              autoplot(white_led.cps_spct, annotations = c("+", "boundaries")))
})

