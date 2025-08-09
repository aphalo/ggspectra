context("autoplot-raw-cps")
library(ggplot2)
library(photobiology)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
white_led.raw_spct <- interpolate_spct(photobiology::white_led.raw_spct,
                                       length.out = length.out.spct)

test_that("raw_spct", {
  set_annotations_default()
    # skip_on_cran()
  vdiffr::expect_doppelganger("raw-default",
                      autoplot(white_led.raw_spct))
  vdiffr::expect_doppelganger("raw-ylim1",
                              autoplot(white_led.raw_spct, ylim = c(NA, 9e4)))
  vdiffr::expect_doppelganger("raw-ylim2",
                              autoplot(white_led.raw_spct, ylim = c(-5e3, 9e4)))
  vdiffr::expect_doppelganger("raw-range-num-shrink",
                              autoplot(white_led.raw_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("raw-range-num-shrink-long",
                              autoplot(white_led.raw_spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("raw-range-num-shrink-r",
                              autoplot(white_led.raw_spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("raw-range-num-shrink-l",
                              autoplot(white_led.raw_spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("raw-range-num-expand",
                              autoplot(white_led.raw_spct, range = c(100, 1300)))
  vdiffr::expect_doppelganger("raw-range-num-expand-long",
                              autoplot(white_led.raw_spct, range = c(100, 600, 1300)))
  vdiffr::expect_doppelganger("raw-range-num-expand-r",
                              autoplot(white_led.raw_spct, range = c(NA, 1300)))
  vdiffr::expect_doppelganger("raw-range-num-expand-l",
                              autoplot(white_led.raw_spct, range = c(100, NA)))
  vdiffr::expect_doppelganger("raw-range-wb",
                              autoplot(white_led.raw_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("raw-no-annotations",
                      autoplot(white_led.raw_spct, annotations = ""))
  vdiffr::expect_doppelganger("raw-reserve-space",
                              autoplot(white_led.raw_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("raw-minus-annotations",
                      autoplot(white_led.raw_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("raw-plus-annotations",
                      autoplot(white_led.raw_spct, annotations = c("+", "boundaries")))
})

