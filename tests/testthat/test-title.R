context("ggtitle_spct")
library(photobiology)

test_that("title", {
  vdiffr::expect_doppelganger("cps-title",
                              plot(white_led.cps_spct, annotations = c("+", "title")))
  vdiffr::expect_doppelganger("cps-title-objt",
                              plot(white_led.cps_spct, annotations = c("+", "title:objt")))
  vdiffr::expect_doppelganger("cps-title-class",
                              plot(white_led.cps_spct, annotations = c("+", "title:class")))
  vdiffr::expect_doppelganger("cps-title-what",
                              plot(white_led.cps_spct, annotations = c("+", "title:what")))
  vdiffr::expect_doppelganger("cps-title-when",
                              plot(white_led.cps_spct, annotations = c("+", "title:when")))
  vdiffr::expect_doppelganger("cps-title-where",
                              plot(sun.spct, annotations = c("+", "title:where")))
  vdiffr::expect_doppelganger("cps-title-inst.name",
                              plot(white_led.cps_spct, annotations = c("+", "title:inst.name")))
  vdiffr::expect_doppelganger("cps-title-inst.sn",
                              plot(white_led.cps_spct, annotations = c("+", "title:inst.sn")))
  vdiffr::expect_doppelganger("cps-title-none",
                              plot(white_led.cps_spct, annotations = c("+", "title:none")))
  testthat::expect_warning(plot(white_led.cps_spct, annotations = c("+", "title:zzz")))
})

test_that("subtitle", {
  vdiffr::expect_doppelganger("cps-title-none-objt",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:objt")))
  vdiffr::expect_doppelganger("cps-title-none-class",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:class")))
  vdiffr::expect_doppelganger("cps-title-none-what",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:what")))
  vdiffr::expect_doppelganger("cps-title-none-when",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:when")))
  vdiffr::expect_doppelganger("cps-title-none-where",
                              plot(sun.spct, annotations = c("+", "title:none:where")))
  vdiffr::expect_doppelganger("cps-title-none-inst.name",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:inst.name")))
  vdiffr::expect_doppelganger("cps-title-none-inst.sn",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:inst.sn")))
  vdiffr::expect_doppelganger("cps-title-none-none",
                              plot(white_led.cps_spct, annotations = c("+", "title:none:none")))
  testthat::expect_warning(plot(white_led.cps_spct, annotations = c("+", "title:none:zzz")))
})

test_that("ggtitle_spct", {
  vdiffr::expect_doppelganger("cps-ggtitle-default",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct))
  vdiffr::expect_doppelganger("cps-ggtitle-title",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = "title"))
  vdiffr::expect_doppelganger("cps-ggtitle-object",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = "title:objt"))
  vdiffr::expect_doppelganger("cps-ggtitle-object-class",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = "title:objt:class"))
  vdiffr::expect_doppelganger("cps-ggtitle-no-ann1",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = ""))
  vdiffr::expect_doppelganger("cps-ggtitle-no-ann2",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = NULL))
  vdiffr::expect_doppelganger("cps-ggtitle-no-ann3",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                ggtitle_spct(white_led.cps_spct, annotations = character()))
  testthat::expect_warning(ggplot(white_led.cps_spct) +
                             geom_line() +
                             ggtitle_spct(white_led.cps_spct, annotations = "title:zzz"))
})
