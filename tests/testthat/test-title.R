context("autotitle")
library(photobiology)

test_that("title", {
  vdiffr::expect_doppelganger("cps-title",
                              autoplot(white_led.cps_spct, annotations = c("+", "title")))
  vdiffr::expect_doppelganger("cps-title-objt",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:objt")))
  vdiffr::expect_doppelganger("cps-title-class",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:class")))
  vdiffr::expect_doppelganger("cps-title-what",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:what")))
  vdiffr::expect_doppelganger("cps-title-when",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:when")))
  vdiffr::expect_doppelganger("cps-title-where",
                              autoplot(sun.spct, annotations = c("+", "title:where")))
  vdiffr::expect_doppelganger("cps-title-inst.name",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:inst.name")))
  vdiffr::expect_doppelganger("cps-title-inst.sn",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:inst.sn")))
  vdiffr::expect_doppelganger("cps-title-none",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none")))
  vdiffr::expect_doppelganger("cps-title-what-when-class",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:what:when:class")))
  vdiffr::expect_doppelganger("cps-title-none-when-what",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:when:what")))
  vdiffr::expect_doppelganger("cps-title-what-none-when",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:what:none:when")))
  vdiffr::expect_doppelganger("cps-title-none-none-what",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:none:what")))
  vdiffr::expect_doppelganger("cps-title-what-none-none",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:what:none:none")))
  testthat::expect_warning(plot(white_led.cps_spct, annotations = c("+", "title:zzz")))
})

test_that("subtitle", {
  vdiffr::expect_doppelganger("cps-title-none-objt",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:objt")))
  vdiffr::expect_doppelganger("cps-title-none-class",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:class")))
  vdiffr::expect_doppelganger("cps-title-none-what",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:what")))
  vdiffr::expect_doppelganger("cps-title-none-when",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:when")))
  vdiffr::expect_doppelganger("cps-title-none-where",
                              autoplot(sun.spct, annotations = c("+", "title:none:where")))
  vdiffr::expect_doppelganger("cps-title-none-inst.name",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:inst.name")))
  vdiffr::expect_doppelganger("cps-title-none-inst.sn",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:inst.sn")))
  vdiffr::expect_doppelganger("cps-title-none-none",
                              autoplot(white_led.cps_spct, annotations = c("+", "title:none:none")))
  testthat::expect_warning(plot(white_led.cps_spct, annotations = c("+", "title:none:zzz")))
})

test_that("autotitle", {
  vdiffr::expect_doppelganger("cps-autotitle-default",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct))
  vdiffr::expect_doppelganger("cps-autotitle-title",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = "title"))
  vdiffr::expect_doppelganger("cps-autotitle-object",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = "title:objt"))
  vdiffr::expect_doppelganger("cps-autotitle-object-class",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = "title:objt:class"))
  vdiffr::expect_doppelganger("cps-autotitle-no-ann1",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = ""))
  vdiffr::expect_doppelganger("cps-autotitle-no-ann2",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = NULL))
  vdiffr::expect_doppelganger("cps-autotitle-no-ann3",
                              ggplot(white_led.cps_spct) +
                                geom_line() +
                                autotitle(white_led.cps_spct, annotations = character()))
  testthat::expect_warning(ggplot(white_led.cps_spct) +
                             geom_line() +
                             autotitle(white_led.cps_spct, annotations = "title:zzz"))
})
