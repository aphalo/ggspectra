context("stats-find")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

test_that("stat_find_wls", {
  vdiffr::expect_doppelganger("stat-find-wls",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls())

  vdiffr::expect_doppelganger("stat-find-wls-HM",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = "half.maximum"))

  vdiffr::expect_doppelganger("stat-find-wls-HR",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = "half.range"))

  vdiffr::expect_doppelganger("stat-find-wls-zero",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = 0))

  vdiffr::expect_doppelganger("stat-find-wls-100",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = 100))

  vdiffr::expect_doppelganger("stat-find-wls-vector",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10))

  vdiffr::expect_doppelganger("stat-find-wls-vector-long",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = (-1:9) / 10))

  vdiffr::expect_doppelganger("stat-find-qtys",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys())

  vdiffr::expect_doppelganger("stat-find-qtys-HR",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = "half.range"))

  vdiffr::expect_doppelganger("stat-find-qtys-vector",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-long",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = (1:10) * 100))

})


test_that("stat_find_wls-group", {

  white_led.source_mspct <-
    source_mspct(list(one = white_led.source_spct,
                 half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("stat-find-wls-group",
                              ggplot(white_led.source_mspct) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-wls-vector-group",
                              ggplot(white_led.source_mspct)+
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10,
                                              aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-group",
                              ggplot(white_led.source_mspct)+
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100,
                                               aes(colour = spct.idx)))

  })

test_that("stat_find_wls-panel", {

  white_led.source_mspct <-
    source_mspct(list(one = white_led.source_spct,
                      half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("stat-find-wls-panel",
                              ggplot(white_led.source_mspct) +
                                facet_wrap(~spct.idx) +
                                geom_line() +
                                stat_find_wls(aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-wls-vector-panel",
                              ggplot(white_led.source_mspct)+
                                facet_wrap(~spct.idx) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10,
                                              aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-panel",
                              ggplot(white_led.source_mspct)+
                                facet_wrap(~spct.idx) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100,
                                               aes(colour = spct.idx)))
})

