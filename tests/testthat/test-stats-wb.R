context("stats-wb")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 200L
white_led.raw_spct <- interpolate_spct(photobiology::white_led.raw_spct, length.out = length.out.spct)
white_led.cps_spct <- interpolate_spct(photobiology::white_led.cps_spct, length.out = length.out.spct)
white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)

test_that("stat_wb_label", {
  vdiffr::expect_doppelganger("stat-wb_label-vis",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_label(aes(color = after_stat(wb.color)),
                                              w.band = Red()) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_label-vis-bands-source",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_label(aes(color = after_stat(wb.color)),
                                              w.band = VIS_bands(),
                                              ypos.fixed = 0.7,
                                              angle = 45) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_label-label-source",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_label(geom = "label",
                                              w.band = VIS_bands(),
                                              ypos.fixed = 0.7,
                                              angle = 45) +
                                scale_fill_identity() +
                                scale_colour_identity() +
                                expand_limits(y = 0.8))

  })

test_that("stat_wb_e_irrad", {
  vdiffr::expect_doppelganger("stat-wb_e_irrad-vis",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_e_irrad(aes(color = after_stat(wb.color)),
                                                w.band = Red()) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_e_irrad-vis-bands-source",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_e_irrad(aes(color = after_stat(wb.color)),
                                                w.band = VIS_bands(),
                                                ypos.fixed = 0.7,
                                                angle = 45) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_e_irrad-label-source",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wb_e_irrad(geom = "label",
                                                w.band = VIS_bands(),
                                                ypos.fixed = 0.7,
                                                angle = 45) +
                                scale_fill_identity() +
                                scale_colour_identity() +
                                expand_limits(y = 0.8))

})

test_that("stat_wb_q_irrad", {
  vdiffr::expect_doppelganger("stat-wb_q_irrad-vis",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_q_irrad(aes(color = after_stat(wb.color)),
                                                w.band = Red(),
                                                label.fmt = "%.3e") +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_q_irrad-vis-bands-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_q_irrad(aes(color = after_stat(wb.color)),
                                                w.band = VIS_bands(),
                                                label.fmt = "%.3e",
                                                angle = 45) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_q_irrad-label-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_q_irrad(geom = "label",
                                                w.band = VIS_bands(),
                                                angle = 45) +
                                scale_fill_identity() +
                                scale_colour_identity())
})

test_that("stat_wb_box", {
  vdiffr::expect_doppelganger("stat-wb_box-vis",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_box(aes(color = after_stat(wb.color)),
                                                w.band = VIS()) +
                                scale_fill_identity() +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_box-vis-bands-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_box(aes(color = after_stat(wb.color)),
                                            w.band = VIS_bands()) +
                                scale_fill_identity() +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_box-label-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                stat_wb_box(w.band = VIS_bands(),
                                            ypos.fixed = 2e-6,
                                            box.height = 1.3,
                                            alpha = 0.5) +
                                geom_line() +
                                scale_fill_identity() +
                                scale_colour_identity())
})

test_that("stat_wb_hbar", {
  vdiffr::expect_doppelganger("stat-wb_hbar-vis",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_hbar(w.band = VIS()) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_hbar-vis-bands-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_hbar(w.band = VIS_bands()) +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_hbar-linewidth-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                stat_wb_hbar(w.band = VIS_bands(),
                                             ypos.fixed = -1e-7,
                                             linewidth = 1) +
                                geom_line() +
                                scale_fill_identity() +
                                scale_colour_identity())
})

test_that("stat_wb_column", {
  vdiffr::expect_doppelganger("stat-wb_column-vis",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_column(w.band = VIS(),
                                               alpha = 1/3) +
                                scale_fill_identity())

  vdiffr::expect_doppelganger("stat-wb_column-vis-bands-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                geom_line() +
                                stat_wb_column(w.band = VIS_bands(),
                                               alpha = 1/3) +
                                scale_fill_identity())

  vdiffr::expect_doppelganger("stat-wb_column-colour-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                stat_wb_column(aes(colour = after_stat(wb.color)),
                                               w.band = VIS_bands(),
                                               alpha = 1/3) +
                                geom_line() +
                                scale_fill_identity() +
                                scale_colour_identity())

  vdiffr::expect_doppelganger("stat-wb_column-linetype-source",
                              ggplot(white_led.source_spct, unit.out = "photon")+
                                stat_wb_column(w.band = VIS_bands(),
                                               alpha = 1/3,
                                               linewidth = 0.25,
                                               colour = "black",
                                               linetype = "dashed") +
                                geom_line() +
                                scale_fill_identity() +
                                scale_colour_identity())
})
