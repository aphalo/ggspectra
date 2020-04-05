context("stats-spikes")
library(ggplot2)
library(photobiology)

test_that("stat_spikes", {
  my_test.spct <- clip_wl(white_led.raw_spct, range = c(260, 900))[ , c(1, 4)]
  names(my_test.spct)[2] <- "counts"
  my_test_despiked.spct <- despike(my_test.spct,
                                   max.spike.width = 5,
                                   z.threshold = 4)

  vdiffr::expect_doppelganger("stat-spikes-sun",
                              ggplot(sun.spct) +
                                geom_line() +
                                stat_spikes())

  vdiffr::expect_doppelganger("stat-spikes-sun-width",
                              ggplot(sun.spct) +
                                geom_line() +
                                stat_spikes(max.spike.width = 1))

  vdiffr::expect_doppelganger("stat-spikes-led",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes())

  vdiffr::expect_doppelganger("stat-spikes-none-led",
                              ggplot(my_test_despiked.spct) +
                                geom_line() +
                                stat_spikes())

  vdiffr::expect_doppelganger("stat-spikes",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 4))

  vdiffr::expect_doppelganger("stat-spikes-width",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 4, max.spike.width = 2))

  vdiffr::expect_doppelganger("stat-spikes-width-colour",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(mapping = aes(color = stat(wl.color)),
                                            z.threshold = 4,
                                            max.spike.width = 2) +
                                scale_color_identity())

  vdiffr::expect_doppelganger("stat-spikes-text",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 50,
                                            max.spike.width = 1,
                                            geom = "text"))

  vdiffr::expect_doppelganger("stat-spikes-text-overlap",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 4,
                                            geom = "text",
                                            check_overlap = TRUE))

  vdiffr::expect_doppelganger("stat-spikes-label",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(mapping = aes(color = stat(wl.color),
                                                          fill = stat(BW.color)),
                                            z.threshold = 4,
                                            max.spike.width = 2,
                                            geom = "label") +
                                scale_color_identity() +
                                scale_fill_identity())
})

