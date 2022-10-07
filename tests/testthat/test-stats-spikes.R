context("stats-spikes")
library(ggplot2)
library(photobiology)

test_that("stat_spikes", {
  my_test.spct <- clip_wl(photobiology::white_led.raw_spct,
                          range = c(200, 470))[ , c(1, 3)]
  names(my_test.spct)[2] <- "counts"
  my_test_despiked.spct <- despike(my_test.spct,
                                   max.spike.width = 7,
                                   z.threshold = 9)

  vdiffr::expect_doppelganger("stat-spikes-led-default",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes())

  vdiffr::expect_doppelganger("stat-spikes-led-width",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(max.spike.width = 7))

  vdiffr::expect_doppelganger("stat-spikes-led-wd-thr",
                              ggplot(my_test_despiked.spct) +
                                geom_line() +
                                stat_spikes(max.spike.width = 7,
                                            z.threshold = 9))

  vdiffr::expect_doppelganger("stat-spikes-led-despike",
                              ggplot(clip_wl(my_test_despiked.spct, range = c(NA, 430))) +
                                geom_line() +
                                stat_spikes(max.spike.width = 7,
                                            z.threshold = 9))

  vdiffr::expect_doppelganger("stat-spikes-led-thr",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 4))

  vdiffr::expect_doppelganger("stat-spikes-width-thr-a",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(z.threshold = 4,
                                            max.spike.width = 2))

  vdiffr::expect_doppelganger("stat-spikes-width-colour",
                              ggplot(my_test.spct) +
                                geom_line() +
                                stat_spikes(mapping = aes(color = after_stat(wl.color)),
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
                                stat_spikes(mapping = aes(color = after_stat(wl.color),
                                                          fill = after_stat(BW.color)),
                                            z.threshold = 4,
                                            max.spike.width = 2,
                                            geom = "label") +
                                scale_color_identity() +
                                scale_fill_identity())
})

