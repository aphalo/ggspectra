context("set-unset-options")
library(photobiology)

test_that("set_annotations_default", {
  set_annotations_default()
  expect_null(getOption("photobiology.plot.annotations"))
  set_annotations_default("")
  expect_setequal(getOption("photobiology.plot.annotations"), "")
  set_annotations_default("reserve.space")
  expect_setequal(getOption("photobiology.plot.annotations"), "reserve.space")
  set_annotations_default()
  set_annotations_default(c("+", "title:what:where"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default("+")
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default("-")
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default(c("-", "title"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default(c("-", "title*"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("-", "peaks*"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "peak.labels"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peak.labels"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "segments"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "boxes"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("=", "segments", "summaries"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "summaries"))
  set_annotations_default(c("=", ""))
  expect_setequal(getOption("photobiology.plot.annotations"), "")
  set_annotations_default(c("segments", "summaries"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "summaries"))
  set_annotations_default(c("color.guide", "colour.guide"))
  expect_setequal(getOption("photobiology.plot.annotations"), "colour.guide")
  set_annotations_default() # clean up
})

test_that("set_w.band_default", {
  set_w.band_default()
  expect_null(getOption("photobiology.plot.bands"))
  set_w.band_default(UVB())
  expect_setequal(getOption("photobiology.plot.bands"), list(UVB()))
  set_w.band_default(UV_bands())
  expect_setequal(getOption("photobiology.plot.bands"), UV_bands())
  expect_warning(set_w.band_default("UVB"))
  expect_warning(set_w.band_default(c(400,500)))
  expect_warning(set_w.band_default(list(a = 400, b = "a")))
  set_w.band_default() # clean up
})
