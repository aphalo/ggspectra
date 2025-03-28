## ----message=FALSE------------------------------------------------------------
library(ggplot2)
library(scales)
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ggrepel)

## ----include=FALSE, echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4,
               cache = FALSE)
options(warnPartialMatchArgs = FALSE)

## -----------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct / 2))

## -----------------------------------------------------------------------------
two_suns.spct <- rbindspct(two_suns.mspct)

## -----------------------------------------------------------------------------
theme_set(theme_bw())

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line()

## -----------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) + geom_line()

## -----------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) + geom_line()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") + geom_line()

## -----------------------------------------------------------------------------
photon_as_default()
ggplot(sun.spct) + geom_line()
ggplot(sun.spct, unit.out = "energy") + geom_line()

## -----------------------------------------------------------------------------
unset_user_defaults()

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct) + geom_line()

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct, plot.qty = "absorbance") + geom_line()

## -----------------------------------------------------------------------------
Afr_as_default()
ggplot(yellow_gel.spct) + geom_line()
unset_user_defaults()

## -----------------------------------------------------------------------------
ggplot(two_suns.mspct) + 
  aes(linetype = spct.idx) +
  wl_guide(ymax = -0.05) +
  geom_line()

## -----------------------------------------------------------------------------
ggplot(two_suns.mspct) + 
  wl_guide(ymax = -0.05) +
  geom_spct() +
  geom_line() +
  facet_wrap(facets = vars(spct.idx), ncol = 1L)

## -----------------------------------------------------------------------------
ggplot(two_suns.mspct) + 
  wl_guide(ymax = -0.05) +
  geom_spct() +
  geom_line() +
  facet_wrap(vars(spct.idx), ncol = 1L, scales = "free_y")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(axis.symbols = FALSE) +
  scale_y_s.e.irrad_continuous(axis.symbols = FALSE)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(unit.exponent = -6) +
  scale_y_s.e.irrad_continuous(unit.exponent = -3)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(unit.exponent = -7)  +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
nearest_SI_exponent(-4)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(unit.exponent = nearest_SI_exponent(-4))  +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous(unit.exponent = 0,
                               labels = SI_tg_format(exponent = -3))

## -----------------------------------------------------------------------------
# current version of scales shows math_format() and trans_format() as superseded!
# y-axis tick labels are still correct in the HTML output
temp.spct <- clean(sun.spct, range.s.data = c(1e-20, Inf), fill = 1e-20)
ggplot(temp.spct) + 
  geom_line(na.rm = TRUE) +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous(unit.exponent = 0,
                               trans = "log10",
                               labels = trans_format("log10", math_format()),
                               limits = c(1e-6, NA))

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(label.text = "Longitud de onda,") +
  scale_y_s.e.irrad_continuous(label.text = "Irradiancia espectral,")

## -----------------------------------------------------------------------------
norm_sun.spct <- normalize(sun.spct)
ggplot(norm_sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous(normalized = getNormalized(norm_sun.spct))

## -----------------------------------------------------------------------------
scaled_sun.spct <- fscale(sun.spct)
ggplot(scaled_sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous(scaled = is_scaled(scaled_sun.spct))

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct, aes(x = wl2frequency(w.length), y = s.e.irrad)) + 
  geom_line() +
  scale_x_frequency_continuous() +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(sec.axis = sec_axis_w_frequency()) +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(sec.axis = sec_axis_energy_eV()) +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous(sec.axis = sec_axis_w_frequency(15)) +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(white_led.raw_spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_counts_continuous()

## -----------------------------------------------------------------------------
ggplot(white_led.raw_spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_counts_tg_continuous()

## -----------------------------------------------------------------------------
ggplot(white_led.cps_spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_cps_continuous(unit.exponent = 3)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_continuous()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon", range = c(293, NA)) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.irrad_log10(unit.exponent = -6)

## -----------------------------------------------------------------------------
ggplot(ccd.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.response_continuous(unit.exponent = 6)

## -----------------------------------------------------------------------------
ggplot(ccd.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_s.e.action_continuous(unit.exponent = 6)

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_Tfr_continuous(Tfr.type = getTfrType(yellow_gel.spct))

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_Tfr_continuous(Tfr.type = getTfrType(yellow_gel.spct),
                         labels = percent)

## -----------------------------------------------------------------------------
gel_internal.spct <- convertTfrType(yellow_gel.spct, Tfr.type = "internal")
ggplot(gel_internal.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_Tfr_continuous(Tfr.type = getTfrType(gel_internal.spct))

## -----------------------------------------------------------------------------
ggplot(gel_internal.spct, plot.qty = "absorbance") + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_A_continuous(Tfr.type = getTfrType(gel_internal.spct))

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct, plot.qty = "absorptance") + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_Afr_continuous()

## -----------------------------------------------------------------------------
ggplot(green_leaf.spct) + 
  geom_line() +
  scale_x_wl_continuous() +
  scale_y_Rfr_continuous(Rfr.type = getRfrType(green_leaf.spct))

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_peaks(color = "red")

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") + geom_line() + stat_peaks(color = "red")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_valleys(color = "blue")

## -----------------------------------------------------------------------------
ggplot(yellow_gel.spct) + geom_line() + stat_find_wls(color = "orange")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, color = "black") + scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 35, shape = 4, color = "red", size = 2) +
  stat_peaks(span = 35, color = "red", geom = "rug", sides = "b")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(geom = "text", 
             span = 35,
             color = "red", 
             vjust = "bottom",
             position = position_nudge(y = 0.01)) 

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, 
             span = 35, 
             size = 2) + 
  stat_label_peaks(geom = "label", 
                   span = 35, 
                   vjust = "bottom", 
                   size = 3,
                   position = position_nudge(y = 0.01)) +
  scale_fill_identity() +
  scale_color_identity() + 
  expand_limits(y = 0.9)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, 
             span = 35, 
             size = 2) + 
  stat_label_peaks(geom = "label", 
                   span = 35, 
                   size = 3,
                   na.rm = TRUE,
                   vjust = "bottom",
                   position = position_nudge(y = 0.01)) +
  scale_fill_identity() +
  scale_color_identity() +
  expand_limits(y = 0.9)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL, geom = "vline", linetype = "dotted", color = "red") +
  stat_peaks(span = NULL, geom = "hline", linetype = "dotted", color = "red")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, span = 35, size = 2) + 
  stat_label_peaks(aes(label = after_stat(y.label)),
                   span = 35, geom = "label", size = 3,
                   position = position_nudge(y = 0.04),
                   label.fmt = "%1.2f") +
  expand_limits(y = 1) +
  scale_fill_identity() + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_valleys(shape = 21, 
               span = 35, 
               size = 2) + 
  stat_label_valleys(geom = "label", 
                     span = 35, 
                     size = 3,
                     na.rm = TRUE,
                     vjust = "top",
                     position = position_nudge(y = -0.01)) +
  scale_fill_identity() +
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, span = 35, size = 2) + 
  stat_label_peaks(segment.colour = "black", 
                   span = 35, geom = "label_repel", size = 3,
                   max.overlaps = Inf,
                   position = position_nudge_repel(y = 0.12),
                   min.segment.length = 0,
                   box.padding = 0.25,
                   force_pull = 0) +
  expand_limits(y = 1) +
  scale_fill_identity() + 
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_valleys(shape = 21, span = 35, size = 2) + 
  stat_label_valleys(segment.colour = "black", 
                     span = 35, geom = "label_repel", size = 3,
                     max.overlaps = Inf,
                     position = position_nudge_repel(y = -0.12),
                     min.segment.length = 0,
                     box.padding = 0.25,
                     force = 0.5,
                     force_pull = 1) +
  scale_fill_identity() + 
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL, color = "red") +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, color = "red", 
             aes(label = paste(after_stat(y.label), "at", after_stat(x.label), "nm"))) +
  expand_limits(y = c(NA, 0.9))

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, geom = "point", colour = "red") +
  stat_valleys(span = 21, geom = "point", colour = "blue") +
  stat_peaks(span = 51, geom = "text", colour = "red", 
             vjust = -0.3, label.fmt = "%3.0f nm") +
  stat_valleys(span = 51, geom = "text", colour = "blue", 
               vjust = 1.2, label.fmt = "%3.0f nm")

## -----------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + ylim(NA, 0.9) +
  stat_peaks(span = NULL, color = "black") +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 3, 
             color = "black", 
             aes(label = paste(stat(y.label), "at", after_stat(x.label), "nm"))) +
  facet_grid(rows = vars(spct.idx))

## -----------------------------------------------------------------------------
ggplot(white_led.raw_spct, aes(w.length, counts_3)) + 
  geom_line() + 
  stat_spikes(color = "red", z.threshold = 8, max.spike.width = 7)

## -----------------------------------------------------------------------------
ggplot(despike(white_led.raw_spct, z.threshold = 8, max.spike.width = 7), 
       aes(w.length, counts_3)) + 
  geom_line() + 
  stat_spikes(color = "red", z.threshold = 8, max.spike.width = 7)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color() + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color(chroma.type = "CC") + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(clip_wl(sun.spct)) + 
  stat_color(chroma.type = beesxyzCMF.spct) + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() +
  stat_color(shape = 21, color = "black") + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color(geom = "bar") + 
  geom_line(color = "black") +
  geom_point(shape = 21, color = "black", stroke = 1.2, fill = "white") +
  scale_fill_identity() + 
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color(geom = "bar", chroma.type = beesxyzCMF.spct) + 
  geom_line(color = "black") +
  geom_point(shape = 21, color = "black", stroke = 1.2, fill = "white") +
  scale_fill_identity() + 
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(shape = spct.idx) +
  stat_color() + scale_color_identity() +
  geom_line() + 
  facet_grid(cols = vars(spct.idx), scales = "free_y")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_wb_box(w.band = VIS_bands(), color = "white") +
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + stat_wb_column(w.band = VIS_bands()) + geom_line() +
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_wb_hbar(w.band = VIS_bands(), size = 1.2) +
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_wb_box(w.band = PAR(), color = "white", ypos.fixed = 0.85) +
  stat_wb_label(w.band = PAR(), ypos.fixed = 0.85) +
  scale_fill_identity() + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_wl_summary()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_wl_summary(range = c(300,350), geom = "rect") +
  geom_line()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wl_summary(geom = "hline", color = "red") +
  stat_wl_summary(label.fmt = "Mean = %.3g", color = "red", vjust = -0.3)

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_summary(range = c(400,500), 
                  geom = "rect", 
                  alpha = 0.2, 
                  fill = color_of(450)) +
  stat_wl_summary(range = c(400,500), 
                  label.fmt = "Mean = %.3g", 
                  vjust = -0.3, 
                  geom = "text") + 
  geom_line()

## -----------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3g", vjust = 1.2, show.legend = FALSE) +
  facet_grid(cols = vars(spct.idx))

## -----------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3g", vjust = 1.2, show.legend = FALSE) +
  facet_grid(cols = vars(spct.idx), scales = "free_y")

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wb_hbar(w.band = PhR(), size = 1.3) +
  stat_wb_mean(aes(color = after_stat(wb.color)), w.band = PhR(), ypos.mult = 0.95) +
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_hbar(w.band = c(400,500), size = 1.2) +
  stat_wb_mean(aes(color = after_stat(wb.color)),
               w.band = c(400,500), ypos.mult = 0.95) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wb_hbar(w.band = list(Blue(), Red()), size = 1.2) +
  stat_wb_mean(aes(color = after_stat(wb.color)),
               w.band = list(Blue(), Red()), ypos.mult = 0.95, 
               hjust = 1, angle = 90) +
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = PhR()) +
  stat_wb_total(w.band = PhR()) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = c(400,500)) +
  stat_wb_total(w.band = c(400,500)) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct * yellow_gel.spct) +
  stat_wb_box(w.band = Plant_bands(), color = "white", ypos.fixed = 0.7) +
  stat_wb_column(w.band = Plant_bands(), color = "white", alpha = 0.5) +
  stat_wb_mean(w.band = Plant_bands(), label.fmt = "%1.2f",
               ypos.fixed = 0.7, size = 2) +
  geom_line() + 
  scale_fill_identity() + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = PAR()) +
  stat_wb_irrad(w.band = PAR(), unit.in = "energy", time.unit = "second") +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") +
  stat_wb_box(w.band = PAR()) +
  stat_wb_irrad(w.band = PAR(),
                unit.in = "photon", time.unit = "second", 
                aes(label = sprintf("%s = %.3g", 
                                    after_stat(wb.name),
                                    after_stat(wb.yint) * 1e6))) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = PAR()) +
  stat_wb_e_irrad(w.band = PAR()) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = CIE()) +
  stat_wb_e_irrad(w.band = CIE()) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.daily.spct) +
  stat_wb_box(w.band = CIE()) +
  stat_wb_e_irrad(w.band = CIE(), time.unit = "day", label.mult = 1e-3) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") +
  stat_wb_box(w.band = VIS_bands(), color = "black") +
  stat_wb_column(w.band = VIS_bands(), color = NA, alpha = 0.5) +
  stat_wb_q_irrad(w.band = VIS_bands(), label.mult = 1e6, size = 2) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wb_hbar(w.band = PAR(), size = 1.4) +
  stat_wb_e_sirrad(aes(color = after_stat(wb.color)), 
                   w.band = PAR(), ypos.mult = 0.95) +
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") +
  stat_wb_column(w.band = PAR(), alpha = 0.8) +
  stat_wb_q_sirrad(w.band = PAR(), 
                   mapping =
                     aes(label = sprintf("Total %s = %.3g", 
                                         after_stat(wb.name),
                                         after_stat(wb.yint) * 1e6)), 
                   ypos.mult = 0.55) +
  stat_wb_q_sirrad(w.band = PAR(),
                   mapping = 
                     aes(label = sprintf("Mean %s = %.3g", 
                                         after_stat(wb.name), 
                                         after_stat(wb.ymean) * 1e6)), 
                   ypos.mult = 0.45) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_box(w.band = waveband(CIE()), ypos.fixed = 0.85) +
  stat_wb_e_sirrad(w.band = CIE(), ypos.fixed = 0.85) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.daily.spct) +
  stat_wb_box(w.band = waveband(CIE()), ypos.fixed = 34e3) +
  stat_wb_e_sirrad(w.band = CIE(),
                   label.fmt = "%.2g kj / day",
                   time.unit = "day",
                   ypos.fixed = 34e3) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
my.bands <- split_bands(c(300,800), length.out = 10)
ggplot(sun.spct, unit.out = "photon") +
  stat_wb_hbar(w.band = my.bands, size = 1.4) +
  stat_wb_q_sirrad(geom = "label", w.band = my.bands, 
                   size = 2.5, ypos.fixed = 3.5e-6) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_column(w.band = VIS_bands(), alpha = 0.5) +
  stat_wb_e_irrad(w.band = VIS_bands(), angle = 90,
                   ypos.fixed = 0.05, hjust = 0,
                   aes(label = paste(after_stat(wb.name), 
                                     after_stat(y.label), 
                                     sep = " = "))) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") +
  stat_wb_column(w.band = VIS_bands(), alpha = 0.5) +
  stat_wb_q_irrad(w.band = VIS_bands(), angle = 90, 
                  label.mult = 1e6, ypos.fixed = 1e-7, hjust = 0,
                  aes(label = paste(after_stat(wb.name), 
                                    after_stat(y.label), 
                                    sep = " = "))) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
my.bands <- split_bands(c(300,800), length.out = 10)
ggplot(sun.spct) +
  stat_wb_column(w.band = my.bands, alpha = 0.5) +
  stat_wb_e_irrad(w.band = my.bands, angle = 90,
                  ypos.fixed = 0.05, hjust = 0) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(data.frame(w.length = 300:800), aes(w.length)) +
  stat_wl_strip(w.band = VIS_bands(), ymax = Inf, ymin = -Inf) +
  stat_wb_label(w.band = VIS_bands(), angle = 90) +
  scale_fill_identity() + 
  scale_color_identity() + 
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(breaks = seq(from = 300, to = 800, by = 25)) +
  labs(x = "Wavelength (nm)", title = "Colours according to ISO standard") +
  theme_minimal()


## -----------------------------------------------------------------------------
ggplot(data.frame(w.length = 300:1100), aes(w.length)) +
  stat_wl_strip(w.band = RBV_bands(), ymax = 1, ymin = 3) +
  stat_wb_label(w.band = RBV_bands(), ypos.fixed = 2, angle = 90, vjust = 0.3, size = 3) +
  stat_wl_strip(w.band = MSS_bands(), ymax = 4, ymin = 6, na.rm = TRUE) +
  stat_wb_label(w.band = MSS_bands(), ypos.fixed = 5, angle = 90, vjust = 0.3, size = 3) +
  stat_wl_strip(w.band = ETM_bands(), ymax = 7, ymin = 9, na.rm = TRUE) +
  stat_wb_label(w.band = ETM_bands(), ypos.fixed = 8, angle = 90, vjust = 0.3, size = 3) +
  stat_wl_strip(w.band = OLI_bands(), ymax = 10, ymin = 12, na.rm = TRUE) +
  stat_wb_label(w.band = OLI_bands(), ypos.fixed = 11, angle = 90, vjust = 0.3, size = 3) +
  scale_fill_identity() + 
  scale_color_identity() + 
  scale_y_continuous(labels = c("RBV", "MSS", "TM/ETM", "OLI"), 
                     breaks = c(2,5,8,11),
                     limits = c(0, 13),
                     name = "Imager",
                     sec.axis = dup_axis(labels = c("L1-L2", "L1-L5", "L4-L7", "L8"), name = "Landsat mission")) +
  scale_x_continuous(breaks = seq(from = 300, to = 1200, by = 100),
                     limits = c(400, 1100),
                     sec.axis = dup_axis()) +
  labs(x = "Wavelength (nm)", title = "Landsat imagers: VIS and NIR bands") +
  theme_classic()

## -----------------------------------------------------------------------------
ggplot(data.frame(w.length = 100:400), aes(w.length)) +
  stat_wl_strip(w.band = UV_bands("ISO"), ymax = 1, ymin = 3, color = "white") +
  stat_wb_label(w.band = UV_bands("ISO"), ypos.fixed = 2, size = 3) +
  stat_wl_strip(w.band = UV_bands("CIE"), ymax = 4, ymin = 6, color = "white") +
  stat_wb_label(w.band = UV_bands("CIE"), ypos.fixed = 5, size = 3) +
  stat_wl_strip(w.band = UV_bands("plants"), ymax = 7, ymin = 9, color = "white") +
  stat_wb_label(w.band = UV_bands("plants"), ypos.fixed = 8, size = 3) +  
  stat_wl_strip(w.band = UV_bands("none"), ymax = 10, ymin = 12, color = "white") +
  stat_wb_label(w.band = UV_bands("none"), ypos.fixed = 11, size = 3) +
  stat_wl_strip(w.band = UV_bands("medical"), ymax = 13, ymin = 15, color = "white") +
  stat_wb_label(w.band = UV_bands("medical"), ypos.fixed = 14, size = 3) +  

  scale_fill_identity() + 
  scale_color_identity() + 
  scale_y_continuous(labels = c("ISO", "CIE", "plants", "none", "medical"), 
                     breaks = c(2,5,8,11,14),
                     limits = c(0, 16),
                     name = "Definition",
                     sec.axis = dup_axis(labels = 
                      c("use", "use", "use?", "avoid!", "avoid!"), name = "Recommendation")) +
  scale_x_continuous(breaks = c(seq(from = 100, to = 400, by = 50), 280, 315),
                     limits = c(100, 400),
                     sec.axis = 
                       dup_axis(breaks = 
                                c(100, 150, 200, 220, 250, 290, 320, 340, 400))) +
  labs(x = "Wavelength (nm)", title = "UV bands",
       subtitle = "According to ISO standard, CIE recommendations, and non-standard use") +
  theme_classic()

## -----------------------------------------------------------------------------
my.data <- data.frame(x = 300:800)
ggplot(my.data, aes(x)) + stat_wl_strip(ymin = -1, ymax = 1, color = NA) +
    scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() +
  stat_wl_strip(ymin = -Inf, ymax = -0.025) + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wl_strip(w.band = VIS_bands(), ymin = -Inf, ymax = -0.025) + 
  scale_fill_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_strip(w.band = VIS_bands(), ymin = -Inf, ymax = Inf, alpha = 0.4) + 
  scale_fill_identity() +
  geom_line()

## -----------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_strip(alpha = 0.4, ymin = -Inf, ymax = Inf) + 
  scale_fill_identity() +
  geom_line()

## -----------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") +
  stat_wl_strip(alpha = 0.4, ymin = -Inf, ymax = Inf) + 
  stat_wb_box(w.band = PAR()) +
  stat_wb_q_irrad(w.band = PAR(), label.mult = 1e6,
                  aes(label = paste(after_stat(wb.name), 
                                    " = ", 
                                    after_stat(y.label), 
                                    sep = ""))) +  
  geom_line() + 
  scale_fill_identity() + scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + geom_spct()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct(fill = color_of(sun.spct))

## -----------------------------------------------------------------------------
ggplot(sun.spct * yellow_gel.spct) + 
  geom_spct(fill = color_of(sun.spct * yellow_gel.spct))

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide(alpha = 0.4) +
  geom_line()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide(ymax = -0.025) +
  geom_line() 

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide() +
  geom_spct(alpha = 0.75, colour = "white", size = 1) 

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide() +
  geom_line(size = 2, colour = "white") +
  geom_line(size = 1, colour = "black") +
  geom_hline(yintercept = 0, colour = "grey92")

## -----------------------------------------------------------------------------
color_chart(colors())

## -----------------------------------------------------------------------------
color_chart(grep("blue", colors(), value = TRUE), ncol = 5, text.size = 4)

## -----------------------------------------------------------------------------
 color_chart(w_length2rgb(570:689, color.name = as.character(570:689)), 
            use.names = TRUE, text.size = 4) +
  ggtitle("Reddish colors", subtitle = "Labels: wavelength (nm)")

