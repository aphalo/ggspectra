library(photobiology)
library(ggspectra)
library(ggplot2)

fig <-
  ggplot(sun.spct) +
  geom_line() +
  scale_x_wl_continuous(sec.axis = sec_axis_w_frequency()) +
  scale_y_s.e.irrad_continuous() +
  stat_peaks(span = 31L, color = "red") +
  stat_peaks(geom = "text", span = 31L, color = "red", hjust = -0.2, vjust = -0.4, angle = 33) +
  stat_valleys(span = 31L, color = "blue") +
  stat_valleys(geom = "text", span = 31L, color = "blue", hjust = 1.2, vjust = 1.2, angle = 33) +
  stat_wl_strip(ymin = -Inf, ymax = -0.025) +
  stat_wl_strip(ymin = 0.875, ymax = Inf) +
  expand_limits(y = 0.85) +
  scale_fill_identity() +
  theme_bw(16)

fig

png("inst-not/ggplot2-exts/ggspectra.png", width = 700, height = 600)
print(fig)
dev.off()


