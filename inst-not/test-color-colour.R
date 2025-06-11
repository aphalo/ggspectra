library(ggplot2)
library(ggspectra)

p0 <-
  ggplot(sun.spct) +
  geom_point() +
  scale_fill_identity() +
  scale_color_identity()

# default mapping to returned value in column wl.color works for fill
p0 +
  stat_peaks(span = NULL, geom = "label", colour = "white")

p0 +
  stat_peaks(span = NULL, geom = "label",
             mapping = aes(fill = after_stat(wl.color),
                           color = after_stat(BW.color)))

p0 +
  stat_peaks(span = NULL, geom = "label",
             mapping = aes(label = after_stat(x.label),
                           fill = after_stat(wl.color),
                           color = after_stat(BW.color)))

p0 +
  stat_peaks(span = NULL, geom = "label",
             parse = TRUE,
             mapping = aes(label = after_stat(x.label),
                           fill = after_stat(wl.color),
                           color = after_stat(BW.color)))

p0 +
  stat_peaks(span = NULL, geom = "label",
             mapping = aes(label = paste(after_stat(x.label), "at", after_stat(y.label)),
                           fill = after_stat(wl.color),
                           color = after_stat(BW.color)))
