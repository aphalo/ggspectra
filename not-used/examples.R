library(photobiology)
library(ggplot2)
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_peaks(span = 21, geom = "text")

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_valleys(span = 21, geom = "text")

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_peaks(span = 21, geom = "point", colour = "red") +
  stat_valleys(span = 21, geom = "point", colour = "blue") +
  stat_peaks(span = 51, geom = "text", colour = "red", vjust = -0.3, label.fmt = "%3.0f nm") +
  stat_valleys(span = 51, geom = "text", colour = "blue", vjust = 1.2, label.fmt = "%3.0f nm")

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_color() + scale_color_identity()

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_integral()

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_integral(range = c(300,500), label.fmt = "%.3f", color = "red") +

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_integral(geom = "hline")

ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_integral(label.fmt = "%.3f", color = "red")