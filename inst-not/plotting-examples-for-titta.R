# open .spct RDA file

library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ggplot2)
library(ggspectra)

my_wbands <- list(
  blue = waveband(c(400, 500), wb.name = "blue"),
  green = waveband(c(500, 600), wb.name = "green"),
  red = waveband(c(600, 700), wb.name = "red"),
  far_red = waveband(c(700, 800), wb.name = "far_red"),
  IR1 = waveband(c(800, 900), wb.name = "IR1"),
  IR2 = waveband(c(900, 1050), wb.name = "IR2"),
  Sum = waveband(c(400, 1050), wb.name = "sum")
)

# one spectrum

q_irrad(sun.spct, my_wbands, scale.factor = 1e6)

# a collection

q_irrad(sun_evening.mspct, my_wbands, scale.factor = 1e6)

# spectra in long form

q_irrad(sun_evening.spct, my_wbands, scale.factor = 1e6)

# one spectrum

# changes the default! (to avoid write 'unit.out = "photon"' in every plot)
photon_as_default()

autoplot(sun.spct)
# "geometry"
autoplot(sun.spct, geom = "line")
autoplot(sun.spct, geom = "spct")
autoplot(sun.spct, geom = c("spct", "line"))
# change theme
autoplot(sun.spct) + theme_bw()
autoplot(sun.spct) + theme_light()
autoplot(sun.spct) + theme_classic()
# different wavebands
autoplot(sun.spct, w.band = my_wbands[1:6])
autoplot(sun.spct, w.band = my_wbands[7])
# normalised to 1 at the maximum
autoplot(sun.spct, norm = "max")
# peaks
autoplot(sun.spct, span = 31)
# valleys
autoplot(sun.spct, annotations = c("+", "valleys"), span = 31)
# segments instead of bars
autoplot(sun.spct, annotations = c("+", "segments"), span = 31)
# no summary values
autoplot(sun.spct, annotations = c("-", "summaries"), span = 31)
# titles if the object contains metadata
autoplot(sun.spct, annotations = c("+", "title:what:when:how"), span = 31)
autoplot(sun.spct, annotations = c("+", "title:objt:when"), span = 31)
# Any title, subtitle and caption
autoplot(sun.spct) +
  labs(title = "My Title", subtitle = "My subtitle\nin two lines",
       caption = "This is just a demonstration...")

# collection of spectra

## default is one plot
autoplot(sun_evening.mspct)
## separate panels in two columns
autoplot(sun_evening.mspct, facets = 2)
## Change title key
autoplot(sun_evening.mspct, idfactor = "Time point")
## mean of the spectra
autoplot(sun_evening.mspct, plot.data = "mean")
## median of the spectra
autoplot(sun_evening.mspct, plot.data = "median")

# Multiple spectra in long form
# just as above
autoplot(sun_evening.spct)

# Manual plot construction
ggplot(sun_evening.mspct[1:3], aes(colour = Lamp), idfactor = "Lamp") +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_x_wl_continuous(limits = c(300,1050), breaks = seq(300, 1000, by = 100)) +
  scale_y_s.q.irrad_continuous() +
  scale_colour_manual(values = c(time.01 = "blue", time.02 = "goldenrod", time.03 = "red")) +
  theme_classic(base_size = 12) + theme(legend.position = "inside",
                                        legend.text = element_text(size = 12),
                                        legend.position.inside = c(0.1, 0.9))
