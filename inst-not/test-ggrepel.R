library(tidyverse)
library(ggrepel)
library(scales)

dat <- data.frame(
  x = seq(1, 10, length.out = 10),
  y1 = seq(1, 5, length.out = 10),
  y2 = seq(1, 6, length.out = 10)
)

# convert to long format
dat <- dat %>%
  gather(var, value, -x) %>%
  mutate(value = signif(value, 3))

# plot it with geom_label
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label(aes(label = value))

# plot it with geom_label_repel (works ok)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label_repel(aes(label = value))

# plot it with geom_label and displacing labels in opposite directions
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label(aes(label = value),
             nudge_y = ifelse(dat$var == "y2", 1, -1) * 1)

# same with geom_label_repel (works ok)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label_repel(aes(label = value),
                   nudge_y = ifelse(dat$var == "y2", 1, -1) * 1)

# same with geom_label_repel (works ok)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_text_repel(aes(label = value),
                  nudge_y = ifelse(dat$var == "y2", 1, -1) * 1)

# same with geom_label_repel (works ok)
ggplot(data = dat, aes(x = rev(x), y = value, color = var)) +
  geom_line() +
  geom_text_repel(aes(label = value),
                  nudge_y = ifelse(dat$var == "y2", 1, -1) * 0.5)

# same with geom_label_repel (works ok)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_text_repel(aes(label = value),
                  nudge_y = ifelse(dat$var == "y2", 1, -1) * 1,
                  angle = 90)

# plot it with geom_label keeping only final label and displacing labels in opposite directions
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label(aes(label = if_else(x == max(x), comma(value), NA_character_)),
             nudge_y = ifelse(dat$var == "y2", 1, -1), na.rm = TRUE)

# same with geom_label_repel (this time both are pushed in same direction **BUG ??**)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_label_repel(aes(label = if_else(x == max(x), comma(value), NA_character_)),
                   nudge_y = ifelse(dat$var == "y2", 1, -1), na.rm = TRUE)
# same with geom_label_repel (this time both are pushed in same direction **BUG ??**)
ggplot(data = dat, aes(x = x, y = value, color = var)) +
  geom_line() +
  geom_text_repel(aes(label = if_else(x == max(x), comma(value), NA_character_)),
                  nudge_y = ifelse(dat$var == "y2", 1, -1), na.rm = TRUE)

### ggspectra

library(ggspectra)
library(ggrepel)

ggplot(sun.spct) +
  geom_line() +
  stat_peaks()

ggplot(sun.spct) +
  geom_line() +
  stat_peaks(geom = "text", vjust = -0.5, span = 31)

ggplot(sun.spct) +
  geom_line() +
  stat_peaks(geom = "text_repel", vjust = -0.5, span = 31)

devtools::session_info()

# library(ggspectra)
#
# set.seed(1234)
# ggplot(sun.spct) +
#   geom_point() +
#   stat_peaks(geom = "label_repel", span = 51) +
#   expand_limits(y = 1)

