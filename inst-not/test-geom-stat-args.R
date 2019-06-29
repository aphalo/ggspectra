library(ggplot2)

ggplot(mpg, aes(factor(cyl), hwy)) +
  stat_summary(fun.y = mean, geom = "col") +
  stat_summary(aes(label = round(..y..)),
               fun.y = mean, geom = "text", colour = "yellow",
               nudge_x = -0.2)

ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_col(fun.y = mean, stat = "summary") +
  geom_text(aes(label = round(..y..)),
               fun.y = mean, stat = "summary", colour = "yellow",
               nudge_x = -0.2)

