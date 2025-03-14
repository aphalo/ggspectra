library(ggplot2)
library(ggpmisc)

mensur <- read.csv("./inst-not/colour-bug/mensurations.csv", sep = ";")

gcor <- ggplot(mensur, aes(x=cuisse, y=biceps))
gcor +
  geom_point() +
  facet_wrap(~GENRE) +
  stat_smooth(method="lm",
              geom="smooth",
              show.legend = TRUE,
              fullrange = TRUE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = "right",
               label.y = 0.15,
               formula = y ~ x,
               parse = TRUE,
               size = 3)

gcor <- ggplot(mensur, aes(x=cuisse, y=biceps, colour=factor(GENRE)))
gcor + geom_point() + facet_wrap(~GENRE) +
  stat_smooth(method="lm",
              geom="smooth",
              show.legend = TRUE,
              fullrange = TRUE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               geom = "text", # default geom is "text_npc", for label position
                              # in data coordinates "text" is used
               label.x = 50,
               label.y = c(44, 23.5),
               hjust = 0,
               formula = y ~ x,
               parse = TRUE,
               size = 3)
