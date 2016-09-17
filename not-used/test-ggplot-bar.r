library(photobiologyPlants)
library(ggplot2)

my.data <-
  data.frame(dwt = c(10, 8, -5, 20, 12, -15),
             part = factor(rep( c("leaves", "stem", "roots"), 2),
                           levels = c("roots", "stem", "leaves")),
             treatment = factor(rep(c("A", "B"), c(3,3)))
  )
levels(my.data$part)

ggplot(my.data, aes(treatment, dwt, fill = part)) +
  geom_bar(stat = "identity")

sessionInfo()

