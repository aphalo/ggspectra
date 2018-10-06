# test for bug in ggrepel/ggplot2
library(ggplot2)
library(ggrepel)

# sometimes works, usually R crashes
n = 1000L
my.data <- data.frame(x = runif(n), y = runif(n),
                      my.label = c(rep("", n/100 - 1), "abcd"))

# ggplot(my.data, aes(x, y, label = my.label)) +
#  geom_label()

ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label_repel()

# never works, R crashes

n = 10000L
my.data <- data.frame(x = runif(n), y = runif(n),
                      my.label = c(rep("", n/1000 - 1), "abcd"))

# ggplot(my.data, aes(x, y, label = my.label)) +
#  geom_label()

ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label_repel()
