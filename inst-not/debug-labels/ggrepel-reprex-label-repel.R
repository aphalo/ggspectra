# test for bug in ggrepel/ggplot2
library(ggplot2)
library(ggrepel)

# sometimes works, usually R crashes
n = 1000L
my.data <- data.frame(x = runif(n), y = runif(n),
                      my.label = c(rep("", n/100 - 1), "abcd"),
                      stringsAsFactors = FALSE)

# ggplot(my.data, aes(x, y, label = my.label)) +
#  geom_label()

fig_label <-
  ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label()

str(fig_label)

fig_label

fig_label_repel <-
  ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label_repel()

str(fig_label_repel)

fig_label_repel

fig_label_s <-
  ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label_s()

str(fig_label_s)

fig_label_s

# never works, R crashes
# in 2025 no crash, warning and empty plot rendered

n = 10000L
my.data <- data.frame(x = runif(n), y = runif(n),
                      my.label = c(rep("", n/1000 - 1), "abcd"))

# ggplot(my.data, aes(x, y, label = my.label)) +
#  geom_label()

fig <-
  ggplot(my.data, aes(x, y, label = my.label)) +
  geom_label_repel()

fig

