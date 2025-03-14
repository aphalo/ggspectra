library(ggrepel)

my.data <- data.frame(x = 1:3, y = 1:3, label = c("one", "two", "three"))

# < 0
ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text(hjust = -0.5) +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel(hjust = -0.5, direction = "x") +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel(vjust = -0.5, direction = "y") +
  expand_limits(x = c(0, 4), y = c(0, 4))

# > 1
ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text(hjust = 1.5) +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel(hjust = 1.5, direction = "x") +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  expand_limits(x = c(0, 4), y = c(0, 4))

ggplot(my.data, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel(vjust = 1.5, direction = "y") +
  expand_limits(x = c(0, 4), y = c(0, 4))

