library(ggplot2)

my.df <- data.frame(a = 1:20, b = 1:20)
# o.k.
ggplot(my.df, aes(a, b)) + stat_identity(geom = "text", aes(label = b, y = b))
# o.k.
ggplot(my.df, aes(a, b)) + stat_identity(geom = "text", aes(label = ..y.., y = b))
# error
# Error in eval(expr, envir, enclos) : object 'y' not found
# Called from: eval(expr, envir, enclos)
ggplot(my.df, aes(a, b)) + stat_identity(geom = "text", aes(label = ..y.., y = ..y..))

ggplot(my.df, aes(a, b)) + stat_identity(geom = "label", aes(label = b, y = b))
# o.k.
ggplot(my.df, aes(a, b)) + stat_identity(geom = "label", aes(label = ..y.., y = b))
# error
# Error in eval(expr, envir, enclos) : object 'y' not found
# Called from: eval(expr, envir, enclos)
ggplot(my.df, aes(a, b)) + stat_identity(geom = "label", aes(label = ..y.., y = ..y..))

# o.k.
ggplot(my.df, aes(a, b)) + stat_identity(geom = "point", aes(y = b))
# error
# Error in eval(expr, envir, enclos) : object 'y' not found
# Called from: eval(expr, envir, enclos)
ggplot(my.df, aes(a, b)) + stat_identity(geom = "point", aes(y = ..y..))
