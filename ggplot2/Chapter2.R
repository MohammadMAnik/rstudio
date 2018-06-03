library(ggplot2)

mpg

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()

ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(aes(colour = "blue"))
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(colour = "blue")

