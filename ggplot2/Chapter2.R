library(ggplot2)

# 2.2 Fuel Economy Data

mpg

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()

ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(aes(colour = "blue"))
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(colour = "blue")

# 2.5 Facetting

ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~ class)

# 2.6 Plot Geoms
geom_smooth()
geom_boxplot()
geom_histogram()
geom_freqploy()
geom_bar()
geom_path()
geom_line()

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(se = FALSE)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.2)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 1)

library(mgcv)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x))

# method = lm
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "lm")
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# 2.6.2 Boxplot and Jittered Points
ggplot(mpg, aes(drv, hwy)) + geom_point()
ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()




