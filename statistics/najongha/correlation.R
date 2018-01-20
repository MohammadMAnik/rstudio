# correlation
# cor(x, y=NULL, use="everyting", method=c("pearson", "kendal", "spearman"))
# default = "pearson"
data(longley)
str(longley)

# pearson's correlation
cor(longley)

# multivariate scatter plot
pairs(longley)

# Spearman's correlation
cor(longley, method="spearman")

# Kendall's correlation
cor(longley, method="kendall")

# cor.test()
# cor.test(formula, data, subset, na.action, ...)

# data = cats, correlation analysis
library(MASS)
data(cats)
str(cats)

summary(cats)

# plot
with(cats, plot(Bwt, Hwt))
title(main = "Heart Weight (g) vs. Body Weight (kg) \nof Domestic Cats")

# cor
with(cats, cor(Bwt, Hwt))

with(cats, cor(Bwt, Hwt))^2

# cor.test
with(cats, cor.test(Bwt, Hwt))

# cor.test, Sex==Female
with(cats, cor.test(~ Bwt + Hwt, subset=(Sex=="F")))

# scatter plot
with(cats, plot(Bwt, Hwt, type = "n", xlab = "Body Weight in kg", ylab = "Heart Weight in g",
                main = "Heart Weight vs. Body Weight of Cats"))

with(cats, points(Bwt[Sex=="F"], Hwt[Sex=="F"], pch=16, col="red"))
with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch=17, col="blue"))

# correlation visualization
library(psych)

# pairs.panels()
# data = iris
data(iris)
pairs.panels(iris[1:4], scale = FALSE) # scale defalut = FALSE
pairs.panels(iris[1:4], scale = TRUE)

# Species
pairs.panels(iris[1:4], bg=c("red", "yellow", "blue")[iris$Species], pch=21, main="Fisher Iris data by Sepcies")

# cor.plot()
cor.plot(cor(mtcars))
