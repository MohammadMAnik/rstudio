# anova
# data = mtcars
data(mtcars)
attach(mtcars)

?mtcars
plot(wt ~ cyl)
plot(mpg ~ cyl)

av <- aov(mpg ~ as.factor(cyl))
summary(av)

# baskball, typing
Time <- c("Morning", "Morning", "Night", "Night", "Morning", "Morning", "Night", "Night", "Morning", "Morning", "Night", "Night", "Morning", "Morning", "Night", "Night")
Shoes <- c("Others", "Others", "Others", "Others", "Favorite", "Favorite", "Favorite", "Favorite", "Others", "Others", "Others", "Others", "Favorite", "Favorite", "Favorite", "Favorite")
Made <- c(25, 26, 27, 27, 32, 22, 30, 34, 35, 34, 33, 30, 33, 37, 36, 38)

baskball <- cbind(Time, Shoes, Made)
baskball

int <- aov(Made ~ Time*Shoes)
summary(int)

noint <- aov(Made ~ Time + Shoes)
summary(noint)

tapply(Made, Time, mean)
tapply(Made, Shoes, mean)

# diagnostic plots
plot(av)

# multiple comparison
# TukeyHSD : Tukey's honestly significant differences
(mc <- TukeyHSD(av))

plot(mc, las = "2")

# boxplot
boxplot(mpg ~ cyl, xlab = "Cylinders", ylab = "Miles per gallon")

# interaction.plot
gear <- factor(gear)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type = "b", col = c(1:3), pch = c(1:3), legend = TRUE)

# plotmeans
# package = gplots
install.packages("gplots")
library(gplots)
plotmeans(mpg ~ cyl) 





