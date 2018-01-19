# Chapter 17
# two samples comparison

# data = mtcars
data(mtcars)
attach(mtcars)

boxplot(mpg ~ am)

t.test(mpg ~ as.factor(am))

# data = sleep
data(sleep)
attach(sleep)

?sleep

names(sleep)

t.test(extra[group==1], extra[group==2], paired = TRUE)

# one sample t-test group 2 - group 1
sleep.d <- with(sleep, extra[group == 2] - extra[group == 1])
t.test(sleep.d)

# one sample t-test group 1 - group 2
sleep.da <- with(sleep, extra[group == 1] - extra[group == 2])
t.test(sleep.da)

# variance equality
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)

plot(x)
plot(y)

var.test(x, y)

# proportion test
prop.test(x, n, p=NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)

set.seed(500)
heads <- rbinom(1, size = 100, prob = 0.5)
prop.test(heads, n = 100, p = 0.5)

# 4 patients group, smokers cases
smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

prop.test(smokers, n = patients)

# nonparametric
# inspection time
A <- c(6.0, 2.2, 2.3, 3.3, 3.7, 2.3, 2.2, 2.4, 4.4, 3.9)
B <- c(2.7, 3.9, 7.8, 5.8, 2.3, 2.4, 6.9, 4.4, 2.4, 2.5)
plot(density(A))
plot(density(B))

boxplot(A)
boxplot(B)

wilcox.test(A, B)
