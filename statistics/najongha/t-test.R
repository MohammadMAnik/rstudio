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


