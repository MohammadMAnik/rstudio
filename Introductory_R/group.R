# 집단요약
mtcars
car <- mtcars

# label
car <- within(car, am <- factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")))

# split()
gear <- split(x=car$mpg, f=car$am)
gear

sapply(gear, length)
sapply(gear, mean)

# unstack()
unstack(data.frame(car$mpg, car$am))

# iris
iris
head(iris)

# rowsum
rowsum(x=iris[-5], group = iris$Species)

# table
table(car$gear)

table(car$am)

table(car$am, car$gear)










