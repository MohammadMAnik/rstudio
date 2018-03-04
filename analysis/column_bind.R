rm(list=ls())

name <- c("a", "b", "c", "d", "e")
name

var <- c(1, 2, 3, 4, 5)

x <- cbind(name, var)
x

y <- c(6, 7, 8, 9, 10)

# y를 열로 추가할 때
z <- cbind(x, y)
z

# 순서를 다르게 하면
z <- cbind(y, x)
z


