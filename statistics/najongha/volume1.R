# 나종화 I권

month.name
month.abb

help("ANOVA")

apropos("ANOVA")

RSiteSearch("lattice")

v <- c(2, 4, 6)
v
class(v)

str(v)

names(v) <- c("s1", "s2", "s3")
v

class(v)

v1 <- factor(c("low", "high"))
v <- data.frame(v, v1)
v

gender <- gl(2, 5, labels = c("male", "female"))
table(gender)
wh <- c("white", "black", "white", "black", "black", "white", "black", "white", "black", "black")
wh

(t <- table(gender, wh))

margin.table(t, 1)
margin.table(t, 2)

prop.table(t, 1)

data.frame

midterm <- data.frame(id=c(1, 2, 3), name=c("Kim", "Lee", "Park"), score=c(100, 100, 20))
midterm

midterm[midterm$name == "Kim", c("id", "score")]
midterm[midterm$name == "Kim", "score"]
midterm[midterm$score <= 20, ]

(t <- ts(1:10, frequency=4, start=c(2000, 3)))

plot(t)

set.seed(1000)
z <- ts(matrix(rnorm(300), 100, 3), start=c(1961, 1), frequency=12)
class(z)

head(z)

plot(z)

is.data.frame(z)

is.matrixz(z)


iris

lm.iris <- lm(Sepal.Length ~ Sepal.Width, data=iris)
lm.iris

ls(lm.iris)

summary(lm.iris)
plot(lm.iris)

par(mfrow=c(2,2))
plot(lm.iris)

a <- function(x) {
  par(mfrow=c(2, 2))
  hist(x)
  boxplot(x)
  qqnorm(x)
  qqline(x)
  plot(density(x), type="l")
}

a(iris$Sepal.Length)

write.csv(a, file=            )


a <- c(2, 7, 5, 3, 1, 4, 6)
sum(a)

prod(a)
 max(a)
min(a)

diff(a)
which.max(a)
which.min(a)

range(a)
mean(a)
median(a)
sd(a)
var(a)
quantile(a)
plot(scale(a))

sort(a, decreasing = TRUE)

rev(a)

rank(a)

cumsum(a)
