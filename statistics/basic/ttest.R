
# t.test

A <- c(80, 80, 90, 90, 100, 100, 110, 110, 120, 120)
plot(density(A))

B <- c(100, 100, 110, 110, 120, 120, 130, 130, 140, 140)
plot(density(B))

C <- c(60, 60, 70, 70, 80, 80, 90, 90, 100, 100)
plot(density(C))

t.test(A, B, var.equal=TRUE, conf.level=0.99)
t.test(A, B, var.equal=TRUE)
t.test(B, C, var.equal=TRUE)
t.test(A, C, var.equal=TRUE)

D <- c(1, 2, 3, 4, 5, 6, 7)
mean(D)
var(D)
sd(D)

E <- c(8,9,10,11,12,13,14)
mean(E)
var(E)
4.66/7
0.77+0.77
sqrt(1.54)
-7/1.24

t.test(D, E)


# sample
m0 <- rnorm(30, mean=0, sd=1)
m3 <- rnorm(30, mean=3, sd=1)

plot(m0)

# normality test (using graph)
#1 Q-Q plot
qqnorm(m0)
qqline(m0, col=2)

#2 Boxplot
boxplot(m0)

#3 Normal Probability Plot(정규확률그림)
qqnorm(m0)
qqline(m0, col="red")

#4 Histogram
hist(m0)

# normality test(using statistical method)
#1 chi-square test (sample < 20)

#2 D'Agostino-Pearson test (sample > 20), using skewness, kurtosis

#3 Jarque-Bera test

#4 Kolmogorov-Smirnov Goodneess of Fit test (compare normal distribution)
ks.test(m0, "pnorm", mean=mean(m0), sd=sd(m0))

#5 Liliefors test

#6 Shapiro-Wilk test
shapiro.test(m0)

# equal variance
var.test(m0, m3)

# t.test
t.test(m0, m3)

set.seed(1); T1 <- c(sample(140:160, 30, replace=TRUE))
set.seed(2); T2 <- c(sample(140:160, 30, replace=TRUE))
data <- data.frame(T1, T2)
data

plot(T1)
plot(T2)

var.test(T1, T2)

t.test(T1, T2, var.equal=TRUE)

uni <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
plot(uni)
hist(uni)

