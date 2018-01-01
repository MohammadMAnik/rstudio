# 02_one sample t-test

# one-sample t-test
LungCapData <- read.table("rstudio/dataset/LungCapData.txt", header = TRUE, sep = "\t")

names(LungCapData)
class(LungCapData)

dim(LungCapData)

help(t.test)

# t-test command
# LungCapData$LungCap
# Ho : mu >=8, Ha : mu < 8
# one-sided 95% confidence
t.test(LungCapData$LungCap, mu = 8, alternative = "less", conf.level = 0.95)

# two-sided
t.test(LungCapData$LungCap, mu = 8, alt = "two.sided", conf = 0.95)

# two-sided test is the default in R
t.test(LungCapData$LungCap, mu = 8, conf = 0.95)

t.test(LungCapData$LungCap, mu = 8, conf = 0.99)

