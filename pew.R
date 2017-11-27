setwd("/Users/kimjongha/Documents/rstudio")
getwd()

pew <- read.csv("dataset/Omnibus_Oct_2012_Political_Video_csv.csv", sep = ",", header = TRUE)

class(pew)
names(pew)
str(pew)
summary(pew$age)

# descriptive statistics
descriptive.statistics <- function(myvariable) {
  myvar.length.missing.include <- length(myvariable)
  myvar.length.missing.exclude <- length(myvariable[!is.na(myvariable)])
  myvar.sum <- sum(myvariable, na.rm = TRUE)
  myvar.mean <- mean(myvariable, na.rm = TRUE)
  myvar.median <- median(myvariable, na.rm = TRUE)
  table.myvariable <- table(myvariable)
  myvar.mode <- as.numeric(names(table.myvariable)[which.max(table.myvariable)])
  myvariable.range.min <- range(myvariable, na.rm = TRUE)[1]
  myvariable.range.max <- range(myvariable, na.rm = TRUE)[2]
  myvariable.interquartile25 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[1]
  myvariable.interquartile75 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[2]
  myvariable.var <- var(myvariable, na.rm = TRUE)
  myvariable.sd <- sd(myvariable, na.rm = TRUE)
  descriptive.myvariable <- rbind(myvar.mean, myvar.median, myvar.mode, myvariable.range.min,
                                  myvariable.range.max, myvariable.interquartile25, myvariable.interquartile75,
                                  myvariable.var, myvariable.sd, myvar.sum, myvar.length.missing.exclude,
                                  myvar.length.missing.include)
  rownames(descriptive.myvariable) <- c('mean', 'meian', 'mode', 'min', 'max', '25%', '75%', 'var',
                                        'sd', 'sum', 'na.exc', 'na.inc')
  colnames(descriptive.myvariable) <- 'value'
  round(descriptive.myvariable, digit = 2)
}

descriptive.statistics(pew$age)

# 15 multivariate inference statistical analysis
# t-test
# one sample t-test
setwd("/Users/kimjongha/Documents/github/rstudio")
getwd()

tess <- read.csv("dataset/TESS2_089_Trawalter_Client.csv", sep = ",", header = TRUE)

class(tess)
str(tess)
names(tess)
summary(tess)

table(tess$XIDEO, useNA = 'always')
t.test(tess$XIDEO, mu = 3.934)
