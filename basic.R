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
getwd()
setwd("/Users/kimjongha/Documents/github/rstudio")

tess <- read.csv(file = "dataset/TESS2_089_Trawalter_Client.csv", sep = ",", header = TRUE)

class(tess)
str(tess)
names(tess)
summary(tess)

# one sample t-test

table(tess$XIDEO, useNA = 'always')
tess$XIDEOr <- tess$XIDEO

tess$XIDEOr[tess$XIDEO == -1] <- NA # miss values
table(tess$XIDEOr)

t.test(tess$XIDEOr, mu = 4)

# paired sample t-test
# tess$Q4, tess$Q8

table(tess$Q4, useNA = 'always')
table(tess$Q8, useNA = 'always')

tess$Q4r <- tess$Q4
tess$Q8r <- tess$Q8

tess$Q4r[tess$Q4 == -1] <- NA
tess$Q4r[tess$Q8 == -1] <- NA

mean(tess$Q4r, na.rm = TRUE) ; sd(tess$Q4r, na.rm = TRUE)
mean(tess$Q8r, na.rm = TRUE) ; sd(tess$Q8r, na.rm = TRUE)

t.test(tess$Q4r, tess$Q8r, paired = TRUE) # paired t.test

# independent sample t-test
# tess$Q8 "I stub my leg on a chair leg"
# Ho : Mfemale = Mmale
# Ha : Mfemale > Mmale
table(tess$Q8, useNA = "always")
tess$Q8r <- tess$Q8

tess$Q8r[tess$Q8 == -1] <- NA
table(tess$Q8r, useNA = "always")

table(tess$XTESS089, useNA = "always")
tess$other.female <- NA

tess$other.female[tess$XTESS089 >= 13] <- 1
tess$other.female[tess$XTESS089 <= 12] <- 0

# statistics mean and variance
aggregate(Q8r ~ other.female, data = tess, FUN = mean)
aggregate(Q8r ~ other.female, data = tess, FUN = var)

# Bartlett test of homogeneity of variance
bartlett.test(Q8r ~ other.female, data = tess)

t.test(Q8r ~ other.female, var.equal = FALSE, data = tess)

# Chi-square 
# data = pew
pew <- read.csv("dataset/February_2012_csv.csv", sep = ",", header = TRUE)
colnames(pew)

table(pew$intuse, useNA = "always")
table(pew$q1, useNA = "always")

pew$intuse.r <- pew$intuse
pew$life.quality <- pew$q1

pew$intuse.r[pew$intuse == 8 | pew$intuse == 9] <- NA
pew$life.quality[pew$q1 == 8 | pew$q1 == 9] <- NA

table(pew$intuse.r, pew$life.quality, useNA = "always")

chisq.test(table(pew$intuse.r, pew$life.quality))



