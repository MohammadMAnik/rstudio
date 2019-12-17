
#rm(list = ls())
#rm(oleiportal_04)
rm(mydata)

# packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("car")

# load packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(car)

# path setting
getwd()
setwd("E:/OLEI data/R Analysis/oleiportal/grade(200001-284734)")

# load file
oleiportal_04 <- read.csv(file="oleiportal_04.csv", stringsAsFactors = FALSE)
head(oleiportal_04)
names(oleiportal_04)
str(oleiportal_04)

mydata <- oleiportal_04
rm(oleiportal_04)

# descriptive statistics
class(mydata)
dim(mydata)
str(mydata)
head(mydata, 10)
tail(mydata, 10)
tbl_df(mydata)
ls.str(mydata)
summary(mydata)
glimpse(mydata)
View(mydata)

# pid
mydata$pid <- seq(1:190733)

# rename
mydata <- rename(mydata, birth = birth_re)
mydata <- rename(mydata, start = start_re)
mydata <- rename(mydata, home = home_addr_re)
mydata <- rename(mydata, end   = end_re)
mydata <- rename(mydata, accum = accum_sec)
mydata <- rename(mydata, progress = prog_rate_re)
str(mydata)

# gend -> refine
mydata$gend[mydata$gend == '기타'] <- NA
mydata$gend[mydata$gend == '정보없음'] <- NA
mydata$gend2 <- mydata$gend
mydata$gend2[mydata$gend2 == '남자'] <- 1
mydata$gend2[mydata$gend2 == '여자'] <- 2

# emply -> refine
mydata$emply[mydata$emply == '기타'] <- NA
mydata$emply2 <- mydata$emply
mydata$emply2[mydata$emply2 == '구직자'] <- 1
mydata$emply2[mydata$emply2 == '재직자'] <- 2

# save file
#write.csv(mydata, file = "oleiportal_05.csv")

# load file
#mydata <- read.csv(file = "oleiportal_05.csv")

# x.1 delete
#mydata <- mydata[, -c(1)]

# levels
levels(mydata$gend)
levels(mydata$emply)
table(mydata$birth)
levels(mydata$home)
levels(mydata$gisu)
levels(mydata$company)

# duplicate
mydata_01 <- mydata[-which(duplicated(mydata$id)),]
mydata_01$pid2 <- seq(1:60926)
count(mydata_01, vars = ("id"))

# gend
table(mydata_01$gend)
mydata_01$gend[mydata_01$gend == '기타'] <- NA
mydata_01$gend[mydata_01$gend == '정보없음'] <- NA
barplot(table(mydata_01$gend))
barplot(table(mydata_01$gend, useNA = 'always'))
barplot(table(mydata_01$gend, useNA = 'always'), main = 'oleiportal gender')
barplot(table(mydata_01$gend, useNA = 'always'), main = 'oleiportal gender',
        col = 'lightblue', border = NA, axes = FALSE)
barplot(table(mydata_01$gend, useNA = 'always'), main = 'oleiportal gender',
        col = 'lightblue', border = NA, ylim = c(0, 50000))
barplot(table(mydata_01$gend, useNA = 'always'), main = 'oleiportal gender',
        col = 'lightblue', border = NA, ylim = c(0, 50000))
gender <- table(mydata_01$gend)
barplot(gender)
text(gender, labels = gender)

mydata_01$gend2 <- mydata_01$gend
mydata_01$gend2[mydata_01$gend2 == '남자'] <- 1
mydata_01$gend2[mydata_01$gend2 == '여자'] <- 2
str(mydata_01$gend2)

ggplot(data = mydata_01) +
  geom_bar(mapping = aes(x = gend2, fill = gend2))

# employment
table(mydata_01$emply)
mydata_01$emply[mydata_01$emply == '기타'] <- NA
barplot(table(mydata_01$emply))
barplot(table(mydata_01$emply, useNA = 'always'))

mydata_01$emply2 <- mydata_01$emply
mydata_01$emply2[mydata_01$emply2 == '구직자'] <- 1
mydata_01$emply2[mydata_01$emply2 == '재직자'] <- 2
str(mydata_01$emply2)

# age
plot(birth~pid, data = mydata_01, ylim = c(1960, 2000))
mydata_01$age[mydata_01$birth >= 2000] <- 10
mydata_01$age[mydata_01$birth < 2000 & mydata_01$birth >= 1990] <- 20
mydata_01$age[mydata_01$birth < 1990 & mydata_01$birth >= 1980] <- 30
mydata_01$age[mydata_01$birth < 1980 & mydata_01$birth >= 1970] <- 40
mydata_01$age[mydata_01$birth < 1970 & mydata_01$birth >= 1960] <- 50
mydata_01$age[mydata_01$birth < 1960] <- 60
table(mydata_01$age)

barplot(table(mydata_01$age))
barplot(sort(table(mydata_01$age), decreasing = TRUE))

mydata_01$birth2 <- as.character(mydata_01$birth)

ggplot(data = mydata_01, mapping = aes(x = pid2, y = birth2)) + 
  geom_point(mapping = aes(color = birth2))

# region
table(mydata_01$home)
mydata_01$home[mydata_01$home == '01'] <- NA
mydata_01$home[mydata_01$home == '73'] <- NA
mydata_01$home[mydata_01$home == '강북'] <- '서울'
barplot(table(mydata_01$home))
barplot(table(mydata_01$home, useNA = 'always'))
barplot(sort(table(mydata_01$home), decreasing = TRUE))
barplot(sort(table(mydata_01$home, useNA = 'always'), decreasing = TRUE))

ggplot(data = mydata_01, mapping = aes(x = pid2, y = home)) + 
  geom_point(mapping = aes(color = home))

# gisu
table(mydata_01$gisu)
barplot(table(mydata_01$gisu))
barplot(table(mydata_01$gisu, useNA = 'always'))

# course
table(mydata_01$course)
course_cnt <- table(mydata_01$course)
course_cnt_fq <- sort(course_cnt, decreasing = TRUE)
View(course_cnt_fq)
barplot(course_cnt_fq)
hist(course_cnt_fq)# breaks = 10000, xlim = c(0, 100))

# lesson
lesson_cnt <- table(mydata_01$lesson)
barplot(lesson_cnt)
plot(lesson~pid, data = mydata_01)
plot(lesson~pid, data = mydata_01, xlim = c(35000, 60000), ylim = c(5, 30))

# accum
table(mydata_01$accum)
View(mydata_01$accum)
plot(mydata_01$accum)
qplot(mydata_01$accum)
plot(mydata_01$accum, ylim = c(0, 3600)) # 1 hour

# progress
summary(mydata_01$progress)
mean(mydata_01$progress)
table(mydata_01$progress)

plot(mydata_01$progress)
boxplot(mydata_01$progress)
qplot(mydata_01$progress)

hist(mydata_01$progress)
hist(mydata_01$progress, breaks = 100)
axis(1, at = 10*(1:10))

boxplot(mydata_01$progress~mydata_01$gisu)
plot(mydata_01$accum ~ mydata_01$progress)
plot(mydata$accum ~ mydata$progress)

progress100 <- mydata[which(mydata$progress == 100), ]
head(progress100, 10)
str(progress100)

plot(progress100$accum)
plot(progress100$accum, ylim = c(0, 600))
progress100$accum[progress100$accum < 3600]

woman <- mydata[which(mydata$gend == '여자'), ]
man <- mydata[which(mydata$gend == '남자'), ]
plot(woman$progress)
plot(man$progress)

gisu <- mydata[which(mydata$gisu == '기수제'), ]
gisu_wm <- gisu[which(gisu$gend == '여자'), ]
plot(gisu_wm$progress)

gisu_m <- gisu[which(gisu$gend == '남자'), ]
plot(gisu_m$progress)

ggplot(data = mydata, mapping = aes(x = pid, y = progress)) +
  geom_point(mapping = aes(colour = gend2))

ggplot(data = mydata, mapping = aes(x = pid, y = progress)) +
  geom_point(mapping = aes(colour = emply2))

# company
table(mydata_01$company)
company_cnt <- table(mydata_01$company)
company_cnt_fq <- sort(company_cnt, decreasing = TRUE)
View(company_cnt_fq)
hist(company_cnt_fq)# breaks = 10000, xlim = c(0, 100))
