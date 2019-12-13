#rm(list=ls())

# OprResult.join.R

getwd()
setwd("D:/rstudio/ons")

## load data
OprResult.join <- read.csv("OprResult.join.csv", header=TRUE)

# load library
library(dplyr)
library(ggplot2)

# summary()
str(OprResult.join)
names(OprResult.join)

Lifelong <- OprResult.join %>%
  filter(OprType.1 == "평생과정" & SubOT == "기수제")

# 수료율 (수료인원/수강인원)
round(sum(Lifelong$CompleteSum, na.rm = TRUE)/sum(Lifelong$EduSum, na.rm = TRUE), 3)

# 기수제별로 수료율
gisu <- aggregate(EduSum ~ Cardinal + OprStart, Lifelong, sum)
gisu

by_gisu <- aggregate(EduSum ~ Cardinal, gisu, sum)
by_gisu$gisu <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1, 2, 3, 4, 5, 6, 7, 8, 9)

# 정렬 다시하고
by_gisu <- by_gisu[c(order(by_gisu$gisu)), ]
by_gisu

plot(by_gisu$gisu, by_gisu$EduSum)
by_gisu_10000 <- with(by_gisu, by_gisu[EduSum > 10000, ])
points(by_gisu_10000$gisu, by_gisu_10000$EduSum, col = "red", pch = 19)

# 년월별 수강인원
# gisu$OprStart 날짜변수로 변경
by_Oprym <- aggregate(EduSum ~ OprStart, gisu, sum)
by_Oprym

by_Oprym$OprStart <- as.Date(by_Oprym$OprStart)
str(by_Oprym)
plot(by_Oprym)

# 운영시작일별 수강인원
by_Oprym %>%
  ggplot(aes(x=OprStart, y=EduSum)) +
  geom_point(size=2) +
  geom_text(aes(label=OprStart), vjust = -1.5, color = "maroon", size=2)

# 선 추가
by_Oprym %>%
  ggplot(aes(x=OprStart, y=EduSum)) +
  geom_point(size=2) +
  geom_text(aes(label=OprStart), vjust = -1.5, color = "maroon", size=2) +
  stat_smooth(method=loess, se=FALSE, colour = "black")
