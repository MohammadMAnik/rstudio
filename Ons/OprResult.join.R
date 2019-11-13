# OprResult.join.R

getwd()
setwd("D:/rstudio/ons")

## inner join, two tables

# CntList.csv
CntList <- read.csv("CntList.csv", header=TRUE)
names(CntList)

# OprResult.csv
OprResult <- read.csv("OprResult.csv", header=TRUE)
names(OprResult)

# sqldf()
install.packages("sqldf")
library(sqldf)

sqldf("select Title, Company, CompleteSum from OprResult where No == 1", row.names=TRUE)

# Inner Join
OprResult.join <- sqldf("select *
      from CntList
      inner join OprResult
      on CntList.CntId = OprResult.No")

write.csv(x=OprResult.join, file="OprResult.join.csv", row.names=FALSE)

## load data
OprResult.join <- read.csv("OprResult.join.csv", header=TRUE)

# load library
library(dplyr)
library(ggplot2)

# summary()
str(OprResult.join)

# 수료율 (수료인원/교육인원)
comp <- sum(OprResult.join$CompleteSum, na.rm = TRUE)
edu <- sum(OprResult.join$EduSum, na.rm = TRUE)
round(comp/edu*100, 1)
# 38.1%

# 연도별 수료율
OprResult.join %>%
  select(Year, EduSum, CompleteSum) %>%
  group_by(Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(Year)

# 과정별 수료인원
Content <- OprResult.join %>%
  select(CntId, EduSum, CompleteSum) %>%
  group_by(CntId) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(CntId)

ggplot(Content, aes(CntId, Rate)) +
  geom_point(aes(size=EduSum), alpha=1/3) +
  geom_smooth()

# 과정별, 연도별, 운영실적
Content_Year <- OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(CntId)

# Correlation Positive > .70
Upper_70 <- Content_Year %>%
  group_by(CntId) %>%
  summarise(Cor = cor(Year, EduSum)) %>%
  filter(Cor > 0.7)

# Inner Join (sqldf 설치, 로딩 다시 해야 함)
Upper_70.join <- sqldf("select *
      from Upper_70
      inner join Content_Year
      on Upper_70.CntId = Content_Year.CntId")

# graph
ggplot(Upper_70.join, aes(Year, EduSum)) +
  geom_line() +
  facet_wrap(~ CntId)





