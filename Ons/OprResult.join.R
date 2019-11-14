# OprResult.join.R

getwd()
setwd("D:/rstudio/ons")

## inner join, two tables

## CntList.csv
CntList <- read.csv("CntList.csv", header=TRUE)
names(CntList)

## OprResult.csv
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
  select(CntId, CntTitle, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) #%>% arrange(CntId)

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

## 연도별 교육인원이 가장 많은 과정
# 2016년
Content_Year %>%
  filter(Year == 2016) %>%
  arrange(desc(EduSum)) %>%
  head(20)
  
# 2017년
Content_Year %>%
  filter(Year == 2017) %>%
  arrange(desc(EduSum)) %>%
  head(20)

# 2018년
Content_Year %>%
  filter(Year == 2018) %>%
  arrange(desc(EduSum)) %>%
  head(20)

# 2019년
Content_Year %>%
  filter(Year == 2019) %>%
  arrange(desc(EduSum)) %>%
  head(20)

## 연도별 수료인원이 가장 많은 과정
# 2016년
Content_Year %>%
  filter(Year == 2016) %>%
  arrange(desc(CompleteSum)) %>%
  head(20)

# 2017년
Content_Year %>%
  filter(Year == 2017) %>%
  arrange(desc(CompleteSum)) %>%
  head(20)

# 2018년
Content_Year %>%
  filter(Year == 2018) %>%
  arrange(desc(CompleteSum)) %>%
  head(20)

# 2019년
Content_Year %>%
  filter(Year == 2019) %>%
  arrange(desc(CompleteSum)) %>%
  head(20)

## ggplot, geom_bar()
Content_Year %>%
  filter(Year == 2019) %>%
  #subset(CompleteSum > 100)
  ggplot(aes(x=CntId, y=CompleteSum)) +
  geom_bar(stat="identity") +
  geom_boxplot(alpha=0.2, colour="maroon3") #theme_bw()

## ggplot (연습)
Content_Year %>%
  #filter(Year == 2019)
  #subset(CompleteSum > 100)
  ggplot(aes(reorder(x=rownames(CntId), y=CompleteSum, colour=factor(Year)))) +
  facet_wrap(~ Year) +
  geom_point()
  #geom_boxplot()
  #theme_bw()

Content_Year %>%
  filter(CompleteSum > 500) %>%
  ggplot(aes(x=reorder(CntId, CompleteSum), y=CompleteSum, label=CntId)) +
  #facet_wrap(~ Year) +
  geom_bar(stat="identity") +
  geom_text()

Content_Year %>%
  filter(Year == 2019) %>%
  filter(CompleteSum > 500) %>%
  ggplot(aes(x=reorder(CntId, CompleteSum), y=CompleteSum, label=CntId)) +
  #facet_wrap(~ Year) +
  geom_bar(stat="identity") +
  geom_text()

## 최대한 친절하게 쓴 R로 그래프 그리기(https://kuduz.tistory.com/1077)
# OprResult.join
OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  filter(Year == 2018) %>%
  filter(CompleteSum > 500) %>%
  ggplot(aes(x=reorder(CntId, CompleteSum), y=CompleteSum, label=CntId)) +
  #facet_wrap(~ Year) +
  geom_bar(stat="identity") +
  geom_text()


