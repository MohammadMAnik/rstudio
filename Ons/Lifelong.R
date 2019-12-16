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

# 기수별 교육인원
GisuES <- aggregate(EduSum ~ Cardinal + OprStart, Lifelong, sum)
GisuCS <- aggregate(CompleteSum ~ Cardinal + OprStart, Lifelong, sum)

write.csv(GisuES, file="GisuES.csv")
write.csv(GisuCS, file="GisuCS.csv")
#엑셀에서 편집해서, Opryymm.csv 파일로 저장함

# 저장한 파일 불러오기
Opryymm <- read.csv("Opryymm.csv", header=TRUE)
Opryymm

# 수료율 추가
Opryymm$CompletRate <- round(Opryymm$CompleteSum/Opryymm$EduSum, 3)*100
Opryymm

# 그래프
plot(Opryymm$OprStart, Opryymm$EduSum)
lines(Opryymm$OprStart, Opryymm$EduSum)
plot(Opryymm$OprStart, Opryymm$CompleteSum)
lines(Opryymm$OprStart, Opryymm$CompleteSum)

# 그래프
ggplot(Opryymm, aes(x=OprStart, y=EduSum)) +
  geom_bar(stat="identity", fill="cyan2") +
  theme(axis.text.x=element_text(angle=90)) +
  geom_text(aes(label=CompletRate), vjust=-.5, colour="black",
            position=position_dodge(.9), size=3)

# 과정별 수료인원
TitleES <- aggregate(EduSum ~ Title, Lifelong, sum)
TitleCS <- aggregate(CompleteSum ~ Title, Lifelong, sum)
TitleES
TitleCS

TitleEC <- TitleES %>% inner_join(TitleCS, by="Title")
TitleEC
TitleEC$CompleteRate <- round(TitleEC$CompleteSum/TitleEC$EduSum, 3)*100

# 그래프
TitleEC %>%
  arrange(desc(EduSum)) %>%
  head(20) %>%
  ggplot(aes(x=reorder(Title, EduSum), y=EduSum)) +
  geom_bar(stat="identity", fill="cyan2") +
  theme(axis.text.x=element_text(angle=90)) +
  coord_flip() +
  geom_text(aes(label=EduSum), vjust=0.3, hjust=1.5, colour="black",
            position=position_dodge(.9), size=4) +
  geom_text(aes(label=CompleteRate), vjust=1, colour="blue",
            position=position_dodge(.9), size=3)

# 정렬
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


###############################################################################
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

## 개발연도에 따른 연도별 교육인원
OprResult.join %>%
  ggplot(aes(x=CntId, y=EduSum)) +
  geom_point(aes(colour=factor(DevYear)))

## 개발연도에 따른 연도별 수료인원
OprResult.join %>%
  ggplot(aes(x=CntId, y=CompleteSum)) +
  geom_point(aes(colour=factor(DevYear)))

## 최대한 친절하게 쓴 R로 그래프 그리기(https://kuduz.tistory.com/1077)
## 연도별 상위 20과정 (수료인원, 예시)
OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  filter(!is.na(CompleteSum)) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  #mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  filter(Year == 2016) %>%
  head(10) %>%
  #filter(CompleteSum > 500) %>%
  ggplot(aes(x=reorder(CntId, CompleteSum), y=CompleteSum, label=CntId)) +
  #facet_wrap(~ Year) +
  geom_point() + #aes(colour=factor(CntId)))
  #geom_bar(stat="identity") +
  geom_text()

# 연도별 교육인원 상위 20과정 (2016년)
OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  filter(Year == 2016) %>%
  arrange(desc(EduSum)) %>%
  head(20) %>%
  ggplot(aes(x=reorder(CntId, -EduSum), y=EduSum, label=CntId)) +
  geom_bar(stat="identity")

# 연도별 수료인원 상위 20과정 (2016년)
OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  filter(Year == 2016) %>%
  arrange(desc(CompleteSum)) %>%
  head(20) %>%
  ggplot(aes(x=reorder(CntId, -CompleteSum), y=CompleteSum, label=CntId)) +
  geom_bar(stat="identity")

# 평생과정 수료인원
lifelong <- OprResult.join %>%
  filter(OprType == "평생과정") %>%
  select(CntId, Year, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

lifelong %>%
  filter(CompleteSum > 500) %>%
  ggplot(aes(x=Year, y=CompleteSum)) + 
  facet_wrap(~ CntId) +
  geom_bar(stat="identity")

ggplot(lifelong, aes(x=CntId, y=CompleteSum)) +
  facet_wrap(~ Year) +
  geom_bar(postion="dodge", stat="identity")

# 평생과정 > 기업맞춤 > 삼성협력사 수료인원
OprResult.join %>%
  filter(OprType == "평생과정") %>%
  filter(SubOT == "기업맞춤") %>%
  filter(Company == "삼성협력사") %>%
  select(CntId, CompleteSum) %>%
  group_by(CntId) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  arrange(desc(CompleteSum)) %>%
  head(20) %>%
  ggplot(aes(x=reorder(CntId, CompleteSum), y=CompleteSum, label=CntId)) +
  geom_bar(stat="identity")

## 특정분야 등등
ggplot(subset(OprResult.join), aes(x=CntId, y=CompleteSum)) +
  facet_wrap(~ Cardinal) +
  geom_point(aes(colour=factor(DevYear)))

## 상관관계 (교육인원-수료인원)
# 상관계수
Corr_EC <- OprResult.join %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

with(Corr_EC, plot(CompleteSum, EduSum))

with(Corr_EC, cor(CompleteSum, EduSum, method="pearson"))
with(Corr_EC, cor(CompleteSum, EduSum, method="kendall"))
with(Corr_EC, cor(CompleteSum, EduSum, method="spearman"))

with(Corr_EC, cor.test(CompleteSum, EduSum, method="pearson"))

# 상관분석 시각화
library(psych)
pairs.panels(Corr_EC)

## 단순회귀분석
lm.out <- lm(Corr_EC$CompleteSum ~ Corr_EC$EduSum)
summary(lm.out)
anova(lm.out)
plot(Corr_EC$CompleteSum ~ Corr_EC$EduSum)
abline(lm.out, col="red")
par(mfrow=c(2,2))
plot(lm.out)

lm.out$fitted[144]
lm.out$residuals[144]

par(mfrow=c(1,1))
plot(cooks.distance(lm.out))

## 다중회귀분석
Model <- lm(CompleteSum ~ Year + EduSum, data=Corr_EC)
summary(Model)

Model.step <- step(Model, direction="backward")
summary(Model.step)
confint(Model.step)
predict(Model.step, list(Year=2019, EduSum=1000))

par(mfrow=c(2,2))
plot(Model.step)

## 정규성 검정
par(mfrow=c(1,1))
qqnorm(Corr_EC$EduSum)
qqline(Corr_EC$EduSum)

