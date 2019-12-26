# OprResult.join.filter.lifelong.cardinal.R

getwd()
setwd("D://rstudio/ons")

# load data
OprResult.join <- read.csv("OprResult.join.csv", header=TRUE)
summary(OprResult.join)
names(OprResult.join)

# package
search()
library(dplyr)
library(ggplot2)

# lifelong, cardinal, filter
Lifelong.Ca <- OprResult.join %>%
  filter(OprType == "평생과정" & SubOT == "기수제")

head(Lifelong.Ca)
summary(Lifelong.Ca)

# aggregate
LC.aggregate <- aggregate(cbind(EduSum, CompleteSum) ~ CntTitle + NCS + SubOT +
                     Cardinal + OprStart + OprEnd, Lifelong.Ca, FUN=sum)

head(LC.aggregate)

# mutate CompleteRate
LC.aggregate  <- mutate(LC.aggregate, CompleteRate=round(CompleteSum/EduSum*100, 1))

# save file
write.csv(LC.aggregate, file="LC.aggregate.csv")

# CntTitle
LC.aggre.title <- aggregate(cbind(EduSum, CompleteSum) ~ CntTitle, 
                            Lifelong.Ca, FUN=sum)

# filter, EduSum > 0
LC.aggre.title <- LC.aggre.title %>%
  filter(EduSum > 0) %>%
  mutate(CompleteRate=round(CompleteSum/EduSum*100, 1))

# save
write.csv(LC.aggre.title, file="LC.aggre.title.csv")

# load data
LC.aggre.title <- read.csv("LC.aggre.title.csv", header=TRUE)

# EduSum, over1000, analysis
ESover1000 <- LC.aggre.title %>%
  arrange(desc(EduSum)) %>%
  filter(EduSum > 1000)

ESover1000

# EduSum, ESover1000, arrange, ggplot, CompleteSum > 1000
ESover1000 %>%
  filter(EduSum > 1000) %>%
  arrange(desc(EduSum)) %>%
  ggplot(aes(x=reorder(CntTitle, EduSum), y=EduSum)) +
  theme(axis.text.x=element_text(angle=90)) +
  coord_flip() +
  geom_bar(stat="identity", fill="steelblue2") +
  geom_bar(data=ESover1000[ESover1000$CompleteSum >= 1000, ],
           aes(x=CntTitle, y=EduSum), fill='tomato1', stat='identity') +
  geom_text(aes(label=EduSum), vjust=0.3, hjust=1.5, colour="white",
            position=position_dodge(.9), size=3.5) + 
  geom_text(aes(label=CompleteRate), vjust=1, colour="gray20",
            position=position_dodge(.9), size=3)

# CompleteRate, arrange desc
ESover1000 %>%
  filter(EduSum > 1000) %>%
  arrange(desc(CompleteRate)) %>%
  ggplot(aes(x=reorder(CntTitle, CompleteRate), y=EduSum)) +
  theme(axis.text.x=element_text(angle=90)) +
  coord_flip() +
  geom_bar(stat="identity", fill="steelblue2") +
  geom_bar(data=ESover1000[ESover1000$CompleteRate >= 40, ],
           aes(x=CntTitle, y=EduSum), fill='tomato1', stat='identity') +
  geom_bar(data=ESover1000[ESover1000$EduSum >= 3000, ],
           aes(x=CntTitle, y=EduSum), fill='mediumpurple1', stat='identity') +
  geom_text(aes(label=EduSum), vjust=0.3, hjust=1.5, colour="white",
            position=position_dodge(.9), size=3.5) + 
  geom_text(aes(label=CompleteRate), vjust=1, colour="gray20",
            position=position_dodge(.9), size=3)

# EduSum, filter < 100, arrange, ggplot, CompleteSum > 1000
LC.aggre.title %>%
  filter(EduSum < 50) %>%
  arrange(desc(EduSum)) %>%
  ggplot(aes(x=reorder(CntTitle, EduSum), y=EduSum)) +
  theme(axis.text.x=element_text(angle=90)) +
  coord_flip() +
  geom_bar(stat="identity", fill="steelblue2") +
  geom_text(aes(label=EduSum), vjust=0.3, hjust=2, colour="white",
            position=position_dodge(.9), size=3.2) + 
  geom_text(aes(label=CompleteRate), vjust=0.5, colour="gray20",
            position=position_dodge(.9), size=3)

# as.list
EduSum <- ESover1000$EduSum
CompleteSum <- ESover1000$CompleteSum

ECRatio <- rbind(EduSum, CompleteSum)
ECRatio

# beside=TRUE, green=EduSum, blue=CompleteSum
barplot(ECRatio,
        beside=TRUE,
        ylim=c(0, 5000),
        col=c("lightgreen", "blue"),
        names.arg=ESover1000$CntTitle,
        legend=TRUE,
        main="Lifelong Cardinal ESover 1000 CompleteSum/EduSum",
        las=2,
        cex.names=0.8,
        font=1) #bold 2

# CompleteRate
plot(ESover1000$CompleteRate, ylim=c(0, 100),
     main="CompleteRate",
     xlab="")
lines(ESover1000$CompleteRate, ylim=c(0, 100))
a <- seq(1:34)
b <- ESover1000$CompleteRate
Rate <- data.frame(a, b)
text(a, b, labels=b, data=Rate, pos=3, cex=0.8)

# over 40%, blue
faithful <- ESover1000$CompleteRate
faith <- data.frame(a, faithful)
over40 <- with(faith, faith[faithful >= 40,])
plot(faith)
points(over40, col="blue", pch=19)

# barplot
barplot(ESover1000$CompleteRate,
        ylim=c(0,100),
        names.arg=ESover1000$CntTitle,
        las=2,
        cex.names=0.8)

# 연도별 수료율
OprResult.join %>%
  filter(OprType == "평생과정" & SubOT == "기수제") %>%
  select(Year, EduSum, CompleteSum) %>%
  group_by(Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(Year)

# 정렬
byGisu <- OprResult.join %>%
          filter(OprType == "평생과정" & SubOT == "기수제")

byGisu <- aggregate(cbind(EduSum, CompleteSum) ~ OprStart + Cardinal,
                    data=byGisu, FUN=sum)
byGisu
byGisu <- byGisu %>% arrange(OprStart)

# 년월별 수강인원
# gisu$OprStart 날짜변수로 변경
byOprym <- aggregate(EduSum ~ OprStart, byGisu, sum)
byOprym$OprStart <- as.Date(by_Oprym$OprStart)
plot(byOprym)
plot.ts(byOprym$EduSum)

# 운영시작일별 수강인원
byOprym %>%
  ggplot(aes(x=OprStart, y=EduSum)) +
  geom_point(size=2) + 
  geom_text(aes(label=OprStart), vjust = -1.5, color = "steelblue2", size=2)

# CntId, EduSum, CompleteSum, CompleteRate
Content <- OprResult.join %>%
  filter(OprType == "평생과정" & SubOT == "기수제") %>%
  filter(EduSum != 0 & CompleteSum != 0) %>%
  select(CntId, EduSum, CompleteSum) %>%
  group_by(CntId) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(CntId)

# rate mis calculate
Content$Rate <- NULL

# re calculate
Content$Rate <- round(Content$CompleteSum/Content$EduSum*100, 1)
Content

# by content, CompleteRate, color=EduSum, size=EduSum, geom_smooth
Content %>%
  ggplot(aes(x=CntId, y=Rate, color=EduSum)) +
  scale_color_gradient(low="steelblue1", high="violetred1") +
  geom_point(aes(size=EduSum), alpha=1/1) +
  geom_smooth()

# CntId, Year, EduSum, CompleteSum, CompleteRate
OprYear <- OprResult.join %>%
  filter(OprType == "평생과정" & SubOT == "기수제") %>%
  filter(EduSum != 0 & CompleteSum != 0) %>%
  select(CntId, Year, EduSum, CompleteSum) %>%
  group_by(CntId, Year) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(Rate = round(CompleteSum/EduSum*100, 1)) %>%
  arrange(CntId)

OprYear$CntId <- as.factor(OprYear$CntId)

un2019 <- OprYear %>%
             filter(Year < 2019)

un2019id <- un2019 %>%
            select(CntId, Year, EduSum)

write.csv(un2019id, file="un2019id.csv", row.names=FALSE)

# in Excel, countif, CntId > 3, edit, save
# load file
un2019.3year <- read.csv("un2019id.3year.csv", header=TRUE)
un2019.3year

un2019.3over <- un2019.3year %>%
                filter(Count == 3)

un2019.3over
uni.CntId <- unique(un2019.3over$CntId)

mx.EduSum <- un2019.3over$EduSum
mx.EduSum

EduSum.mx <- matrix(mx.EduSum, ncol=3)
Cagr <- data.frame(EduSum.mx)
names(Cagr) <- c("2016", "2017", "2018")
Cagr$CntId <- uni.CntId
Cagr

# in Excel, calculate, CAGR, save
# load file
CAGR <- read.csv("OprResult.lifelong.CAGR.csv", header=TRUE)
CAGR

CAGR$EduSum.3year <- CAGR$X2016+CAGR$X2017+CAGR$X2018

# inner join, CntId, CntTitle
library(sqldf)

# test, drv="SQLite"
sqldf("select * from iris limit 5", drv="SQLite")

# loda file
CntList <- read.csv("CntList.csv", header=TRUE)

# Inner Join
CAGR_join <- sqldf("select *
      from CntList
      inner join CAGR
      on CntList.CntId = CAGR.CntId", drv="SQLite")

# join confirm
sqldf("select * from CAGR_join", drv="SQLite", row.names=TRUE)

# write
write.csv(CAGR_join, file="CAGR_join.csv", row.names=FALSE)

# load file
CAGR_join <- read.csv("CAGR_join.csv", header=TRUE)

CAGR_join %>%
  filter(CAGR2 > 0) %>%
  ggplot(aes(x=factor(CntId), y=CAGR2*100)) +
  geom_bar(stat="identity", fill="steelblue2") +
  geom_text(aes(label=EduSum.3year), vjust=-1, hjust=0.5, colour="black",
            position=position_dodge(.9), size=3.1) + 
  geom_text(aes(label=CAGR2), vjust=1.2, hjust=0.5, colour="black",
            position=position_dodge(.9), size=3)

# CntList
CAGR_positive <- CAGR_join %>%
  filter(CAGR2 > 0) %>%
  select(CntId, CntTitle, EduSum.3year, CAGR2)

# save
write.csv(CAGR_positive, file="CAGR_positive.csv", row.names=FALSE)

# CntList, arrange
CAGR_positive_order <- CAGR_join %>%
  filter(CAGR2 > 0) %>%
  arrange(desc(EduSum.3year)) %>%
  select(CntId, CntTitle, EduSum.3year, CAGR2)

# save
write.csv(CAGR_positive_order, file="CAGR_positive_order.csv", row.names=FALSE)

# positive, negative, TF
CAGR_join$pos <- CAGR_join$CAGR2 >= 0 #TRUE, FALSE
CAGR_join

# graph
ggplot(CAGR_join, aes(x=factor(CntId), y=CAGR2, fill=pos)) +
  geom_bar(stat="identity", position="identity") +
  theme(axis.text.x=element_blank())




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

