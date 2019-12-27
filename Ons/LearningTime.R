#learning time analysis
#rm(list=ls())

getwd()
setwd("D:/rstudio/ons")

search()
system.time(search())

install.packages("data.table")
library(data.table)
library(ggplot2)
library(dplyr)

LearningTime <- fread("LearningTime.csv") #too much data, use fread

dim(LearningTime)
# 394204  18

summary(LearningTime)
head(LearningTime, 10)
names(LearningTime)

# column name change
names(LearningTime) <- c("V1", "id", "name", "gender", "empYN","company",
                         "birthdate", "address", "title", "startDate", "endDate",
                         "learningTime", "certificate", "eduSum", "compSum",
                         "learningTimeSeconds", "hhmmss", "progress",
                         "compStatus", "idx")

LearningTime$V1 <- NULL

# 시리얼번호 추가
idx <- seq(1:394204)
LearningTime$idx <- idx

# save
write.csv(LearningTime, file = "LearningTime2.csv", row.names=FALSE)

# re-load file
LearningTime2 <- fread("LearningTime2.csv")
names(LearningTime2)

LearningTime <- LearningTime2

# 전체 수료여부
table(LearningTime$compStatus)
#1      2      3      4      5 
#58994 182482   5294 147432      2
#2 수료

# table 결과를 그래프로 그리기
barplot(table(LearningTime$compStatus), type = "b", lwd = 3)

# table, progress
prgRate <- table(LearningTime$progress)
prgRate <- data.table(prgRate)
prgRate

# save file
write.csv(prgRate, file="proRate.csv", row.names=FALSE)

# progress rate
plot(LearningTime$progress,
     pch = 20,
     cex = 0.1)

# dealing with overplotting, alpha
ggplot(LearningTime, aes(idx, progress)) +
  geom_point(shape=20, alpha=1/3)

# white space
LearningTime %>%
  filter(idx > 150000 & idx < 200000) %>%
  ggplot(aes(x=idx, y=progress)) +
  geom_point(shape=20, alpha=1/3)

# idx 17000 ~ 182000
idxWS <- LearningTime %>%
  filter(idx > 170000 & idx < 182000)

# idxWS, title count
idxWS.title <- idxWS %>%
  group_by(title) %>%
  summarise(N=n()) %>%
  arrange(desc(N))

# idxWS, count progress
idsWS.prog <- LearningTime %>%
  filter(idx > 170000 & idx < 182000) %>%
  group_by(progress) %>%
  summarise(N=n())

apply(idsWS.prog[1, ], 2, sum)
apply(idsWS.prog[2:209, ], 2, sum)
apply(idsWS.prog[210, ], 2, sum)

x <- c("0%", "0.1%~99.9%", "100%")
y <- c(6431, 391, 5177)
idsWS.freq <- data.frame(x, y)
idsWS.freq
barplot(idsWS.freq$y, ylim=c(0, 7000))

# histogram
hist(LearningTime$progress)

# compStatus == 2, progress distribution
CS2.prog.dist <- LearningTime %>%
  filter(compStatus == 2) %>%
  select(idx, id, title, progress, learningTimeSeconds)

dim(CS2.prog.dist)

CS2.prog.dist$mm <- as.numeric(round(CS2.prog.dist$learningTimeSeconds/60, 1))
CS2.prog.dist$hh <- as.numeric(round(CS2.prog.dist$learningTimeSeconds/3600, 1))
head(CS2.prog.dist, 10)

hist(CS2.prog.dist$mm, breaks=1000, xlim=c(0, 10000))
hist(CS2.prog.dist$hh, breaks=2000, xlim=c(0, 60), ylim=c(0, 2000))

# over 60min, learner
CS2.prog.dist[CS2.prog.dist$hh > 60, ]

CS2.prog.dist %>%
  filter(hh >= 60) %>%
  group_by(id) %>%
  summarise(N=n(), AvglearningTime=mean(hh)) %>%
  arrange(desc(AvglearningTime)) %>%
  mutate(learningTime1 = AvglearningTime/N) %>%
  filter(N >= 3) -> goodLearner

library(stringr)
koreatech <- str_detect(string=goodLearner$id, pattern='koreatech')
goodLearner[koreatech, c("id", "N")]


# join, learner, information

  
# gender
ggplot(LearningTime, aes(x=idx, y=progress)) +
  geom_point(aes(colour = gender_code)) +
  facet_wrap(~ gender_code)

# complete


# progress 40%~60%
prog50 <- LearningTime %>%
  filter(progress == 50) %>%
  select(service_title, name, company_name, start_date, progress) %>%
  arrange(service_title)

# by company_name
ggplot(g, aes(x=idx, y=progress)) +
  geom_point(aes(colour= company_name))

# company_10
company_10 <- LearningTime %>%
  filter(company_name == "삼성디스플레이" |
           company_name == "한국철도공사" |
           company_name == "베스트윈" |
           company_name == "마산대학교" |
           company_name == "동부대우전자" |
           company_name == "네패스" |
           company_name == "유라코퍼레이션" |
           company_name == "ABB코리아" |
           company_name == "세메스" |
           company_name == "한국기술교육대학교")

# company_10
ggplot(company_10, aes(x=idx, y=progress)) +
  geom_point(aes(colour = company_name)) +
  facet_wrap(~ company_name)

ggplot(company_10, aes(x=progress)) +
  geom_histogram()

# 진도율 히스토그램
company_10 %>% 
  ggplot(aes(progress)) +
  geom_histogram()

# 진도율이 0%이상 100%이하인 학습자의 히스토그램
company_10 %>% 
  filter(progress > 0 & progress < 100) %>%
  ggplot(aes(progress)) +
  geom_histogram()

# 마산대학교  
masan <- company_10 %>% filter(company_name == "마산대학교")
g <- ggplot(masan, aes(x=idx, y=progress))

# by gender
g + geom_point(aes(colour = gender_code))
  
ggplot(masan, aes(x=progress)) + geom_histogram()







# by gender
gender %>%
  ggplot(aes(x=idx, y=progress, colour=gender_code)) +
  geom_point(size=0.5) #size=2(defalut)

# 
gender %>%
  ggplot(aes(x=idx, y=progress, colour=gender_code)) +
  geom_point(size=1) #size=2(defalut)

# progress count
table(LearningTime$progress)

# histogram
hist(progress,
     main = "진도율 히스토그램",
     xlab = "진도율",
     ylab = "빈도수",
     col = "maroon")

# Kernel density plot
plot(density(LearningTime$progress))

# 밀도그래프에 Filled density plot
polygon(density(LearningTime$progress), col = "blue", border = "blue")

# 100%, 0%만으로 plot 그리기
progress_ex <- LearningTime %>%
  select(idx, progress) %>%
  filter(progress == 0 | progress == 100)

plot(progress_ex)

# 0 < data < 100
progress_in <- LearningTime %>%
  select(idx, progress) %>%
  filter(progress > 0 & progress < 100)

plot(progress_in)

progress_re <- LearningTime %>%
  select(idx, progress) %>%
  filter(progress > 0 | progress == 100)

plot(progress_re)

# 진도율 0% 개수
LearningTime %>%
  select(idx, progress) %>%
  filter(progress == 0) %>% tally()

LearningTime %>%
  select(idx, progress) %>%
  filter(progress == 100) %>% tally()

LearningTime %>%
  select(idx, progress) %>%
  filter(progress > 0 & progress < 100) %>% tally()

# 진도율 구간 나누기
LearningTime$progress_rg <- 
  ifelse(LearningTime$progress == 100, 100,
         ifelse(LearningTime$progress >=  80, 80,
                ifelse(LearningTime$progress >=  60, 60,
                       ifelse(LearningTime$progress >=  20, 20,
                              ifelse(LearningTime$progress >=  10, 10,
                                     ifelse(LearningTime$progress == 0, 0, 1))))))

barplot(table(LearningTime$progress_rg))

# 남녀
table(LearningTime$gender_code)
#1      2      3   NULL 
#215480  68420   1856   1653

barplot(table(LearningTime$gender_code), ylim=c(0, 300000))

# 재직여부
table(LearningTime$employment_status_code)
#10    11    20    21    22    23    29  NULL 
#78546 32979 47485 49104 26630 43708  3836  5121

# 동일인 중복 제거
duplicated_rm <- LearningTime[-which(duplicated(LearningTime$name)), ]
dim(duplicated_rm)
# 52155    20

# 중복제거 후 남녀
table(duplicated_rm$gender_code)
#1     2     3  NULL 
#38625 12979   352   199 

# 중복제거 후 재직여부
table(duplicated_rm$employment_status_code)
#10    11    20    21    22    23    29  NULL 
#13904  6106  7536  8890  5339  9041   424   915

# 중복제거 후 아이디 분석(문자형 벡터 X를 split 기준으로 나누기)
email <- duplicated_rm$id

install.packages("stringr")
library(stringr)

#행렬로 추출(str_split_fixed)
email_split <- str_split_fixed(email, "@", 2)
email_last <- email_split[,2]
write.csv(head(sort(table(email_last), decreasing=TRUE), 30), file="email_last.csv")
#naver.com         hanmail.net           gmail.com          kopo.ac.kr            nate.com 
#23178                4579                4271                2190                1993 
#samsung.com            daum.net     koreatech.ac.kr                              korail.com 
#1845                1244                1238                 859                 533 

# 대학교 이메일 주소 갯수
table(endsWith(email_last, 'ac.kr'))
#FALSE  TRUE 
#47513  4642




# 1초라도 학습한 학습자 (진도율이 0%를 초과한 학습자)
learner <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0)

dim(learner)
#287409

str(learner)
summary(learner)
View(learner)

# 과정명 기준으로 중복 제거
distinct_title <- learner[-which(duplicated(learner$service_title)),]
distinct_title

dim(distinct_title)
#1496 18

str(distinct_title)
mode(distinct_title)

chasi <- as.numeric(distinct_title$study_time)
mean(chasi, na.rm = TRUE)
#15.5641 차시
sd(chasi, na.rm = TRUE)
var(chasi, na.rm = TRUE)
plot(chasi)
hist(chasi)
boxplot(chasi)
barplot(chasi)
table(chasi)
#0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  30  31  36  38  39  59 100 
#1  30   3   7  38  12   6   2  34   6  47   4   8  17  12  28  89  16  14   4  41  74  16   6  10   5   8   3   5  23   4   1   1   8   1   1

# 과정수 내림차순
course_count <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  select(id, name, gender_code, company_name, date_of_birth, address_1) %>%
  group_by(id) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt))

head(course_count)
View(course_count)
dim(course_count)
#id가 99710이고, id별 수강한 과정수 카운트

# course_count 기술통계
sum(course_count$cnt)
#287409
mean(course_count$cnt)
#2.882449 (1인당 수강과정수 평균)
sd(course_count$cnt)
#4.436348
var(course_count$cnt)
#19.68118
table(course_count$cnt)
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    21    22    23    24 
#44676 23605 11417  5601  3888  2541  1590  1165   904   751   568   433   369   290   274   219   161   141   150   102    81    78    71    53 
#25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48 
#53    50    28    31    30    40    27    23    29    18    12    18    10    12    11     8     7     6     8     9    12     6     9     4 
#49    50    51    53    54    55    56    57    58    59    60    61    62    63    64    65    66    67    70    71    72    74    75    78 
#4     5     6     6     4     6     4     5     2     1     5     6     2     3     6     5     5     2     2     3     2     4     1     1 
#79    80    81    82    83    84    86    87    89    91   103   106   107   112   115   116   118   128   129   161   167   200   209   270 
#1     2     2     2     1     2     2     2     2     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1 
sd(course_count$cnt)
var(course_count$cnt)
fivenum(course_count$cnt)
quantile(course_count$cnt)
quantile(course_count$cnt, probs = 1:100/100)
summary(course_count)

plot(course_count$cnt)
barplot(course_count$cnt)
boxplot(course_count$cnt)
hist(course_count$cnt)
hist(course_count$cnt, probability = TRUE)
lines(density(course_count$cnt), col = 'red')
stem(course_count$cnt)
qqnorm(course_count$cnt)
course_count$num <- seq(1:99710)
course_count

# 구간 나누기 (ifelse)
course_count$cnt_group <- course_count$cnt
course_count$cnt_group <- ifelse(course_count$cnt_group >= 200, "200~",
                                 ifelse(course_count$cnt_group >= 100, "100~",
                                        ifelse(course_count$cnt_group >= 50, "50~",
                                               ifelse(course_count$cnt_group >= 20, "20~",
                                                      ifelse(course_count$cnt_group >= 10, "10~",
                                                             ifelse(course_count$cnt_group >= 5, "5~",
                                                                    ifelse(course_count$cnt_group >= 2, "2~", "1")))))))

head(course_count)
course_cnt_group2 <- sort(table(course_count$cnt_group), decreasing = TRUE)
barplot(course_cnt_group2)
#1    2~    5~   10~   20~   50~  100~  200~ 
#44676 40623 10088  3356   850   103    11     3 

# 수강 많이 한 학습자
learner1 <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'dungoul3@nate.com') #이종헌, 270

sort(table(learner1$service_title), decreasing = TRUE)

learner2 <-
  LearningTime %>%
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'nameserver@korea.com') #주종수, 200

sort(table(learner2$service_title), decreasing = TRUE)

learner3 <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'jwpark@koreatech.ac.kr') #박진우, 129

sort(table(learner3$service_title), decreasing = TRUE)

learner4 <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'ndolcia@naver.com') #김남중, 128

sort(table(learner4$service_title), decreasing = TRUE)

learner5 <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'erun7meu1@hanmail.net') #이동언, 118

sort(table(learner5$service_title), decreasing = TRUE)

learner6 <-
  LearningTime %>% 
  filter(total_learning_time_in_seconds > 0) %>%
  filter(id == 'vosej27@koreatech.ac.kr') #이지은, 84

learner1_dst <-
  learner1 %>%
  distinct(service_title)

learner2_dst <-
  learner2 %>%
  distinct(service_title)

learner3_dst <-
  learner3 %>%
  distinct(service_title)

learner4_dst <-
  learner4 %>%
  distinct(service_title)

learner5_dst <-
  learner5 %>%
  distinct(service_title)

learner_dst <- rbind(learner1_dst, learner2_dst, learner3_dst, learner4_dst, learner5_dst)
learner_dst

# 상위 5명이 수강한 과정들
dim(learner_dst)
#744

learner_dst_cnt <-
  learner_dst %>%
  group_by(service_title) %>%
  summarize(st_cnt = n()) %>%
  arrange(desc(st_cnt))

# 수료한 학습자 (course_completion_status_code == 2)
dim(learner)
#287409

# 어떤 과정을 수강했는지
table(learner$service_title) # 정렬X

learner_course_cnt <-
  learner %>%
  group_by(service_title) %>%
  summarise(st_cnt = n()) %>%
  arrange(desc(st_cnt))

learner_course_cnt$num <- seq(1:1496)
table(learner_course_cnt$st_cnt)
barplot(learner_course_cnt$st_cnt)
hist(learner_course_cnt$st_cnt)

# 수료여부
(counts <- table(learner$course_completion_status_code))
#1 진행 중, 2 수료, 3 출석 기준 미달, 4 점수 기준 미달, 5 평가항목 미진행
#1      2      3      4      5 
#17 178481   5206 103703      2 
barplot(counts, horiz = FALSE, names.arg = c("진행 중", "수료", "출석기준 미달", "점수기준 미달", "평가항목 미진행"), ylab = "수강인원", ylim = c(0, 200000))

plot(learner$total_learning_time_in_seconds)
plot(learner$hhmmss)

# 수강시작일별 수강생수
start_learner <-
  learner %>%
  group_by(start_date) %>%
  summarise(st_learner_cnt = n())

start_learner <- subset(start_learner, select = -c(yymm))
start_learner$yymmdd <- as.Date(start_learner$start_date)
start_learner$year <- as.character(start_learner$yymmdd, '%Y')
start_learner$month <- as.character(start_learner$yymmdd, '%m')

#년월 합치기
library(tidyr)
start_learner <- unite(data = start_learner, col = "yymm", year, month, sep = "-")

# 년월별 그룹핑
library(dplyr)
yymm_learner_cnt <-
  start_learner %>%
  group_by(yymm) %>%
  summarise_at(vars(st_learner_cnt), funs(sum))

yymm_learner_cnt$st_learner_cnt

yymm_lcnt <- c(1105,950,1497,1426,2158,2839,2128,4876,5763,3070,4254,4687,5589,2752,2589,3292,2775,5559,3418,3210,3330,1491,2567,2782,3180,4581,3595,3435,3133,5317,2206,4167,3065,1860,690,9285,1478,5254,2960,2477,3265,7755,5328,4246,5422,3076,2801,7892,6044,5414,3391,2435,2820,3060)

yymm_lcnt_ts <- ts(yymm_lcnt, start = c(2015, 4), end = c(2019, 9), frequency = 12)
plot.ts(yymm_lcnt_ts)
