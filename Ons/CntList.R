#rm(list=ls())

getwd()
setwd("D:/rstudio/ons")

library(dplyr) # tbl_df
library(car) # recode
library(ggplot2)

# Content Analysis
CntList <- read.csv("CntList.csv", header=TRUE) #stringsAsFactors = TRUE

# summary
summary(CntList)

# variable recode(done)
CntList$SME <- recode(CntList$SME, "'내브' = '내부'")
CntList$SME <- recode(CntList$SME, "'내용전문가없음' = '정보없음'")
CntList$SME <- recode(CntList$SME, "'' = '정보없음'")
CntList$Owner <- recode(CntList$Owner, "'' = '정보없음'")

# write change
write.csv(CntList, file = "CntList.csv")

# tbl_df
CntList <- tbl_df(CntList)

# glimpse
glimpse(CntList)

# barplot graph 
barplot(table(CntList$Owner))
barplot(table(CntList$DevType))
barplot(table(CntList$OprType))
barplot(table(CntList$DevYear))
barplot(table(CntList$SubNum))
barplot(table(CntList$NCS))
barplot(table(CntList$NCSYN))
barplot(table(CntList$DevTech))
barplot(table(CntList$CntType))
barplot(table(CntList$SME))

# descriptive statistics
summary(CntList)
summary(CntList$SubNum)
cor(CntList$DevYear, CntList$SubNum)
plot(CntList$DevYear, CntList$SubNum)

# pairs
pairs(CntList)
pairs(CntList %>% sample_n(50))

### ggplot2 (* use pipline(%>%))

## Owner
CntList %>% ggplot(aes(x=Owner)) + geom_bar()

# frequency
table(CntList$Owner)

# proportion(%)
prop.table(table(CntList$Owner))
round(prop.table(table(CntList$Owner))*100, 1)

# frequency(n), percent(%)
CntList %>% 
  group_by(Owner) %>% 
  tally() %>% 
  mutate(percent=round(n/sum(n)*100, 1))

# By_Owner - DevYear
CntList %>% 
  mutate(By_Owner = factor(Owner)) %>%
  ggplot(aes(By_Owner, OprType)) +
  geom_jitter(col='darkgray') +
  geom_boxplot(alpha=.5)

## DevYear
CntList %>% ggplot(aes(x=DevYear)) + geom_bar()

# frequency
table(CntList$DevYear)

# proportion(%)
prop.table(table(CntList$DevYear))
round(prop.table(table(CntList$DevYear))*100, 1)

# frequency(n), percent(%)
CntList %>% group_by(DevYear) %>% tally() %>% mutate(percent=round(n/sum(n)*100, 1))

# By_DevYear - SubNum(차시)
CntList %>% 
  mutate(By_DevYear = factor(DevYear)) %>%
  ggplot(aes(By_DevYear, SubNum)) +
  geom_jitter(col = 'darkgray') +
  geom_boxplot(alpha = .5)

## NCSYN
CntList %>% ggplot(aes(x=NCSYN)) + geom_bar()

# frequency
table(CntList$NCSYN)

# proportion(%)
prop.table(table(CntList$NCSYN))
round(prop.table(table(CntList$NCSYN))*100, 1)

# frequency(n), percent(%)
CntList %>% group_by(NCSYN) %>% tally() %>% mutate(percent=round(n/sum(n)*100, 1))

# By_NCSYN - NCS
CntList %>% 
  mutate(By_NCSYN = factor(NCSYN)) %>%
  ggplot(aes(By_NCSYN, NCS)) +
  geom_jitter(col = 'darkgray') +
  geom_boxplot(alpha = .5)
  
