#rm(mydata)

getwd()
setwd("D:/rstudio/ons")

library(dplyr) # tbl_df
library(car) # recode
library(ggplot2)

# Content Analysis
CntList <- read.csv("CntList.csv", header = TRUE) #stringsAsFactors = TRUE

# summary
summary(CntList)

# variable recode
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

# graph 
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

# ggplot2
ggplot(CntList, aes(x=DevYear, y=SubNum)) + geom_point()

CntList %>% ggplot(aes(x=DevYear, y=SubNum)) + geom_point()
CntList %>% ggplot(aes(x=DevYear, y=SubNum)) + geom_point(colour = 'blue')

CntList %>% ggplot(aes(x=DevYear)) + geom_bar()

# frequency, %
table(CntList$DevYear)
prop.table(table(CntList$DevYear))
round(prop.table(table(CntList$DevYear))*100, 1)
CntList %>% group_by(DevYear) %>% tally() %>% mutate(pct = round(n/sum(n)*100, 1))

# pairs
CntList_cont <- CntList[, c(1, 6, 7)]
pairs(CntList_cont %>% sample_n(200))
pairs(CntList_cont)

# boxplot
CntList %>% mutate(class = factor(DevType)) %>%
  ggplot(aes(class, DevYear)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .8) + coord_flip()

CntList %>% mutate(class = factor(OprType)) %>%
  ggplot(aes(class, DevYear)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .8) + coord_flip()

