# OprResult.join.R

getwd()
setwd("D:/rstudio/ons")

# inner join, two tables

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

# load data
OprResult.join <- read.csv("OprResult.join.csv", header=TRUE)

# summary()
str(OprResult.join)

# 수료율 (수료인원/교육인원)
comp <- sum(OprResult.join$CompleteSum, na.rm = TRUE)
edu <- sum(OprResult.join$EduSum, na.rm = TRUE)
round(comp/edu*100, 1)
# 38.1%

# 과정별 수료율
Edu <- OprResult.join %>%
  select(CntId, EduSum) %>%
  group_by(CntId) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

Comp <- OprResult.join %>%
  select(CntId, CompleteSum) %>%
  group_by(CntId) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

Success <- Edu %>% inner_join(Comp)
Success

plot(Success$EduSum, Success$CompleteSum,
     xlim=c(0, 1000),
     ylim=c(0, 1000))

