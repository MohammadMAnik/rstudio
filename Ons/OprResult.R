# OprResult

getwd()
setwd("D:/rstudio/ons")

library(dplyr) # tbl_df

OprResult <- read.csv("OprResult.csv", header=TRUE)
names(OprResult)

# name() modify
names(OprResult) <- c("No", "Title", "Year", "OprType", "SubOT", "Cardinal",
                      "Company", "SubCompany", "Ordinal", "OprStart", "OprEnd",
                      "Emp_Large", "Emp_Middle", "Emp_MS", "Emp_Small", "None",
                      "EmpSum", "JobSeeker", "Miscel", "EduSum", "CompleteSum")

# save
write.csv(x=OprResult, file="OprResult.csv", row.names=FALSE)

# summary
dim(OprResult)
str(OprResult)

is.na(OprResult)
colSums(is.na(OprResult))

sum(OprResult$EduSum, na.rm=TRUE)
sum(OprResult$CompleteSum, na.rm=TRUE)

# complete rate
round(114018/303490, 3)*100
# = 37.6%

# table
table(OprResult$No)
table(OprResult$Title)
table(OprResult$CompleteSum)

table(OprResult$No, OprResult$Year)
table(OprResult$CompleteSum, OprResult$No)

# xtabs
xtabs(formula = CompleteSum ~ No, data = OprResult)
xtabs(formula = CompleteSum ~ Year, data = OprResult)

# aggregate()
aggregate(x=OprResult$CompleteSum, by=list(OprResult$No), FUN=sum, na.rm=TRUE)
aggregate(x=OprResult$CompleteSum, by=list(OprResult$Year), FUN=sum, na.rm=TRUE)
aggregate(x=OprResult$CompleteSum, by=list(OprResult$SubOT), FUN=sum, na.rm=TRUE)
aggregate(x=OprResult$CompleteSum, by=list(OprResult$Cardinal), FUN=sum, na.rm=TRUE)
aggregate(x=OprResult$CompleteSum, by=list(OprResult$Company), FUN=sum, na.rm=TRUE)

# by()
by(data=OprResult$CompleteSum, INDICES=OprResult$No, na.rm=TRUE, FUN=sum)
by(data=OprResult$CompleteSum, INDICES=OprResult$Year, na.rm=TRUE, FUN=sum)
by(data=OprResult$CompleteSum, INDICES=OprResult[, "SubOT"], na.rm=TRUE, FUN=sum)

# rowsum()
rowsum(x=OprResult$CompleteSum, group=OprResult$No, na.rm=TRUE)
rowsum(x=OprResult$CompleteSum, group=OprResult$Year, na.rm=TRUE)
rowsum(x=OprResult$CompleteSum, group=OprResult$Company, na.rm=TRUE)

# 이후는 OprResult.join.R에서 분석



