#rm(list=ls())

getwd()
setwd("/Users/kimjongha/Documents/rstudio")

# join
# base - merge
# plyr - join
# data.tables

# data, download, USAID Open Government initiative
# 8 files - > 1 file
url="http://jaredlander.com/data/US_Foreign_Aid.zip"

# download using R
download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile="data/ForeignAid.zip")

unzip("data/ForeignAid.zip", exdir="data")

# read files
library(stringr)
theFiles <- dir("data/", pattern="\\.csv")
theFiles

# for loop
for(a in theFiles) {
  nameToUse <- str_sub(string=a, start=12, end=18)
  temp <- read.table(file=file.path("data", a),
                     header=TRUE, sep=",", stringsAsFactors=FALSE)
  assign(x=nameToUse, value=temp)
  }

# plyr join
library(plyr)
Aid90s00sJoin <- join(x=Aid_90s, y=Aid_00s,
                      by=c("Country.Name", "Program.Name"))

head(Aid90s00sJoin)

# 8 data -> 1 data
frameNames <- str_sub(string=theFiles, start=12, end=18)
frameNames

# make empty list
frameList <- vector("list", length(frameNames))
frameList

names(frameList) <- frameNames

# add list
for(a in frameNames) {
  frameList[[a]] <- eval(parse(text=a))
}

#
head(frameList[[1]])
head(frameList[["Aid_00s"]])

# Reduce
# list -> join
allAid <- Reduce(function(...) {
  join(..., by=c("Country.Name", "Program.Name")) },
  frameList)

dim(allAid)

# data.table
library(data.table)
dt90 <- data.table(Aid_90s, key=c("Country.Name", "Program.Name"))
dt00 <- data.table(Aid_00s, key=c("Country.Name", "Program.Name"))

dt0090 <- dt90[dt00]
dt0090

# data melting, data casting
# package, reshape2

# load package
# reshape2
Aid_00s

# country - program - year
library(reshape2)

melt00 <- melt(Aid_00s, id.vars=c("Country.Name", "Program.Name"),
               variable.name="year", value.name="Dollars")

melt00

library(scales)
library(stringr)
library(ggplot2)

melt00$Year <- as.numeric(str_sub(melt00$year, 3, 6))
meltAgg <- aggregate(Dollars ~ Program.Name + Year, data=melt00,
                     sum, na.rm=TRUE)
meltAgg

meltAgg$Program.Name <- str_sub(meltAgg$Program.Name, start=1, end=10)
ggplot(meltAgg, aes(x=Year, y=Dollars)) +
  geom_line(aes(group=Program.Name)) +
  facet_wrap(~ Program.Name)







