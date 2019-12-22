# paste
paste("Hello", "Jared", "and others")
paste("Hello", "Jared", "and others", sep="/")

vectorOfText <- c("Hello", "Jared", "and otehrs", ".")
vectorOfText
paste(vectorOfText, collapse=" ")
paste(vectorOfText, collapse="*")

# sprintf
person <- "Jared"
partySize <- "eight"
waitTime <- 25

sprintf("Hello %s, your party of %s will be seated in %s minutes",
      person, partySize, waitTime)

# text extract
install.packages("XML")
library(XML)

theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
presidents <- readHTMLTable(theURL, which=3, as.data.frame=TRUE,
                            skip.rows=1, header=TRUE, stringAsFactors=FALSE)

head(presidents)
tail(presidents)
presidents <- presidents[1:64, ]

# stringr
library(stringr)
yearList <- str_split(string=presidents$YEAR, pattern="-")
yearList

yearMatrix <- data.frame(Reduce(rbind, yearList))
yearMatrix

names(yearMatrix) <- c("Start", "Stop")
yearMatrix

presidents <- cbind(presidents, yearMatrix)
presidents

presidents$Start <- as.numeric(as.character(presidents$Start))
presidents$Stop <- as.numeric(as.character(presidents$Stop))
presidents
