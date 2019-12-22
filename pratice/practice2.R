# data
data(diamonds, package="ggplot2")
diamonds

head(diamonds)

# aggregate
aggregate(price ~ cut, diamonds, mean)
aggregate(price ~ cut + color, diamonds, mean)
aggregate(cbind(price, carat) ~ cut, diamonds, mean)
aggregate(cbind(price, carat) ~ cut + color, diamonds, mean)

# aggregate, each
aggregate(price ~ cut, diamonds, each(mean, median))

# load package
library(plyr)
head(baseball)
baseball$sf[baseball$year < 1954] <- 0
any(is.na(baseball$sf))

baseball$hbp[is.na(baseball$hbp)] <- 0
any(is.na(baseball$hbp))

baseball <- baseball[baseball$ab >= 50, ]

baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))
tail(baseball)

obp <- function(data) {
  c(OBP = with(data, sum(h+bb+hbp)/sum(ab+bb+hbp+sf)))
}

careerOBP <- ddply(baseball, .variables="id", .fun=obp)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing=TRUE), ]

head(careerOBP, 10)

# data.table (Matt Dowle)
library(data.table)
diamondssDT <- data.table(diamonds)
diamondsDT
diamondsDT[1:3,]
diamondsDT[diamondsDT$carat <= 0.23, ]

tables()

# setkey
setkey(diamondsDT, cut, color)
diamondsDT[J("Ideal", "E")]

# aggregate
diamondsDT[, mean(price), by=cut]
diamondsDT[, list(price = mean(price)), by=cut]

diamondsDT[, list(price=mean(price)), by=list(cut, color)]
diamondsDT[, list(price=mean(price), carat=mean(carat), caratSum=sum(carat)), by=cut]

diamondsDT[, list(price=mean(price), carat=mean(carat)), by=list(cut, color)]
plot(diamondsDT[, list(price=mean(price), carat=mean(carat)), by=list(cut, color)])

# dplyr
search()

# load package
library(magrittr)

data(diamonds, package="ggplot2")
dim(head(diamonds, n=4))

diamonds %>% head(4) %>% dim

# load package
library(ggplot2)
class(diamonds)

# load package
library(dplyr)

head(diamonds)
diamonds

# select
select(diamonds, carat, price)
diamonds %>% select(carat, price)
diamonds %>% select(c(carat, price))
diamonds[, c('carat', 'price')]

diamonds %>% select(starts_with('c'))
diamonds %>% select(ends_with('e'))
diamonds %>% select(contains('l'))

# regular expression
diamonds %>% select(matches('r.+t'))

# exception
diamonds %>% select(-carat, -price)
diamonds %>% select(-c(carat, price))

# filter
diamonds %>% filter(cut == "Ideal")
diamonds[diamonds$cut == "Ideal", ]
diamonds %>% filter(cut %in% c("Ideal", "Good"))
diamonds %>% filter(price >= 1000)
diamonds %>% filter(price != 1000)
diamonds %>% filter(carat > 2, price < 14000)
diamonds %>% filter(carat > 2 & price < 14000)
diamonds %>% filter(carat < 1 | carat > 5)
