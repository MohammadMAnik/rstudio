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

# slice
diamonds %>% slice(1:5)
diamonds %>% slice(1:5, 8, 15:20)
diamonds %>% slice(-1)

# mutate
diamonds %>% mutate(price/carat)
diamonds %>% select(carat, price) %>% mutate(price/carat)
diamonds %>% select(carat, price) %>% mutate(ratio=price/carat)
diamonds %>%
  select(carat, price) %>%
  mutate(Ratio=price/carat, Double=Ratio*2)

# summarize
summarize(diamonds, mean(price))
diamonds %>% summarize(mean(price))
diamonds %>% 
  summarize(AvgPrice=mean(price),
            MedianPrice=median(price),
            AvgCarat=mean(carat))

# group_by
diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price))

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price),
            SumCarat=sum(carat))

diamonds %>%
  group_by(cut, color) %>%
  summarize(AvgPrice=mean(price),
            AvgCarat=mean(carat))

# arrange
diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price),
            AvgCarat=mean(carat)) %>%
  arrange(AvgPrice)

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price),
            AvgCarat=mean(carat)) %>%
  arrange(desc(AvgPrice))

# graph
ggplot(meltAgg, aes(x=Year, y=Dollars)) +
  geom_line(aes(group=Program.Name)) +
  facet_wrap(~Program.Name) +
  scale_x_continous(breaks=seq(from=2000, t0=2009, by=2)) +
  theme(axis.text.x=element_text(angle=90, vjust=1, hjust=0)) +
  scale_y_continuous(labels=multiple_format(extra=dollar, multiple="B"))

