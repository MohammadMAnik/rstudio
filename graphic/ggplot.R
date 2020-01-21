# 20190714

# chapter 1, basic of R

# install package
install.packages("ggplot2")
install.packages("gcookbook")

# load package
library(ggplot2)

# chapter 2, structure of data

# plot
mtcars

# plot
plot(mtcars$wt, mtcars$mpg)

# ggplot
# point
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

# data
pressure

# line
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()

# point, line
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# bar chart
barplot(BOD$demand, names.arg=BOD$Time)

table(mtcars$cyl)
barplot(table(mtcars$cyl))

# factor
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# histogram
hist(mtcars$mpg, breaks=10)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)

# boxplot
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)

ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

# chapter 3

# bar chart
ggplot(mtcars, aes(x=cyl, y=hp)) + geom_bar(stat="identity")
ggplot(mtcars, aes(x=cyl, y=hp)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")

# bar chart by count
ggplot(diamonds, aes(x=cut)) + geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_bar()
ggplot(diamonds, aes(x=carat, fill=clarity)) + geom_bar()

# climate
csub <- subset(climate, Source == "Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0

ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) +
  geom_bar(stat="identity", position="identity")

# bar width
# default=0.9, max=1, narrow=0.5
ggplot(diamonds, aes(x=cut, y=price)) + geom_bar(stat="identity")
ggplot(diamonds, aes(x=cut, y=price)) + geom_bar(stat="identity", width=0.8)

# add label in bar chart
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2, colour="white")

# chapter 4

# line graph
BOD
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)

# add point
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=2)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=2, shape=21)

# line type
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line(linetype="dashed", size=0.5)
ggplot(BOD, aes(x=Time, y=demand)) + 
  geom_line(linetype="dashed", size=0.3, colour="blue") +
  geom_point(colour="darkblue")

# point type
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() +
  geom_point(size=3, shape=21, fill="blue", colour="darkblue")

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() +
  geom_point(size=3, shape=21, fill="white", colour="black")


ToothGrowth
library(plyr)
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
pd <- position_dodge(0.2)

ggplot(tg, aes(x=dose, y=length, fill=supp)) + 
  geom_line(position=pd) +
  geom_point(shape=21, size=3, position=pd) +
  scale_fill_manual(values=c("black", "white"))

# shading
sunspots
sunspotyear <- data.frame(Year=as.numeric(time(sunspot.year)),
                          Sunspots=as.numeric(sunspot.year))
head(sunspotyear)

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + 
  geom_area(colour="black", fill="purple", alpha=.2)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + 
  geom_area(fill="purple", alpha=.2) +
  geom_line(size=0.3)

# stacked area graph

# ratio stacked area graph

# chapter 5 plot

# draw plot

# grouping by colour, shape

# overplot
diamonds

ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(alpha=.1)
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(alpha=.01)

ChickWeight
ggplot(ChickWeight, aes(x=Time, y=weight)) + geom_point()
ggplot(ChickWeight, aes(x=Time, y=weight)) + geom_boxplot(aes(group=Time))

# chapter 6 
faithful
str(faithful)
summary(faithful)
ggplot(faithful, aes(x=waiting)) + geom_histogram()

ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(bindwidth=5, fill="white", colour="black")

# 20190728
# geom_area()
sunspot.year
sunspotyear <- data.frame(year=as.numeric(time(sunspot.year)), sunspot=as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x=year, y=sunspot)) + geom_point()
ggplot(sunspotyear, aes(x=year, y=sunspot)) + geom_area()
ggplot(sunspotyear, aes(x=year, y=sunspot)) + geom_area(fill="blue", alpha=.5)
ggplot(sunspotyear, aes(x=year, y=sunspot)) + geom_area(fill="blue", alpha=.5) +
  geom_line(colour="blue", alpha=.5)

# climate
climate
clim <- subset(climate, Source=="Berkeley", select=c("Year", "Anomaly10y", "Unc10y"))
clim

ggplot(clim, aes(x=Year, y=Anomaly10y)) + geom_point()
ggplot(clim, aes(x=Year, y=Anomaly10y)) + geom_line() +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=.2)

# plot
heightweight

heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()

ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=21)
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(size=1)

ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()

# boxplot
ggplot(heightweight, aes(x=sex, y=heightIn)) + geom_boxplot()
table(heightweight$sex)

# lm
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  stat_smooth(method=lm)

ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  stat_smooth(method=lm, se=FALSE)

ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  stat_smooth(method=loess)

# logistic lm
MASS
library(MASS)
biopsy
b <- biopsy
b$classn[b$class=="benign"] <- 0
b$classn[b$class=="malignant"] <- 1

ggplot(b, aes(x=V1, y=classn)) + geom_point()
ggplot(b, aes(x=V1, y=classn)) + 
  geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4) 

ggplot(b, aes(x=V1, y=classn)) + 
  geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4) +
  stat_smooth(method=glm, family=binomial)

