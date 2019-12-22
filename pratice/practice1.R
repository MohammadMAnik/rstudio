# load package
library(ggplot2)

# load data
diamonds

# summary
head(diamonds)

# histogram
hist(diamonds$carat, main="Caret Histogram")

# plot
plot(price ~ carat, data=diamonds)
plot(diamonds$carat, diamonds$price)

# boxplot
boxplot(diamonds$carat)

# ggplot, geom_histogram
ggplot(data=diamonds) + 
  geom_histogram(aes(x=carat))

# ggplot, geom_density
ggplot(data=diamonds) +
  geom_density(aes(x=carat))

# ggplot, geom_point
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=color))

# ggplot, geom_point, facet_wrap
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point() +
  facet_wrap(~cut)

# ggplot, geom_point, facet_grid
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=color)) +
  facet_grid(cut ~ clarity)

# ggplot, geom_histogram, facet_wrap
ggplot(diamonds, aes(x=carat)) +
  geom_histogram() +
  facet_wrap(~color)

# ggplot, geom_boxplot
ggplot(diamonds, aes(y=carat, x=1)) +
  geom_boxplot()

# ggplot, geom_boxplot
ggplot(diamonds, aes(y=carat, x=cut)) +
  geom_boxplot()

# ggplot, geom_violin
ggplot(diamonds, aes(y=carat, x=cut)) +
  geom_violin()

# ggplot, geom_violin, geom_point
ggplot(diamonds, aes(y=carat, x=cut)) +
  geom_violin() +
  geom_point()


# load data
economics

# ggplot, geom_line
ggplot(economics, aes(x=date, y=pop)) +
  geom_line()

# package
install.packages("lubridate")
library(lubridate)

# extract year
economics$year <- year(economics$date)
economics$month <- month(economics$date, label=TRUE)

# economics, year >= 2000
econ2000 <- economics[which(economics$year >= 2000), ]
econ2000

# ggplot, geom_line, scale_color_discrete, scale_y_continuou, labs
ggplot(econ2000, aes(x=month, y=pop)) +
  geom_line(aes(color=factor(year), group=year)) +
  labs(title="Population Growth", x="Month", y="Population")
  
  
