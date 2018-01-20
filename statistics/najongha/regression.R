# regression analysis
# simple linear regression
attach(cats)
str(cats)

lm(Hwt ~ Bwt)
lm.out <- lm(Hwt ~ Bwt)

summary(lm.out)

# except stars
options(show.signif.stars = FALSE)
anova(lm.out)

cats[144,]

rlm(Hwt ~ Bwt)

# multiple linear regression
# multiple correlation coefficient, multiple R^2
# multicollinearity
data(state)
st <- as.data.frame(state.x77)
str(st)

colnames(st)[4] <- "Life.Exp"
colnames(st)[6] <- "HS.Grad"

st[,9] <- st$Population*1000/st$Area
colnames(st)[9] <- "Density"
str(st)

summary(st)
cor(st)
pairs(st)

# multiple linear regression
model11 <- lm(Life.Exp ~ ., data = st)
summary(model11)

model.step <- step(model11, direction = "backward")
summary(model.step)

confint(model.step)

predict(model.step, list(Population=4000, Murder=10.5, HS.Grad=48, Frost=100))

par(mfrow=c(2,2))
plot(model.step)

# data = Prestige
# correlation, regression
library(car)
data(Prestige)
str(Prestige)

head(Prestige)
summary(Prestige)

# correlation visualization
plot(Prestige[,1:4], pch=15, col="blue")

# pairs.panel{psych}
library(psych)
pairs.panels(Prestige[,1:4], scale=TRUE)
pairs.panels(Prestige[,1:4], bg=c("red", "yellow", "blue")[Prestige$type], pch=21)

# model
mod1 <- lm(income ~ education + prestige + women, data = Prestige)
summary(mod1)

mod2 <- lm(income ~ prestige + women, data = Prestige)
summary(mod2)

plot(mod2, pch = 16, which = 1)

# data = mtcars
data(mtcars)
fit <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# outlier test
library(car)
outlierTest(fit)

# qqplot
qqPlot(fit, main = "QQ Plot")

# leverage plot
leveragePlots(fit)

# influential point
avPlots(fit)

# cook's distance
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which = 4, cook.levels = cutoff)

# regression influence
influencePlot(fit, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance")


