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

