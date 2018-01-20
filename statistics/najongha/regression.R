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
model1 <- lm(Life.Exp ~ ., data = st)
summary(model1)
