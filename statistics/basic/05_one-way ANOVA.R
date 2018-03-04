# one-way ANOVA
DietData <- read.table("rstudio/dataset/DietWeigthLoss.txt", header = TRUE)

# dimension
dim(DietData)

# colnames
names(DietData)

# class
class(DietData$WeightLoss)
class(DietData$Diet)

# "aov" command
help(aov)

# We would like to compare WeightLoss separated by Diet type
boxplot(DietData$WeightLoss ~ DietData$Diet)

# Ho : Mean Weight loss is the same for all diets
aov(DietData$WeightLoss ~ DietData$Diet)

ANOVA1 = aov(DietData$WeightLoss ~ DietData$Diet)
ANOVA1

summary(ANOVA1)

attributes(ANOVA1)

# We can extract certain attributes from this objects using the dollar sign($).
ANOVA1$coefficients

TukeyHSD(ANOVA1)

plot(TukeyHSD(ANOVA1), las = 1)

# Kruskal Wallis One-way Analysis of Variance is a non-parametric equivalent the one-way Analysis of Variance
kruskal.test(DietData$WeightLoss ~ DietData$Diet)
