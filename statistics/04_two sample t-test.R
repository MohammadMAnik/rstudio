# 03_two sample t-test

# two-sample t-test
# independent groups
LungCapData <- read.table("dataset/LungCapData.txt", sep = '\t', header = TRUE)

LungCap <- LungCapData$LungCap
Smoke <- LungCapData$Smoke


# We will be exploring the relationship between Smoking and Lung Capacity.
# two sample t-test
# Ho : mean lung cap of smokers = of non smokers
# two-sided test
# assume non-equal variance
t.test(LungCap ~ Smoke)
t.test(LungCap ~ Smoke, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F) # same argument

# two separate groups which is smoke yes or no, we would like compared
t.test(LungCap[Smoke == "no"], LungCap[Smoke == "yes"])

# Levene's test command
# Ho : population variances are equal
# to use this test one must have the CAR
library(car)

# We would like to compare variations in Lung Capacities separated by the Smoking status
# Levene's Test for equality of variances
leveneTest(LungCap ~ Smoke)

# Here we can see that with a small P value.
# We should reject the null hypothesis and conclude that we have evidence to believe population variances are
# not equal and use the non-equal assumption

# Bartlett's Test for equality of variance
bartlett.test(LungCap ~ Smoke)

# variance
var(LungCap[Smoke == "no"])
var(LungCap[Smoke == "yes"])

# normality test
# Shapiro-Wilk normality test
shapiro.test(LungCap)

