# paired sample t-test
# data = BloodPressure
BloodPressure <- read.table("rstudio/dataset/BloodPressure.txt", header = TRUE)

# dimension
dim(BloodPressure)

# column names
names(BloodPressure)

# row 1 to 3
BloodPressure[1:3,]

# help(t.test)
help(t.test)

boxplot(BloodPressure$Before, BloodPressure$After)

plot(BloodPressure$Before, BloodPressure$After)

# add a 45-degree line to this plot using the "abline" argument.
# Here we will add a line that has an intercept of 0 and a slope of 1
abline(a=0, b=1)

# Ho : Mean difference in systolic blood pressure is 0
# two-sided test
# Before, After
t.test(BloodPressure$Before, BloodPressure$After, mu = 0, alt = "two.sided", paired = TRUE, conf.level = 0.95)

# After, Before 
t.test(BloodPressure$After, BloodPressure$Before, mu = 0, alt = "two.sided", paired = TRUE, conf.level = 0.95)

