# crosstable
chisq.test(c(25, 32, 28, 20))

# data = HairEyeColor
HairEyeColor

Eye <- margin.table(HairEyeColor, 2)
Eye

chisq.test(Eye, p=c(.5, .25, .15, .1))

HairEye <- margin.table(HairEyeColor, c(1,2))
HairEye

chisq.test(HairEye)

# crosstable
str(esoph)

# xtabs()
xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
