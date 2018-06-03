# rm(list=ls())
library(dplyr)

airquality
str(airquality)
class(airquality)
summary(airquality)

air <- filter(airquality, Month == 6)
head(air)

air <- filter(airquality, Month == 6 | Temp > 90)
air
head(air)

slice(airquality, 6:10)

# arrange()
air <- arrange(airquality, Temp, Month, Day)
air <- arrange(airquality, desc(Temp), Month, Day)
air

# select()
air <- select(airquality, Month, Day, Temp)
air

air <- select(airquality, -(Ozone:Wind))
air

# rename()
air <- rename(airquality, Solar=Solar.R)
air

# distinct()
distinct(select(airquality, Month))

# mutate()
air <- mutate(airquality, Temp.C=(Temp-32)/1.8, Diff=Temp.C - mean(Temp.C))
head(air)

# summarise()
air <- summarise(airquality, mean(Temp))
air

# Temp
air <- summarise(airquality,
                 Min = min(Temp, na.rm = TRUE),
                 Median = median(Temp, na.rm = TRUE),
                 Mean = mean(Temp, na.rm = TRUE),
                 SD = sd(Temp, na.rm = TRUE),
                 Max = max(Temp, na.rm = TRUE),
                 N = n(),
                 Distinct.Month = n_distinct(Month),
                 First.Month = first(Month),
                 Last.Month = last(Month))

air

# pipe operator
iris %>% head

air <- airquality %>% select(Ozone, Temp, Month) %>% group_by(Month) %>%
  summarise(Mean.Ozone = mean(Ozone, na.rm = TRUE),
            Mean.Temp = mean(Temp, na.rm = TRUE)) %>% filter(Mean.Ozone > 40 | Mean.Temp > 80)

round(air, 2)

