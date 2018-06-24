# dplyr (p.51~)

library(dplyr)

# tbl_df
iris
i2 <- tbl_df(iris)
class(i2)
i2

# glimpse
glimpse(i2)

# %>%
iris %>% head
iris %>% head(10)

# filter
install.packages("gapminder")
library(gapminder)

gapminder <- tbl_df(gapminder)
gapminder
glimpse(gapminder)

filter(gapminder, country=='Korea, Rep.')
filter(gapminder, year==2007)
filter(gapminder, country=='Korea, Rep.' & year== 2007)

gapminder %>% filter(country == 'Korea, Rep.')
gapminder %>% filter(year == 2007)
gapminder %>% filter(country == 'Korea, Rep.' & year == 2007)

# arrange
arrange(gapminder, year, country)
gapminder %>% arrange(year, country)

# select
select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)

# mutate
gapminder %>% mutate(total_gdp = pop * gdpPercap,
                     le_gdp_ratio = lifeExp / gdpPercap,
                     lgrk = le_gdp_ratio * 100)

# group_by
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(median(lifeExp))

gapminder %>% 
  filter(year == 2002) %>%
  group_by(country) %>%
  summarise(lifeExp = median(lifeExp)) %>%
  arrange(-lifeExp)
