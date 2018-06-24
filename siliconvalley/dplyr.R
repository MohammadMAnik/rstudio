# dplyr (p.51~)

library(dplyr)

# tbl_df
iris
i2 <- tbl_df(iris) # tbl_df()
class(i2)
i2

# glimpse
glimpse(i2) # all variable can see with transpose 

# %>%
iris %>% head
iris %>% head(10)

# install "gapminder"
install.packages("gapminder")
library(gapminder)

gapminder <- tbl_df(gapminder)
gapminder
glimpse(gapminder)

# filter()
filter(gapminder, country=='Korea, Rep.')
filter(gapminder, year==2007)
filter(gapminder, country=='Korea, Rep.' & year== 2007)

gapminder %>% filter(country == 'Korea, Rep.')
gapminder %>% filter(year == 2007)
gapminder %>% filter(country == 'Korea, Rep.' & year == 2007)

# arrange()
arrange(gapminder, year, country)
gapminder %>% arrange(year, country)

# select()
select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)

# mutate()
gapminder %>% mutate(total_gdp = pop * gdpPercap,
                     le_gdp_ratio = lifeExp / gdpPercap,
                     lgrk = le_gdp_ratio * 100)

# summarize()
gapminder %>%
  summarize(n_obs = n(),
            n_countries = n_distinct(country),
            n_year = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))

# distinct()
distinct(select(gapminder, country))
distinct(select(gapminder, year))

gapminder %>% select(country) %>% distinct()
gapminder %>% select(year) %>% distinct()

# group_by()
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(median(lifeExp))

gapminder %>% 
  filter(year == 2002) %>%
  group_by(country) %>%
  summarise(lifeExp = median(lifeExp)) %>%
  arrange(-lifeExp)

# join

tbl_df(gapminder)

distinct(select(gapminder, country))

filter(gapminder, country == 'Korea, Rep.')
filter(gapminder, year == 2007)
filter(gapminder, country == 'Korea, Rep.' & year == 2007)

gapminder %>% filter(country == 'Korea, Rep.') %>% filter(year == 2007)
gapminder %>% filter(country == 'Korea, Rep.') %>% select(year, lifeExp)

data_yl <- gapminder %>% filter(country == 'Korea, Rep.') %>% select(year, lifeExp)
plot(data_yl)

data_yp <- gapminder %>% filter(country == 'Korea, Rep.') %>% select(year, pop)
plot(data_yp)

data_ygpc <- gapminder %>% filter(country == 'Korea, Rep.') %>% select(year, gdpPercap)
plot(data_ygpc)

gapminder %>% mutate(total_gdp = pop * gdpPercap)

# method 1
d1 = filter(gapminder, year == 2007)
d2 = group_by(d1, continent)
d3 = summarize(d2, lifeExp = median(lifeExp))
arrange(d3, -lifeExp)
arrange(d1, -lifeExp)

# method 2
gapminder %>% filter(year == 2007) %>% group_by(continent) %>% 
  summarize(lifeExp = median(lifeExp)) %>% arrange(-lifeExp)
