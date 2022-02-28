library(gapminder)
#filter
gapminder %>%
  filter(year == 2007)
#arrange
gapminder %>% #pipe operator
  arrange(desc(gdpPercap))#default is ascending
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))

#mutate
gapminder %>%
  mutate(pop = pop/1000000) %>%
  mutate(gdp = gdpPercap * pop)

gapminder %>%
  mutate(gdp = gdpPercap * pop)%>%
  filter(year == 2007) %>%
  arrange(desc(gdp))

#variable assignment
gapminder_2007 <- gapminder %>%
  filter(year == 2007)

library(ggplot2)
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
  geom_point() +
  scale_x_log10()

#faceting and axis limits
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
  geom_point() +
  scale_x_log10()+
  expand_limits(y = 0)
facet_wrap(~continent)

#mean, sum, median, min, max, summarize
#group by will give combinations of variable
gapminder %>%
  filter(year == 1957)%>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpCap = max(gdpPercp))

#line plots, bar plot, histogram, boxplot
#line: geom_line()
#bar: geom_col()
#histo: geom_histogram(binwidth = 5, bins = 50)
#box:geom_boxplot()

#add title
ggtitle("")
