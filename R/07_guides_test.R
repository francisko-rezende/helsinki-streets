library(tidyverse)
library(gapminder)


gapminder %>% 
  filter(country %in% c('Afghanistan', 'Uganda')) %>% 
  ggplot(aes(x = year, y = gdpPercap, color = country)) +
  geom_line(show.legend = "point") 
