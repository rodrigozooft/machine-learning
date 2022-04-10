# Explore gapminder
head(gapminder, 6)

# Prepare the nested data frame gap_nested
library(tidyverse)
gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()

# Explore gap_nested
head(gap_nested, 6)