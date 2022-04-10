# Explore gapminder
head(gapminder, 6)

# Prepare the nested data frame gap_nested
library(tidyverse)
gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()

# Explore gap_nested
head(gap_nested, 6)

# Create the unnested data frame called gap_unnnested
gap_unnested <- gap_nested %>% 
  unnest()
  
# Confirm that your data was not modified  
identical(gapminder, gap_unnested)

# Extract the data of Algeria
algeria_df <- gap_nested$data[[1]]

# Calculate the minimum of the population vector
min(algeria_df$population)

# Calculate the maximum of the population vector
max(algeria_df$population)

# Calculate the mean of the population vector
mean(algeria_df$population)