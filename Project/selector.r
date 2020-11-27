# install.packages('tidyverse')
library(readr)
library(tidyverse)

cases = read_csv('Project/data/judicial.csv')
my_data <- as_tibble(cases)
cases_in_range = function(df, from, to) {
  ids_and_years = my_data %>% select(1, 4)
  return (ids_and_years %>% filter(year > 2754))
}

