# install.packages('tidyverse')
library(tidyverse)

get_all_judicial_cases_data <- function() {
  csv_path = 'Project/data/judicial.csv'
  cases = read_csv(csv_path)
  my_data <- as_tibble(cases)
  return(my_data)
}

get_graph_labels <- function(case_ids, judicial_df) {

  graph_labels = unlist(sapply(case_ids, FUN = case_data_by_id, judicial_df)[3,])
}

case_data_by_id <- function(case_id, df) {
  return(df %>% filter(caseid == case_id))
}

cases_in_range <- function(df, from, to) {
  return(df %>% filter(year >= from & year <= to))
}
cases_until <- function(df, until) {
  first_year = df[1,]$year
  return(cases_in_range(df, first_year, until))
}
