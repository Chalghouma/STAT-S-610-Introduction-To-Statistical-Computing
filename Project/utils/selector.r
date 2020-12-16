library(tidyverse)

get_graph_labels <- function(case_ids, judicial_df) {
  graph_labels = unlist(sapply(case_ids, FUN = get_case_data_by_id, judicial_df)[3,])
}

get_case_data_by_id <- function(case_id, df) {
  return(df %>% filter(caseid == case_id))
}

get_cases_in_range <- function(df, from, to) {
  return(df %>% filter(year >= from & year <= to))
}
get_cases_until <- function(df, until) {
  first_year = df[1, ]$year
  return(get_cases_in_range(df, first_year, until))
}
