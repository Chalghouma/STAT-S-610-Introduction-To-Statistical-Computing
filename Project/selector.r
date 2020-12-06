# install.packages('tidyverse')
library(tidyverse)

get_all_judicial_cases_data = function() {
  csv_path = 'Project/data/judicial.csv'
  cases = read_csv(csv_path)
  my_data <- as_tibble(cases)
  return (my_data)
}

case_data_by_id = function(case_id, df) {
  return(df %>% filter(caseid == case_id))
}

cases_in_range = function(df, from, to) {
  return(df %>% filter(year >= from & year <= to))
}
cases_until = function(df, until) {
  first_year = df[1,]$year
  return(cases_in_range(df, first_year, until))
}

parse_line = function(line) {
  splitted = str_split(line, ' ')[[1]]
  citing = splitted[1] %>% strtoi()
  cited = splitted[2] %>% strtoi()

  return(list('citing' = citing, 'cited' = cited))
}

calculate_inward_outward = function(ids, file_path = 'Project/data/allcites.txt') {
  inward_vector = rep(0, length(ids))
  outward_vector = rep(0, length(ids))
  lines = readLines(file_path)
  #We switched to the for loop to avoid the copy/re-instanciation of the vectors, so we can update them directly
  for (line_index in 1:length(lines)) {
    parsed = parse_line(lines[line_index])
    line = lines[line_index]
    splitted = str_split(line, ' ')[[1]]
    citing = splitted[1] %>% strtoi()
    cited = splitted[2] %>% strtoi()

    inward_vector[cited] = inward_vector[cited] + 1
    outward_vector[citing] = outward_vector[citing] + 1
  }
  return(list('inward_vector' = inward_vector, 'outward_vector' = outward_vector))
}
