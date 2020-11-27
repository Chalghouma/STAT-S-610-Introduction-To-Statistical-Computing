# install.packages('tidyverse')
library(readr)
library(tidyverse)

# cases = read_csv('Project/data/judicial.csv')
# my_data <- as_tibble(cases)
# ids = my_data[, 1][[1]]
# cases_in_range = function(df, from, to) {
#   ids_and_years = my_data %>% select(1, 4)
#   return(ids_and_years %>% filter(year > 2754))
# }

# process_line = function(line, inward_vector, outward_vector) {
#   splitted = str_split(line, ' ')[[1]]
#   from = splitted[1] %>% strtoi()
#   to = splitted[2] %>% strtoi()
#   inward_vector[to] = inward_vector[to] + 1
#   outward_vector[from] = outward_vector[from] + 1
# }

calculate_inward_outward = function(ids,file_path = 'Project/data/allcites.txt') {
  inward_vector = rep(0, length(ids))
  outward_vector = rep(0, length(ids))
  lines = readLines(file_path)
  #We switched to the for loop to avoid the copy/re-instanciation of the vectors, so we can update them directly
  for (line_index in 1:length(lines)) {
    line = lines[line_index]
    splitted = str_split(line, ' ')[[1]]
    from = splitted[1] %>% strtoi()
    to = splitted[2] %>% strtoi()
    inward_vector[to] = inward_vector[to] + 1
    outward_vector[from] = outward_vector[from] + 1
  }
  return (list('inward_vector'=inward_vector,'outward_vector'=outward_vector))
#   inward_vector
#   outward_vector
}

# calculate_inward_outward(ids)