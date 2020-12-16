library(readr)

write_vector <- function(data, year, category) {
  path <- paste('Project/data/', category, '/', year, '.csv', sep = '')
  df <- data.frame(matrix(ncol = 1, nrow = length(data), dimnames = list(NULL, c(category))))
  df[category] <- data
  write.csv(df, path)
}

write_authorities <- function(data, year) {
  write_vector(data, year, category = 'authority')
}
write_hubs <- function(data, year) {
  write_vector(data, year, category = 'hub')
}

read_vector <- function(year, category) {
  path <- paste('Project/data/', category, '/', year, '.csv', sep = '')
  df <- read_csv(path)
  return(df[, 2][[1]])
}

read_authorities <- function(year) {
  return(read_vector(year, category = 'authority'))
}

read_hubs <- function(year) {
  return(read_vector(year, category = 'hub'))
}

read_authorities_df_from_year_interval <- function(year_interval) {
  max_rows <- 30288
  generated_authorities_df <- data.frame(1:max_rows)
  colnames(generated_authorities_df) = c('case_id')
  for (year in year_interval) {
    authorities <- read_authorities(year)
    padding <- rep(0, max_rows - length(authorities))
    generated_authorities_df[toString(year)] <- append(authorities, padding)
  }
  return(generated_authorities_df)
}

get_authority_from_generated_authorities_df <- function(year, case_id, generated_authorities_df) {
  return(generated_authorities_df[, toString(year)][case_id])
}