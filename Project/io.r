library(readr)

write_vector = function(data, year, category) {
  path = paste('Project/data/', category, '/', year, '.csv', sep = '')
  df = data.frame(matrix(ncol = 1, nrow = length(data), dimnames = list(NULL, c(category))))
  df[category] = data
  write.csv(df, path)
}

write_authorities = function(data, year) {
  write_vector(data, year, category = 'authority')
}
write_hubs = function(data, year) {
  write_vector(data, year, category = 'hub')
}

read_vector = function(year, category) {
  path = paste('Project/data/', category, '/', year, '.csv', sep = '')
  df = read_csv(path)
  return(df[, 2][[1]])
}

read_authorities = function(year) {
  return(read_vector(year, category = 'authority'))
}

read_hubs = function(year) {
  return(read_vector(year, category = 'hub'))
}