library(readr)

write_vector = function(data, year, category ) {
  path = paste('Project/data/', category, '/', year, '.csv', sep = '')
  df = data.frame(matrix(ncol = 1, nrow = length(vector), dimnames = list(NULL, c(category))))
  df[category] = vector
  write.csv(df, path)
}

write_authorities = function(data, year) {
  write_vector(data, year, category = 'authority')
}
write_hubs = function(data, year) {
  write_vector(data, year, category = 'hubs')
}


read_authorities = function(year){
  
}
read_hubs = function(year){
  
}