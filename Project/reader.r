source('Project/selector.r')
library(readr)


read_auth = function(path = 'Project/data/authmat.txt') {
  df = read.csv(path)
}

get_auth_by_year = function(auth_df, year) {
  #We already know the first year is 1800
  # if(year < 1800 )
  if (year < 1800 | year > 2002)
    return(warning('Year should be between 1800 and 2002'))
  return(auth_df[, (year - 1800) + 1 + 1])
}

plot_case = function(authority_df, case_id, year_interval, plot_function=plot) {
  get_authority_for_case = function(year, case_id) {
    return(get_auth_by_year(authority_df, year)[case_id])
  }
  data = sapply(year_interval, FUN = get_authority_for_case, case_id)

  plot_function(data ~ year_interval, type = 'l', lty = 3, xlab = "Year Interval", ylab = "Authority score")
}

brown_versus_mississipi = function() {
  df = read_auth()
  plot_case(df, 18501, 1940:2000,plot)
}



brown_versus_mississipi()