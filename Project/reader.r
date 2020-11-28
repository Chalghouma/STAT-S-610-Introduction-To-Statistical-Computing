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

plot_case = function(authority_df, case_id, year_interval, plot_function = plot, type = 'l', pch = 22, lty = 2, col = 'black') {
  get_authority_for_case = function(year, case_id) {
    return(get_auth_by_year(authority_df, year)[case_id])
  }
  data = sapply(year_interval, FUN = get_authority_for_case, case_id)

  plot_function(data ~ year_interval, xlab = "Year Interval", ylab = "Authority score", pch = pch, lty = lty, col = col, type = type)
}

brown_versus_mississipi = function() {
  df = read_auth()
  plot_case(df, 18501, 1940:2000, plot)
}

plot_figure_10 = function() {
  brown_v_mississipi_case_id = 18501
  escobedo_v_illinois_case_id = 23115
  miranda_v_arizona_case_id = 23601
  rhode_island_v_innis_case_id = 26918

  year_interval = 1940:2000

  case_ids = c(brown_v_mississipi_case_id, escobedo_v_illinois_case_id, miranda_v_arizona_case_id, rhode_island_v_innis_case_id)
  line_types = c(2, 3, 3, 3)
  graph_labels = c('Brown v Mississippi', 'Escobedo v. Illinois', 'Miranda v. Arizona', 'Rhode Island v. Innis')
  colors = c('blue', 'red', 'black', 'green')
  df = read_auth()
  indices = 1:length(case_ids)
  pre_plot_case = function(index, authority_df, case_ids, year_interval, line_types, colors) {
    if (index == 1) plot_function = plot
    else plot_function = lines
    plot_case(authority_df, case_ids[index], year_interval, plot_function, lty = line_types[index], col = colors[index])
  }
  sapply(indices, FUN = pre_plot_case, df, case_ids, year_interval, line_types, colors)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = rep(1, 4))
}


plot_figure_10()