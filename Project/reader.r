source('Project/selector.r')
source('Project/ws.r')
library(readr)
library(jsonlite)

read_judicial_data = function() {
  cases = read_csv('Project/data/judicial.csv')
  return(as_tibble(cases))
}

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

calculate_authority_score_in_range = function(authority_df, case_id, year_interval) {
  get_authority_for_case = function(year, case_id) {
    return(get_auth_by_year(authority_df, year)[case_id])
  }
  return(sapply(year_interval, FUN = get_authority_for_case, case_id))

}

plot_case = function(authority_df, case_id, year_interval, plot_function = plot, type = 'l', pch = 22, lty = 2, col = 'black', onGraphProcessedCallback, xData) {
  print('onGraphProc')
  onGraphProcessedCallback(year_interval, data)
  data = calculate_authority_score_in_range(authority_df, case_id, year_interval)
  plot_function(data ~ xData, xlab = "Year Interval", ylab = "Authority score", pch = pch, lty = lty, col = col, type = type)
}
plot_case_for_6 = function(authority_df, case_id, year_interval, plot_function = plot, type = 'l', pch = 22, lty = 2, col = 'black', onGraphProcessedCallback, xData) {
  print('onGraphProc')
  onGraphProcessedCallback(year_interval, data)
  data = calculate_authority_score_in_range(authority_df, case_id, year_interval)
  plot_function(data ~ xData, xlab = "Years after decision", ylab = "Authority score", pch = pch, lty = lty, col = col, type = type)
}

brown_versus_mississipi = function() {
  df = read_auth()
  plot_case(df, 18501, 1940:2000, plot)
}
plot_figure_6 = function(onGraphProcessedCallback) {
  brown_v_mississipi_case_id = 21109
  escobedo_v_illinois_case_id = 25347


  year_interval = 1940:2003
  case_ids = c(brown_v_mississipi_case_id, escobedo_v_illinois_case_id)
  line_types = c(2, 3)
  graph_labels = c('Brown v. Board of Educ', 'Escobedo v. Illinois')
  colors = c('blue', 'red')
  authority_df = read_auth()
  judicial_df = read_judicial_data()
  indices = 1:length(case_ids)
  pre_plot_case = function(index, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback) {
    if (index == 1) plot_function = plot
    else plot_function = lines
    year_of_decision =case_data_by_id(case_ids[index],judicial_df)$year
    year_interval = year_of_decision:(year_of_decision+30)
    plot_case_for_6(authority_df, case_ids[index], year_interval, plot_function, lty = line_types[index], col = colors[index], onGraphProcessedCallback = onGraphProcessedCallback, xData = 0:30)
  }
  sapply(indices, FUN = pre_plot_case, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = rep(1, 4))
}
plot_figure_10 = function(onGraphProcessedCallback) {
  brown_v_mississipi_case_id = 18501
  escobedo_v_illinois_case_id = 23115
  miranda_v_arizona_case_id = 23601
  rhode_island_v_innis_case_id = 26918

  year_interval = 1940:2003
  case_ids = c(brown_v_mississipi_case_id, escobedo_v_illinois_case_id, miranda_v_arizona_case_id, rhode_island_v_innis_case_id)
  line_types = c(2, 3, 3, 3)
  graph_labels = c('Brown v Mississippi', 'Escobedo v. Illinois', 'Miranda v. Arizona', 'Rhode Island v. Innis')
  colors = c('blue', 'red', 'black', 'green')

  authority_df = read_auth()
  labels = sapply(case_ids, FUN = case_data_by_id, get_all_judicial_cases_data())
  print(labels)
  indices = 1:length(case_ids)
  pre_plot_case = function(index, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback) {
    if (index == 1) plot_function = plot
    else plot_function = lines
    plot_case(authority_df, case_ids[index], year_interval, plot_function, lty = line_types[index], col = colors[index], onGraphProcessedCallback = onGraphProcessedCallback)
  }
  sapply(indices, FUN = pre_plot_case, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback = onGraphProcessedCallback, xData = year_interval)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = rep(1, 4))
}

onWSReadyCallback = function(ws) {
  print('onWSReady')
  onGraphProcessedCallback = function(year_interval, data) {
    print('onGraphProcessedCallback')
    ws$send(serializeJSON(list(year_interval = year_interval, data = data)))
  }
  plot_figure_10(onGraphProcessedCallback)
}
# launch_server(onWSReadyCallback)

plot_figure_8 = function(onGraphProcessedCallback){

  year_interval = 1850:2000
  case_ids=c(1861, 1156,13828,19238)
  line_types = c(2, 3, 3, 3)
  graph_labels = c('Brown v Mississippi', 'Escobedo v. Illinois', 'Miranda v. Arizona', 'Rhode Island v. Innis')
  colors = c('blue', 'red', 'black', 'green')

  authority_df = read_auth()
  labels = sapply(case_ids, FUN = case_data_by_id, get_all_judicial_cases_data())
  print(labels)
  indices = 1:length(case_ids)
  pre_plot_case = function(index, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback,xData) {
    if (index == 1) plot_function = plot
    else plot_function = lines
    plot_case(authority_df, case_ids[index], year_interval, plot_function, lty = line_types[index], col = colors[index], onGraphProcessedCallback = onGraphProcessedCallback,xData = xData)
  }
  sapply(indices, FUN = pre_plot_case, authority_df, case_ids, year_interval, line_types, colors, onGraphProcessedCallback = onGraphProcessedCallback, xData = year_interval)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = rep(1, 4))
}

plot_figure_8(function(a,b){})

# plot_figure_6(function(a, b) { })
# plot_figure_10(function(a, b) { })