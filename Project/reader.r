source('Project/selector.r')
source('Project/ws.r')
source('Project/plotting/plotter.r')
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
  case_ids = c(21109, 25347)
  plotting_functions = plot_functions(length(case_ids), xLab = 'Year', yLab = 'Authority Score')
  authority_df = read_auth()
  judicial_df = read_judicial_data()

  graph_labels = get_graph_labels(case_ids, judicial_df)
  pre_plot_case = function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]

    year_of_decision = case_data_by_id(case_ids[index], judicial_df)$year
    years_after_decision = 30
    year_interval = year_of_decision:(year_of_decision + 30)
    X = 0:years_after_decision
    Y = calculate_authority_score_in_range(authority_df, case_id, year_interval)
    plot_function(X, Y)
  }
  sapply(1:length(case_ids), FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(20, 0.01, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = 1:length(case_ids))
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

plot_figure = function(case_ids,year_interval,onGraphProcessedCallback){ 
  authority_df = read_auth()
  judicial_df = read_judicial_data()
  plotting_functions = plot_functions(length(case_ids), xLab = 'Year', yLab = 'Authority Score')
  graph_labels = get_graph_labels(case_ids, judicial_df)

  pre_plot_case = function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]
    X = year_interval
    Y = calculate_authority_score_in_range(authority_df, case_id, year_interval)
    onGraphProcessedCallback(X,Y)
    plot_function(X, Y)
  }
  sapply(1:length(case_ids), FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, length(case_ids)), lty = 1:length(case_ids))

}

plot_8 = function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(1861, 1156, 13828, 19238),year_interval =  1850:2000,onGraphProcessedCallback)
}
plot_9 = function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(13828, 1016, 1156, 19238, 19230),year_interval =  1950:1970,onGraphProcessedCallback)
}
plot_10 = function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(18501, 23115, 23601, 26918),year_interval =  1940:2000,onGraphProcessedCallback)
}

plot_8(function(X,Y){})