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




onWSReadyCallback = function(ws) {
  print('onWSReady')
  onGraphProcessedCallback = function(year_interval, data) {
    print('onGraphProcessedCallback')
    ws$send(serializeJSON(list(year_interval = year_interval, data = data)))
  }
  plot_figure_10(onGraphProcessedCallback)
}
# launch_server(onWSReadyCallback)
