source('Project/source_paths.r')
source(get_selector_file_path())
# source('Project/ws.r')
source(get_plotter_file_path())
source(get_reader_file_path())
library(readr)
library(jsonlite)

calculate_authority_score_in_range = function(generated_authority_df, case_id,year){

}
plot_6 = function(onGraphProcessedCallback) {
  case_ids = c(21109, 25347)
  plotting_functions = get_plot_functions(length(case_ids), xLab = 'Years after Decision', yLab = 'Authority Score')
  authority_df = read_authorities_df_from_year_interval()
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

plot_figure = function(case_ids,year_interval,onGraphProcessedCallback){ 
  authority_df = read_auth()
  judicial_df = read_judicial_data()
  plotting_functions = get_plot_functions(length(case_ids), xLab = 'Year', yLab = 'Authority Score')
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
