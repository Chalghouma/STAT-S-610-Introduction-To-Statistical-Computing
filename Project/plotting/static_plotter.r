source('Project/source_paths.r')
source(get_selector_file_path())
source(get_plotter_file_path())
source(get_reader_file_path())


plot_static_figure_8 <- function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(1861, 1156, 13828, 19238),year_interval =  1850:2000,onGraphProcessedCallback)
}
plot_static_figure_9 <- function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(13828, 1016, 1156, 19238, 19230),year_interval =  1950:1970,onGraphProcessedCallback)
}
plot_static_figure_10 <- function(onGraphProcessedCallback){
  plot_figure(  case_ids = c(18501, 23115, 23601, 26918),year_interval =  1940:2000,onGraphProcessedCallback)
}

plot_figure <- function(case_ids,year_interval,onGraphProcessedCallback){ 
  authority_df = read_auth()
  judicial_df = read_judicial_data()

  n_case_ids= length(case_ids)

  plotting_functions = get_plot_functions(n_case_ids, xLab = 'Year', yLab = 'Authority Score')
  graph_labels = get_graph_labels(case_ids, judicial_df)

  pre_plot_case <- function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]
    X = year_interval
    Y = calculate_authority_score_in_range(authority_df, case_id, year_interval)
    onGraphProcessedCallback(X,Y)
    plot_function(X, Y)
  }
  sapply(1:n_case_ids, FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(year_interval[1], 0.06, graph_labels, cex = 0.8, col = colors, pch = rep(21, n_case_ids), lty = 1:n_case_ids)

}

#Function 6 is different than 8-10 since we display the Years after Decision, rather than the typical year_interval
#which means
plot_static_figure_6 <- function(onGraphProcessedCallback) {
  case_ids = c(21109, 25347)

  n_case_ids = length(case_ids)
  plotting_functions = get_plot_functions(n_case_ids, xLab = 'Years after Decision', yLab = 'Authority Score')
  authority_df = read_auth()
  judicial_df = read_judicial_data()

  graph_labels = get_graph_labels(case_ids, judicial_df)
  pre_plot_case <- function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]

    year_of_decision = get_case_data_by_id(case_ids[index], judicial_df)$year
    years_after_decision = 30
    year_interval = year_of_decision:(year_of_decision + 30)
    X = 0:years_after_decision
    Y = calculate_authority_score_in_range(authority_df, case_id, year_interval)
    plot_function(X, Y)
  }
  sapply(1:n_case_ids, FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(20, 0.01, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = 1:n_case_ids)
}