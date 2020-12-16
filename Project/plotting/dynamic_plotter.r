source('Project/source_paths.r')
source(get_selector_file_path())
source(get_hubs_authorities_io_file_path())
source(get_plotter_file_path())
source(get_reader_file_path())
library(readr)
library(jsonlite)

plot_dynamic_figure_8 <- function(onGraphProcessedCallback) {
  plot_figure(c(1861, 1156, 13828, 19238), 1850:1960, onGraphProcessedCallback)
}
plot_dynamic_figure_9 <- function(onGraphProcessedCallback) {
  plot_figure(c(13828, 1016, 1156, 19238, 19230), 1950:1964, onGraphProcessedCallback)
}
plot_dynamic_figure_10 <- function(onGraphProcessedCallback) {
  plot_figure(c(18501), 1940:1960, onGraphProcessedCallback)
}


plot_figure <- function(case_ids, year_interval, onGraphProcessedCallback) {

  authority_df = read_authorities_df_from_year_interval(1850:1965)
  judicial_df = read_judicial_data()
  plotting_functions = get_plot_functions(length(case_ids), xLab = 'Year', yLab = 'Authority Score')
  graph_labels = get_graph_labels(case_ids, judicial_df)

  pre_plot_case <- function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]

    X = year_interval
    Y = sapply(year_interval, FUN = get_authority_from_generated_authorities_df, case_id, authority_df)
    #The RSpectra generates negative eigen vectors. By the time I had realized,
    #the data was already stored
    onGraphProcessedCallback(X,-Y)
    plot_function(X, - Y)
  }
  sapply(1:length(case_ids), FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  legend(20, 0.01, graph_labels, cex = 0.8, col = colors, pch = rep(21, length(case_ids)), lty = 1:length(case_ids))

}

plot_dynamic_figure_6 <- function(case_ids = c(18501), onGraphProcessedCallback) {
  plotting_functions = get_plot_functions(length(case_ids), xLab = 'Years after Decision', yLab = 'Authority Score')
  authority_df = read_authorities_df_from_year_interval(1850:1965)
  judicial_df = read_judicial_data()
  graph_labels = get_graph_labels(case_ids, judicial_df)

  pre_plot_case <- function(index, authority_df, case_ids, onGraphProcessedCallback) {
    plot_function = plotting_functions[[index]]
    case_id = case_ids[index]

    year_of_decision = get_case_data_by_id(case_ids[index], judicial_df)$year
    years_after_decision = 29
    year_interval = year_of_decision:(year_of_decision + years_after_decision)
    X = 0:years_after_decision
    Y = sapply(year_interval, FUN = get_authority_from_generated_authorities_df, case_id, authority_df)
    onGraphProcessedCallback(X,-Y)
    
    plot_function(X, - Y)
  }
  sapply(1:length(case_ids), FUN = pre_plot_case, authority_df, case_ids, onGraphProcessedCallback = onGraphProcessedCallback)
  # legend(20, 0.01, graph_labels, cex = 0.8, col = colors, pch = rep(21, 4), lty = 1:length(case_ids))
}