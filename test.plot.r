source('Project/source_paths.r')
library(testthat)
source(get_dynamic_plotter_file_path())
source(get_static_plotter_file_path())

postProcessedCallback <- function(X, Y) { }


test_that('Can plot static Figures 6/8/9/10', {
  plot_static_figure_6(postProcessedCallback)
  plot_static_figure_8(postProcessedCallback)
  plot_static_figure_9(postProcessedCallback)
  plot_static_figure_10(postProcessedCallback)
})

test_that('Can plot dynamic Figures 6/8/9/10', {
  plot_static_figure_6(postProcessedCallback)
  plot_static_figure_8(postProcessedCallback)
  plot_static_figure_9(postProcessedCallback)
  plot_static_figure_10(postProcessedCallback)
})
