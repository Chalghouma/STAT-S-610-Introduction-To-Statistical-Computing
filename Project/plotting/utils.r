colors = c('black', 'gray', 'blue', 'red', 'green')


get_color_by_index <- function(index) {
  return(colors[index])
}

get_R_plot_function_by_index <- function(index) {
  if (index == 1) plot_function = plot
  else plot_function = lines
}