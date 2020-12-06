source('Project/plotting/utils.r')

get_plot_functions = function(number_of_functions, xLab = 'Year', yLab = 'Authority Score') {
  indexes = 1:number_of_functions
  pre_plot_case = function(index) {
    color = get_color_by_index(index)
    R_plot_function = get_R_plot_function_by_index(index)
    lty = index
    plotting_function = function(X, Y) {
      display_function(X, Y, plot_function=R_plot_function, type = 'l', pch = 22, lty = lty, col = color, x_label = xLab, y_label = yLab)
    }

    return(plotting_function)
  }
  return (sapply(indexes, FUN = pre_plot_case))
}

display_function = function(X, Y, plot_function = plot, type = 'l', pch = 22, lty = 2, col = 'black', x_label,y_label) {
  plot_function(Y ~ X, xlab = x_label, ylab = y_label, pch = pch, lty = lty, col = col, type = type)
}
