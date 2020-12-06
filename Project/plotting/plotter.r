source('Project/plotting/utils.r')

plot_functions = function(number_of_functions, xLab = 'Year', yLab = 'Authority Score') {
  indexes = 1:number_of_functions
  pre_plot_case = function(index) {
    color = get_color_by_index(index)
    R_plot_function = get_R_plot_function_by_index(index)
    lty = index
    plotting_function = function(X, Y) {
      plot_case(X, Y, R_plot_function, type = 'l', pch = 22, lty = lty, col = color, xLab = xLab, yLab = yLab)
    }

    return(plotting_function)
  }
  return (sapply(indexes, FUN = pre_plot_case))
}

plot_case = function(X, Y, plot_function = plot, type = 'l', pch = 22, lty = 2, col = 'black', xLab = 'Year', yLab = 'Authority Score') {
  plot_function(Y ~ X, xlab = xLab, ylab = yLab, pch = pch, lty = lty, col = col, type = type)
}


functions = plot_functions(1)
functions[[1]](c(1,2,3),c(3,4,5))
# functions = c(function(x) { return(x * 2) })
# functions[[1]](9)