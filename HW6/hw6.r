log_likelihood_function = function(X, theta) {
  l <- function(xi, theta) {
    return(-(log(pi) + log(1 + (xi - theta) ^ 2)))
  }
  return(sum(sapply(X, FUN = l, theta)))
}

derivative_log_likelihood = function(X, theta) {
  f <- function(xi, theta) {
    return(2 * (xi - theta) / (1 + (xi - theta) ^ 2))
  }
  return(sum(sapply(X, FUN = f, theta)))
}

second_derivative_log_likelihood = function(X, theta) {
  f <- function(xi, theta) {
    return(((4 * (xi - theta) ^ 2) / (1 + (xi - theta) ^ 2)) - 2 / (1 + (xi - theta) ^ 2))
  }
  return(sum(sapply(X, FUN = f, theta)))
}

maximum_likelihood_estimate = function(X, criterion) {
  should_stop_searching = function(X, current_theta, criterion) {
    return(derivative_log_likelihood(X, current_theta) < criterion)
  }

  current_theta = median(X)
  while (!should_stop_searching(X, current_theta, criterion)) {
    current_theta = current_theta - derivative_log_likelihood(X, current_theta) / second_derivative_log_likelihood(X, current_theta)
  }

  return(current_theta)
}
report_mle_results = function(X, starting_thetas, criterion) {
  for (i in 1:length(starting_thetas)) {
    mle = maximum_likelihood_estimate(X, starting_thetas[i], criterion)
    print(paste('MLE on Theta = ', starting_thetas[i], ' = ', mle))
  }
}


data = c(−2.09, −2.68, −1.92, −1.76, −2.12, 2.21, 1.97, 1.61, 1.99, 2.18)
thetas = c(-2, -1, 0, 1, 2)
# report_mle_results( data , thetas , 0.01 )
one_step_estimation = function(X) {
  theta = median(X)
  return(theta - derivative_log_likelihood(X, theta) / second_derivative_log_likelihood(X, theta))
}

compare_methods_on_distribution = function(n, repetitions, criterion) {
  X = rcauchy(n, 0, 1)
  values_with_mle = c()
  values_with_one_step = c()
  for (iteration in 1:repetitions) {
    # if(n==1000)
    # print(paste('N =  (', n, ') iteration = ', iteration))
    values_with_mle = append(values_with_mle, maximum_likelihood_estimate(X, criterion))
    values_with_one_step = append(values_with_one_step, one_step_estimation(X))
  }
  print('done')
  print(paste('Variance With MLE Method over (N=', n, ',iterations=',repetitions,') = ', var(values_with_mle)))
  print(paste('Variance With OneStepIteration Method over (N=', n, ',iterations=',repetitions,') = ', var(values_with_one_step)))
}


repetitions = 10
criterion = 0.0000001
n_values = c(10,100, 1000)
sapply(n_values, FUN = compare_methods_on_distribution, repetitions, criterion)

