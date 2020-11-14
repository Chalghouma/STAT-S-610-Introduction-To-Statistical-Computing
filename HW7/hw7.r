library(microbenchmark)
# p = c(10, 50, 100, 200, 500)
# p = 10
p = c(10)
n = 6 #TODO: Change to 2000
min = -3
max = 3

#Question 1 - Generate X
generate_normal_dist = function(n) {
  input = seq(min, max, length.out = n)
  return(sapply(input, dnorm, mean = 0, sd = 1))
}


generate_x_dataset = function(iterations_amount, n) {
  generate_for_p = function(iteration, n) {
    return(generate_normal_dist(n))
  }


  return(t(mapply(1:iterations_amount, FUN = generate_for_p, n)))
}

generate_X = function(p, n) {
  X = lapply(p, FUN = generate_x_dataset, n)
  return(X)
}
# X = generate_X(c(10), n)
# print(X)

#Question 2 - Generate Y
generate_beta = function(p) { return(matrix(rep(1, p))) }
generate_y_i = function(Xi, p, n) {
  Xi = t(Xi)
  print(paste('generating beta with p ', p))
  Bi = generate_beta(p)
  return((Xi %*% Bi) + generate_normal_dist(n))
}


# generate_y_i(X[[1]], p[1], n)


generate_Y = function(X, p, n) {
  indices = 1:length(p)
  create_yi = function(indice, X, p, n) {
    Xi = X[[indice]]
    return(generate_y_i(Xi, p, n))
  }
  Y = lapply(indices, FUN = create_yi, X, p, n)
  return(Y)
}

#Question 3 - Generate LogLikelihood - Gradient - Hessian
y_log_likelihood = function(Xi, Bi, yi, n) {
  e1 = -n * log(2 * pi) / 2
  e2 = -n * log(1)
  e3 = -t(yi - Xi %*% Bi) %*% (yi - Xi %*% Bi) / 2
  return(e1 + e2 + e3)
}

y_gradient_log_likelihood = function(Xi, Bi, yi) {
  return(t(Xi) %*% (yi - Xi %*% Bi))
}

y_hessian = function(Xi) {
  return(-t(Xi) %*% Xi)
}

#Question 4 - Newton step - Maximize Likelihood
newton_step = function(Xi, Bi, yi) {
  return(solve(t(Xi) %*% Xi) %*% t(Xi) %*% yi)
}


#Question 5 - Gradient descent with Back tracking
gradient_step = function(Xi, Bi, yi, step) {
  gradient = y_gradient_log_likelihood(Xi, Bi, yi)
  return(Xi - (step * gradient))
}

#Question 6 - Newthon's Method
newthon_method = function(Xi, Bi, yi) {
  current_theta = Bi
  current_theta = newton_step(Xi, current_theta, yi)
  return(current_theta)
}

#Question 7 - Gradient Descient with Backtracking
backtracking_line_search = function(Xi, Bi, Yi, gradient, param) {
  t = 1
  vector_norm = function(vector) {
    return(sqrt(sum(vector ^ 2)))
  }
  while ((Yi - gradient) >= Yi - (t / 2) * vector_norm(gradient))
    t = param * t

  return(t)
}



#Question 8 - Bencharmking Newton

X = generate_X(p, n)
Y = generate_Y(X, p, n)
print(length(X))

benchmark_dataset = function(index, X, p, n) {
  Xi = X[[index]]
  Yi = generate_y_i(Xi, p[index], n)
  Bi = generate_beta(p[index])

  newthon_method_benchmark = microbenchmark::microbenchmark(newthon_meth = newthon_method(Xi, Bi, Yi))
  print('=======BENCHMARK=========')
  print(paste('Processing Dataset of indiex: ', index))
  print(newthon_method_benchmark)
}


process_dataset_newton = function(index, X, p, n) {
  Xi = X[[index]]
  Yi = generate_y_i(Xi, p[index], n)
  Bi = generate_beta(p[index])


  tic = proc.time()[3]
  print('=======Processing Time=========')
  print(paste('Processing Dataset of index: ', index))
  new_beta = newthon_method(Xi, Bi, Yi);
  print(new_beta)
  return(proc.time()[3] - tic)
}



#Benchmarking - Newton Method
for (indice in 1:length(p)) {
  sapply(1:length(p), FUN = benchmark_dataset, X, p, n)
}

#Plotting Time taken - Newton Method
time_taken_newthon = sapply(1:length(p), FUN = process_dataset_newton, X, p, n)
plot(time_taken_newthon ~ p, type = 'l')
plot(time_taken_newthon ^ (1 / 3) ~ p, type = 'l')



#Question 11 - Plotting Gradient

process_dataset_gradient = function(index, X, p, n) {
  Xi = X[[index]]
  Yi = generate_y_i(Xi, p[index], n)
  Bi = generate_beta(p[index])


  tic = proc.time()[3]
  print('=======Processing Time=========')
  print(paste('Processing Dataset of index: ', index))
  new_beta = newthon_method(Xi, Bi, Yi);
  print(new_beta)
  return(proc.time()[3] - tic)
}
#Plotting Time taken - Newton Method
time_taken_gradient = sapply(1:length(p), FUN = process_dataset_gradient, X, p, n)
plot(time_taken_gradient ~ p, type = 'l')



