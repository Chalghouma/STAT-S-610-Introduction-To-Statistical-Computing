p = c(10, 50, 100, 200, 500)
p = 10
n = 6
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
X = generate_X(c(10), n)
print(X)

#Question 2 - Generate Y
generate_beta = function(p) { return(matrix(rep(1, p))) }
generate_y_i = function(Xi, p, n) {
  Xi = t(Xi)
  Bi = generate_beta(p)
  return((Xi %*% Bi) + generate_normal_dist(n))
}


generate_y_i(X[[1]], 10, n)


generate_Y = function(X,p,n){

}

#Question 3 - Generate LogLikelihood - Gradient - Hessian
y_log_likelihood = function(Xi, Bi,yi, n){
    e1 = -n * log(2*pi)/2
    e2 = -n * log(1)
    e3 = - t(yi - Xi %*% Bi) %*% (yi - Xi %*% Bi )/2
    return (e1+e2+e3)
}

y_gradient_log_likelihood = function(Xi, Bi, yi ){
    return (t(Xi)  %*% (yi - Xi %*% Bi) )
}

y_hessian = function(Xi){
    return (-t(Xi) %*% Xi)
}




# x_vec = seq(-3, 3, length.out = n)
# fx = generate_normal_dist(n)
# print(fx)
# print(generate_x_dataset(n,p[1]))
# plot(fx ~ x_vec, type = 'l')
