library(readr)
# Homework 4: Functions to be debugged

# 1. Berhu penalty

berhu <- function(x, delta) {
   if(abs(x) < delta) {
       abs(x)
   } else {
       (x^2) / (2 * delta) + delta / 2
   }
}

berhu(1,1) ## should be 1
berhu(.5, 1) ## should be .5
berhu(2, 1) ## should be 2.5
xseq = seq(-3, 3, length.out = 200)
plot(sapply(xseq, berhu, 1) ~ xseq, type = 'l')
return()

# 2. Trimmed mean

trimmed_mean <- function(x, trim) {
    #Changed the (1-trim)/2 to (1-trim)
    qlo = quantile(x, probs = (1 - trim))
    qhi = quantile(x, probs = 1 - (1 - trim))

    process_element_of_array= function(element){
        if(element <= qlo && element >= qhi)
            return(element)
        return(NULL)
    }

    within_range = sapply(x,FUN=process_element_of_array)
    without_nulls = plyr::compact(within_range)
    return(mean(unlist(without_nulls)))
}
## the following two should give the same results
tm = trimmed_mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
print('the trimmed mean is')
print(tm)
mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
return()
## 3.

# ## 4. Gradient descent
# gradient_descent <- function(fn, deriv, start, step_size, epsilon) {
#   x = start
#   while (TRUE) {
#     #We used not to calculate the new_x correctly
#     # new_x = x + step_size * deriv(x)
#     new_x = x - step_size * deriv(x)

#     #We used not to calculate the difference correctly too
#     # if (abs(deriv(new_x)) <= epsilon) {
#     difference = abs(fn(new_x) - fn(x))
#     if (difference <= epsilon) {
#       break
#     }
#     x = new_x
#   }
#   return(x)
# }

# ## should return something close to 0
# gradient_descent(function(x) x ^ 2, function(x) 2 * x, start = 1,
#                  step_size = 0.1, epsilon = 1e-10)

# ## 5. Line search
# backtrack_desc <- function(fn, deriv, start, alpha, beta, epsilon) {
#   x = start
#   while (TRUE) {
#     step_size = backtrack(fn, deriv, x, alpha, beta)
#     # #wrong calculation of new_x
#     # new_x = fn(x) - step_size * deriv(x)
#     new_x = x - step_size * deriv(x)

#     #wrong if statement
#     # if(abs(deriv(new_x)) <= epsilon) {
#     difference = abs(fn(new_x)- fn(x))
#     if (difference <= epsilon) {
#       break
#     }
#     x = new_x
#   }
#   return(x)
# }

# backtrack <- function(fn, deriv, x, alpha, beta) {
#   t = 1
#   while (TRUE) {
#     first = fn(x - t * deriv(x))
#     second = (fn(x) - alpha * t * (deriv(x) ^ 2))
#     if (first < second) {
#       break
#     }
#     t = beta * t
#   }
#   return(t)
# }

# ## should return something close to 0
# backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 10,
#                alpha = .03, beta = .8, epsilon = 1e-10)
# backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 1,
#                alpha = .03, beta = .8, epsilon = 1e-10)