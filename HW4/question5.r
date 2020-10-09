## 5. Line search
backtrack_desc <- function(fn, deriv, start, alpha, beta, epsilon) {
  x = start
  while (TRUE) {
    step_size = backtrack(fn, deriv, x, alpha, beta)
    # #wrong calculation of new_x
    # new_x = fn(x) - step_size * deriv(x)
    new_x = x - step_size * deriv(x)

    #wrong if statement
    # if(abs(deriv(new_x)) <= epsilon) {
    difference = abs(fn(new_x)- fn(x))
    if (difference <= epsilon) {
      break
    }
    x = new_x
  }
  return(x)
}

backtrack <- function(fn, deriv, x, alpha, beta) {
  t = 1
  while (TRUE) {
    first = fn(x - t * deriv(x))
    second = (fn(x) - alpha * t * (deriv(x) ^ 2))
    if (first < second) {
      break
    }
    t = beta * t
  }
  return(t)
}

## should return something close to 0
backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 10,
               alpha = .03, beta = .8, epsilon = 1e-10)
backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 1,
               alpha = .03, beta = .8, epsilon = 1e-10)