## 4. Gradient descent
gradient_descent <- function(fn, deriv, start, step_size, epsilon) {
  x = start
  while (TRUE) {
    #We used not to calculate the new_x correctly
    # new_x = x + step_size * deriv(x)
    new_x = x - step_size * deriv(x)

    #We used not to calculate the difference correctly too
    # if (abs(deriv(new_x)) <= epsilon) {
    difference = abs(fn(new_x) - fn(x))
    if (difference <= epsilon) {
      break
    }
    x = new_x
  }
  return(x)
}

## should return something close to 0
gradient_descent(function(x) x ^ 2, function(x) 2 * x, start = 1,
                 step_size = 0.1, epsilon = 1e-10)
