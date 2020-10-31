log_likelihood_function = function(X , theta){
    l <- function(xi,theta){
        return (- ( log(pi) + log(1+(xi - theta)^2)))
    }
    return (sum(sapply(X, FUN = l, theta )))
}

derivative_log_likelihood = function(X,theta){
    f <- function(xi,theta){
        return (2*(xi-theta) / (1 + (x-theta)^2))
    }
    return (sum(sapply(X, FUN=f,  theta)))
}

second_derivative_log_likelihood = function(X,theta){
    f <- function(xi,theta){
        return (  ((4 * (xi - theta)^2)/(1+ (xi-theta)^2)) - 2/(1+ (xi-theta)^2))
    }
    return (sum(sapply(X,FUN=f, theta)))
}

second_derivative_log_likelihood(1:10,0.1)