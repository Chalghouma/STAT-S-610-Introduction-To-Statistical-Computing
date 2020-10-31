log_likelihood_function = function(X , theta){
    l <- function(xi,theta){
        return (- ( log(pi) + log(1+(xi - theta)^2)))
    }
    return (sum(sapply(X, FUN = l, theta )))
}

derivative_log_likelihood = function(X,theta){
    f <- function(xi,theta){
        return (2*(xi-theta) / (1 + (xi-theta)^2))
    }
    return (sum(sapply(X, FUN=f,  theta)))
}

second_derivative_log_likelihood = function(X,theta){
    f <- function(xi,theta){
        return (  ((4 * (xi - theta)^2)/(1+ (xi-theta)^2)) - 2/(1+ (xi-theta)^2))
    }
    return (sum(sapply(X,FUN=f, theta)))
}

maximum_likelihood_estimate = function(X, starting_theta, criterion){
    should_stop_searching = function(X, current_theta, criterion){
        return (derivative_log_likelihood(X,current_theta) < criterion)
    }

    current_theta = starting_theta
    while(!should_stop_searching(X,current_theta,criterion)){
        current_theta = current_theta - derivative_log_likelihood(X,current_theta) / second_derivative_log_likelihood(X,current_theta)
    }

    return (current_theta)
}

report_mle_results = function(X, starting_thetas , criterion){
    for(i in 1:length(starting_thetas)){
        mle = maximum_likelihood_estimate(X,starting_thetas[i] , criterion)
        print(paste('MLE on Theta = ',starting_thetas[i], ' = ', mle ))
    }
}


data = c(−2.09 , −2.68, −1.92, −1.76, −2.12, 2.21, 1.97, 1.61, 1.99, 2.18)
thetas = c(-2,-1,0,1,2)
# report_mle_results( data , thetas , 0.1 )

one_step_estimation = function(X){
    theta = median(X)
    return (theta - derivative_log_likelihood(X,theta) / second_derivative_log_likelihood(X,theta))
}
