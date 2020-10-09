## Homework 4: Functions to be debugged

## 1. Berhu penalty

#berhu <- function(x, delta) {
#    if(abs(x) < delta) {
#        abs(x)
#    } else {
#        (x^2) / (2 * delta) + delta / 2
#    }
#}
#
#berhu(1,1) ## should be 1
#berhu(.5, 1) ## should be .5
#berhu(2, 1) ## should be 2.5
#xseq = seq(-3, 3, length.out = 200)
#plot(sapply(xseq, berhu, 1) ~ xseq, type = 'l')
#return()

## 2. Trimmed mean

trimmed_mean <- function(x, trim) {
    qlo = quantile(x, probs = (1 - trim) )
    qhi = quantile(x, probs = 1 - (1 - trim) )
    # cat('qlo = ',qlo)
    # cat('qhi = ',qhi)
    # print(qlo)
    # print(qhi)
    lo_idx = x <= qlo
    hi_idx = x >= qhi
    # print('Low indx')
    # print(lo_idx)
    # print('High indx')
    # print(hi_idx)
    # print(length(lo_idx))
    # print(length(hi_idx))
    x_trimmed = x
    # cat('x_trimmed = x = ',x_trimmed)
    x_trimmed = x[!lo_idx]
    # cat('\n x_trimmed= x[!lo_idx] = ',x_trimmed)
    x_trimmed = x_trimmed[!hi_idx]
    # cat('\n x_trimmed= x[!hi_idx] = ',x_trimmed)
    # combined = (!lo_idx) &&  ( !hi_idx)
    # combined
    # x_trimmed = x[combined]
    # print('printing')
    # print(x[1])
    trimmed = c()
    for(i in 1:length(lo_idx))
        if(lo_idx[i] && hi_idx[i])
        {
            trimmed = append(trimmed,x[i])
        }
    # trimmed = x[ x <= qlo && x>= qhi]
    return(mean(trimmed))
    # return(mean(x_trimmed))
}
## the following two should give the same results
tm = trimmed_mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
print('the trimmed mean is')
print(tm)
mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
return()