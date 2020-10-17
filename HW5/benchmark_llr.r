library(microbenchmark)
source('./HW5/llr_functions.r')
# --- example 1 --- #

# get the data
data(french_fries, package = 'reshape2')
french_fries <- na.omit(french_fries)

# input data
x <- french_fries$potato
y <- french_fries$buttery

# space along which to smooth
z <- seq(0, 15, length.out = 100)

llr_benchmark = microbenchmark::microbenchmark(llr_function= llr(z = z, x = x, y = y, omega = 2) )
print(llr_benchmark)


# --- example 2 --- #

# noisy sine wave
x <- runif(1000, -2 * pi, 2 * pi)
y <- sin(x) + rnorm(length(x))

# space along which to smooth
z <- seq(-2 * pi, 2 * pi, length.out = 100)

# run smoothing
fits <- llr(z = z, x = x, y = y, omega = pi / 3)

llr_benchmark = microbenchmark::microbenchmark(llr_function= llr(z = z, x = x, y = y, omega = pi / 3) )
print(llr_benchmark)