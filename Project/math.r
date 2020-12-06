library(RSpectra)
elements = c(0, 0, 1, 0, 0, 0, 0,
            0, 1, 1, 0, 0, 0, 0,
            1, 0, 1, 2, 0, 0, 0,
            0, 0, 0, 1, 1, 0, 0,
            0, 0, 0, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 1, 1,
            0, 0, 0, 2, 1, 0, 1)

A = matrix(elements, 7, byrow = TRUE)
h = rep(1, 7)

A= matrix(c(1,1,1,1,0,1,0,1,0),3,byrow=TRUE)
h=rep(1,3)


A
eg = eigen(A %*% t(A))
lev = eg$values[1]
lev



h2 = (1 / lev) * A %*% t(A) %*% h

factored = (1 / lev) * A %*% t(A)
iterate = function(input_h) {
  return(normalize(factored %*% input_h))
  # return ((1/lev )*A %*% t(A) %*% input_h)
}
normalize = function(x) { x / sqrt(sum(x ^ 2)) }
for (x in 1:100)
  h = normalize(iterate(h))

print(h2)
print(normalize(h2))
eg$vectors[, 1]

eg$values[1]






eg$vectors[, 1] / (-0.06742001 / 0.03)

# main_vector = eigen(A%*%t(A))
# main_vector
# normalize(h2)
# x = normalize(h)
# x
# normalize(h)
vector_magnitude = function(x) {
  sum = 0
  for (index in 1:length(x))
    sum = sum + x[index]
  return(sum)
}
vector_normalized = function(x) {
  magnitude = vector_magnitude(x)
  scaledown = function(item) {
    return(item / magnitude)
  }
  return(sapply(x, FUN = scaledown))

}

vector_normalized(c(1, 2, 1))


normalize_vector = function(x) { x / sqrt(sum(x ^ 2)) }
vector_normalized(eg$vectors[, 1] )
eigsResult = eigs(A%*% t(A),k=1,which='LM')
eg$values
print('EIGS')
eigsResult$values
eigsResult$vectors[ ,1]


