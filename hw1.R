install.packages("readr")
library(readr)

#We initialize the arrays
titles <- vector(mode = "character", length = 21)
n_words <- vector(mode = "numeric", length = 21)
n_chars <- vector(mode = "numeric", length = 21)
n_individuals <- vector(mode = "numeric", length = 21)
frame <- data.frame(titles, n_words, n_chars, n_individuals)

books <- c("1289-0.txt", "1400-0.txt", "564-0.txt", "580-0.txt", "653-0.txt", "675-0.txt", "678-0.txt", "700-0.txt", "766-0.txt", "786-0.txt", "821-0.txt", "882-0.txt", "883-0.txt", "917-0.txt", "963-0.txt", "967-0.txt", "968,0.txt", "98-0,txt", "pg1023.txt", "pg19337.txt", "pg730.txt")
