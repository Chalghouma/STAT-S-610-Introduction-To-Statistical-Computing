install.packages("readr")
library(readr)

#We initialize the arrays
titles <- vector(mode = "character", length = 21)
n_words <- vector(mode = "numeric", length = 21)
n_chars <- vector(mode = "numeric", length = 21)
n_individuals <- vector(mode = "numeric", length = 21)
frame <- data.frame(titles, n_words, n_chars, n_individuals)

book_file_names <- c("1289-0.txt", "1400-0.txt", "564-0.txt", "580-0.txt", "653-0.txt", "675-0.txt", "678-0.txt", "700-0.txt", "766-0.txt", "786-0.txt", "821-0.txt", "882-0.txt", "883-0.txt", "917-0.txt", "963-0.txt", "967-0.txt", "968-0.txt", "98-0.txt", "pg1023.txt", "pg19337.txt", "pg730.txt")

#We go through the books
index <- 1
for (book_file_name in book_file_names) {
  file_path = paste("books/", book_file_name, sep = "")
  book_text = read_file(file_path)

  #We get the title
  title_regex = "Title:( [A-z]+)+"

  title <- regmatches(book_text, regexpr(title_regex, book_text))
  #We remove the "Title: " from the matched value 
  frame[index, 1] <- substring(title, 8)

  #We get the word count
  splitted <- strsplit(book_text, " ")
  splitted
  frame[index, 2] <- length(splitted[[1]])

  #We get the characters' count
  frame[index, 3] <- nchar(book_text)

  #We get the individuals' names (as honorifics)
  #The Mr. Mrs. Dr. Ms. Miss. is dealt with (Mr)|(Mrs)|(Dr)|(Ms)|(Miss))\.
  #For the " E.", " S.", we use ( [A-Z]\.) that we repeat several 0-N times (the name doesn't necessairly have that)
  #As for the normal first/middle/last names, we can be sure that even if placed in the middle of a sentence
  #there's no way the word that comes after starts with a capital letter. which is why we're using the [A-Z] constraint right after the space
  #To get a chunk of the name we use this: ( [A-Z]\.)*( [A-Z][a-z]*) that we repeat at least once.
  #Lastly, we use the \\. instead of \. since we're defining it inside a string
  individual_honorific_regex <- "((Mr)|(Mrs)|(Dr)|(Ms)|(Miss))\\.( [A-Z]\\.)*( [A-Z][a-z]*)+"
  names <- regmatches(book_text, gregexpr(individual_honorific_regex, book_text))
  
  #We print & store the unique names' info
  unique_names <-unique(names[[1]])
  unique_names
  frame[index,4] <- length(unique_names)
  index <- index + 1
}

frame