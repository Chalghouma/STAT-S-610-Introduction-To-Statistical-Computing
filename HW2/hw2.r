#*******************************************************************
#   1) Mutations' number 
#*******************************************************************
is_valid_character = function(char) {
  if (char != "A" && char != "C" && char != "T" && char != "G")
    return(FALSE)
  return(TRUE)
}
is_sequence_valid = function(sequence) {
  for (index in 1:nchar(sequence))
    if (!is_valid_character(substr(sequence, index, index)))
      return(FALSE)
  return(TRUE)
}
are_sequences_comparable = function(seq1, seq2) {
  return(
      nchar(seq1) == nchar(seq2)
      && is_sequence_valid(seq1)
      && is_sequence_valid(seq2)
    )
}
areSequencesIdenticalAt = function(seq1, seq2, index) {
  return(substr(seq1, index, index) == substr(seq2, index, index))
}
number_of_mutations = function(seq1, seq2) {
  if (!are_sequences_comparable(seq1, seq2))
    stop("Sequences are not comparable.")

  n_mutations = 0
  for (index in 1:nchar(seq1))
    if (substr(seq1, index, index) != substr(seq2, index, index)) {
      n_mutations = n_mutations + 1
    }
  return(n_mutations)
}

#*******************************************************************
#   2) Sequence Divergence 
#*******************************************************************
row_sums_to_one = function(row) {
  return(sum(row) == 1)
}
matrix_rows_sum_to_one = function(matrix) {
  for (element in 1:nrow(matrix))
    if (!row_sums_to_one(matrix[element, ]))
      return(FALSE)
  return(TRUE)
}
has_valid_size = function(matrix) {
  return(nrow(matrix) == 4 && ncol(matrix) == 4)
}
non_negative_matrix = function(matrix) {
  for (element in 1:(nrow(matrix) * ncol(matrix)))
    if (matrix[element] < 0)
      return(FALSE)

  return(TRUE)
}
is_valid_matrix = function(matrix) {
  return(has_valid_size(matrix) && non_negative_matrix(matrix) && matrix_rows_sum_to_one(matrix))
}



printf <- function(...) invisible(print(sprintf(...)))
items = c(0.93, 0.05, 0.01, 0.01,
0.05, 0.93, 0.01, 0.01,
0.01, 0.01, 0.93, 0.05,
0.01, 0.01, 0.05, 0.93)
transition_matrix = matrix(items, nrow = 4, byrow = TRUE)

is_valid_matrix(transition_matrix) 