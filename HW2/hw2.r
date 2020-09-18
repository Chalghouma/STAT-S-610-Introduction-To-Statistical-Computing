library(readr)
writefile = function(message){
  write_file("\n","output.txt",TRUE)
  write_file(message,"output.txt",TRUE)
}
write_file("","output.txt")
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

nucleotide_to_index = function(germline) {
  x = c("A", "G", "T", "C")
  return(which(x == germline))
}

equation_three = function(germline, mutated_germline, transition_matrix) {
  if (!are_sequences_comparable(germline, mutated_germline))
    stop("Sequences are not comparable.")

  if (!is_valid_matrix(transition_matrix))
    stop("Matrix is not valid.")

  sum = 0
  for (index in 1:nchar(germline)) {
    # writefile(paste("index = ",index))
    # writefile(paste("T[",mutated_germline,",",germline,"]"))
    # writefile(paste("T[",nucleotide_to_index(mutated_germline),",",nucleotide_to_index(germline),"]"))
    mutated_nucleotide = nucleotide_to_index(substr(mutated_germline,index,index))
    germline_nucleotide = nucleotide_to_index(substr(germline,index,index))
    sum = sum + log(transition_matrix[mutated_nucleotide,germline_nucleotide])
  }

  return(sum)
}


printf <- function(...) invisible(print(sprintf(...)))
items = c(0.93, 0.05, 0.01, 0.01,
0.05, 0.93, 0.01, 0.01,
0.01, 0.01, 0.93, 0.05,
0.01, 0.01, 0.05, 0.93)
transition_matrix = matrix(items, nrow = 4, byrow = TRUE)
transition_matrix



#*******************************************************************
#   3) Read Sequence 
#*******************************************************************
sequences = read.csv("sequences.csv")

#No t calling read_file(to avoid that the germline.txt may not be placed in the test directory)
germline = "GCAGCACCAGATGAACTAGTAAGCGCAGAAGTCTGTCACACTTGAGGAGTTCATAACGATGCGTCAGCGGGCCTGGTTTACAGGCTAGATCTTATATCTAATAAGGACTCTGCGCACCTCGCTATCTTGCCCGACTTTCGCTTCAAAAAGAGGTGGGTGGGAAAATCTCACCCGTTCTCATCGTATCAGTAGCGGTCGCT"

#******************************* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
#   4) Calculate & Report
#*******************************************************************
#*******************************************************************
#   4) a) Calculate Number of mutations, mean & standard deviation for Seq A/B
#*******************************************************************

#We could basically do 1:100 for a and 101:200 for b, but let's suppose the A & B are merged in the .csv
get_sequences_of_type = function(sequences_df, type) {
  indices = which(sequences_df$type==type)
  return(sequences_df$seqs[indices])
}

type_a_sequences = get_sequences_of_type(sequences,"a")
type_b_sequences = get_sequences_of_type(sequences,"b")
  
get_number_of_mutations = function(germline, sequences){
  return (sapply(sequences,FUN = number_of_mutations,germline ))   
}

experimental_condition_type_a_num_mutations = get_number_of_mutations(germline,type_a_sequences)
experimental_condition_type_b_num_mutations = get_number_of_mutations(germline,type_b_sequences) 

experimental_condition_mean_a = mean(experimental_condition_type_a_num_mutations)
experimental_condition_mean_b = mean(experimental_condition_type_b_num_mutations)

experimental_condition_standard_deviation_a = sd(experimental_condition_type_a_num_mutations)
experimental_condition_standard_deviation_b = sd(experimental_condition_type_b_num_mutations)


#*******************************************************************
#   4) b) Usage of likelihood-bases statistic for Seq A/B
#*******************************************************************
get_likelihood_of_mutations = function(germline, transition_matrix, sequences ){
  return (sapply( sequences , FUN= function(sequence){
    equation_three(germline,sequence,transition_matrix)
  } ))
}

transition_matrix = matrix(c(0.93, 0.05, 0.01, 0.01,
          0.05, 0.93, 0.01, 0.01,
          0.01, 0.01, 0.93, 0.05,
          0.01, 0.01, 0.05, 0.93),
   nrow = 4, byrow = TRUE)
statistic_type_a_sequences = get_likelihood_of_mutations(germline,transition_matrix,type_a_sequences)
statistic_type_b_sequences = get_likelihood_of_mutations(germline,transition_matrix,type_b_sequences)

statistic_mean_a = mean(statistic_type_a_sequences)
statistic_mean_b = mean(statistic_type_b_sequences)

statistic_standard_deviation_a = sd(statistic_type_a_sequences)
statistic_standard_deviation_b = sd(statistic_type_b_sequences)

#******************************* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
#   5) Test difference between sequences: [By Number of Mutations]
#*******************************************************************
t.test(experimental_condition_type_a_num_mutations,experimental_condition_type_b_num_mutations)


#******************************* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
#   6) Test difference between sequences: [By Likelihood]
#*******************************************************************
t.test(statistic_type_a_sequences,statistic_type_b_sequences)
