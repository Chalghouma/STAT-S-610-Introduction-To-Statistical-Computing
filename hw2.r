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

number_of_mutations("AGTTC", "TGTCC")
