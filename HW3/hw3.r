#install.packages('reshape2')
library(readr)
library(reshape2)
library(dplyr)

bills_initial_df = read_csv('HW3/bills.csv')
votes_initial_df = read_csv('HW3/votes.csv')
members_initial_df = read_csv('HW3/members.csv')

get_last_names_from_members = function(members_df) {
  return(members_df[, 4])
}
get_bills_subjects = function(bills_df) {
  return(bills_df[, 6])
}

get_subject_contains_name = function(subject, last_name) {
  stringr::str_detect(subject, last_name)
}

occurences_of_last_name = function(subjects, last_name) {
  count = 0
  for (i in 1:length(subjects)) {
    if (get_subject_contains_name(subjects[i], last_name))
      count = count + 1
  }
  return(count)
}
occurences_of_members_in_bills = function(subjects, last_names) {
  occurences = c()
  for (index in 1:nrow(members_initial_df)) {
    last_name = last_names[index]
    occurence = occurences_of_last_name(bills_subjects, last_name)
    occurences = append(occurences, occurence)
  }
  return(occurences)
}

append_measure_activity = function(members_df, bills_df) {
  last_names = get_last_names_from_members(members_initial_df)$last_name
  bills_subjects = get_bills_subjects(bills_initial_df)$subject
  measure_of_activity = occurences_of_members_in_bills(bills_subjects, last_names)
  return(cbind(members_df, measure_of_activity))
}
#head(append_measure_activity(members_initial_df,bills_initial_df))


#************************************************
#Question 3) 
#************************************************
get_number_of_Yea = function(vote_vector) {
  count = 0
  for (x in 1:length(vote_vector)) {
    if (!is.na(vote_vector[x]) && vote_vector[x] == "Yea")
      count = count + 1
  }

  return(count)

  #Shorter but includes NA
  yea = vote_vector[vote_vector == "Yea"]
  return(length(yea))
}

get_bill_ids = function(votes_df) {
  return(colnames(votes_df)[-1])
}

get_bills_number_of_Yea_df = function(votes_df) {
  bill_ids = get_bill_ids(votes_df)

  numbers = c()
  for (index in 1:length(bill_ids)) {
    vote_vector = votes_df[bill_ids[index]][[1]]
    # vote_vector = votes_df[,index+1][[1]]
    number_of_yea = get_number_of_Yea(vote_vector)
    numbers = append(numbers, number_of_yea)
  }
  return(data.frame(bill_ids,numbers))
}

#head(get_bills_number_of_Yea_df(votes_initial_df))
#************************************************
#Question 4) 
#************************************************
string_fraction_to_number= function(stringified_fraction,total_number_of_votes){
    splitted = unlist(strsplit(stringified_fraction, "/"))
    return (as.integer(as.numeric(splitted[1]) / as.numeric(splitted[2])*total_number_of_votes))
}
get_required_votes_array = function(stringified_vector,total_number_of_votes){
    return (lapply(stringified_vector,FUN = string_fraction_to_number,total_number_of_votes))
}

calculate_and_append_required_votes = function(bills_df,total_number_of_votes){
    required_stringified_colun = bills_df[,4]$requires
    bills_df$calculated = unlist(get_required_votes_array(required_stringified_colun,total_number_of_votes))
    return(bills_df)
}

head(calculate_and_append_required_votes(bills_initial_df,101))
