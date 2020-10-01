#install.packages('reshape2')
library(readr)
library(reshape2)

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

head(append_measure_activity(members_initial_df,bills_initial_df))