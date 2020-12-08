library(RSpectra)
source('Project/source_paths.r')
source(get_selector_file_path())
source(get_hubs_authorities_io_file_path())
source(get_reader_file_path())
library(Matrix)
library(tidyverse)
library(spam)

generate_matrix <- function(until_year, csv_path = 'Project/data/judicial.csv', all_cities_path = 'Project/data/allcites.txt') {
  judicial_df = read_judicial_data()
  case_ids_until_year = get_cases_until(judicial_df, until_year)$caseid
  dimension = length(case_ids_until_year)
  print(dimension)
  m = matrix(rep(0, dimension * dimension), nrow = dimension)
  lines = readLines(all_cities_path)
  lines_length = length(lines)
  line_index = 1
  latest_case_id = length(case_ids_until_year)
  has_surpassed_latest_case_id = FALSE
  while (line_index < lines_length & !has_surpassed_latest_case_id) {
    processed_line = parse_line(lines[line_index])
    if (processed_line$citing > latest_case_id) {
      has_surpassed_latest_case_id = TRUE
    } else {
      m[processed_line$citing, processed_line$cited] = m[processed_line$citing, processed_line$cited] + 1
      line_index = line_index + 1
      has_surpassed_latest_case_id
    }
  }
  return(m)
}



write_authority_hub_vectors <- function(year, should_write_hub = TRUE, should_write_authority = TRUE) {
  get_largest_eigen_vector <- function(matrix) {
    return(eigs(multiplied, k = 1, which = 'LM')$vectors[, 1])
  }
  print('Generating A')
  A = generate_matrix(year)
  print('A generated')
  sparse_A = Matrix(A, sparse = T)
  print('sparse_A generated')
  sparse_tA = Matrix(t(sparse_A), sparse = T)
  print('sparse_tA generated')

  if (should_write_hub) {
    multiplied = sparse_A %*% sparse_tA
    write_hubs(get_largest_eigen_vector(multiplied), year)
  }
  if (should_write_authority) {
    print('generating multiplied')
    multiplied = sparse_tA %*% sparse_A
    print('writing authoritiesee multiplied')
    write_authorities(get_largest_eigen_vector(multiplied), year)
  }
}


compute_authority <- function(matrix) {
  sparse_A = Matrix(matrix, sparse = T)
  print('sparse_A calculated')
  sparse_tA = Matrix(t(sparse_A), sparse = T)
  print('sparse_tA calculated')

  multiplied = sparse_A %*% sparse_tA
  print('multiplied calculated')
  eigen_spectra = eigs(multiplied, k = 1, which = 'LM')
  print('eigs calculated')
  return(eigen_spectra$vectors[, 1])
  # eigen_data = eigen.spam(multiplied)
  # return (multiplied)
  # # eigen_data = eigen(multiplied)
  # return (lev = eigen_data$values[1])
  # h = rep(1, 13180)
  # return ((1 / lev) * multiplied %*% h)
}

compute_hub <- function(matrix) {
  sparse_A = Matrix(A, sparse = T)
  sparse_tA = Matrix(t(sparse_A), sparse = T)
  multiplied = sparse_tA %*% sparse_A
  # eigen_data = eigen(multiplied)
  # lev = eigen_data$values[1]
  # return ((1 / lev) * multiplied %*% h)
}

# A = generate_matrix(until_year = 1910)
# print(compute_authority(A))


plot_authorities_for_case <- function(from, until, case_id) {
  year_interval = from:until
  authorities_for_case = c()
  # # # # # # # # # # overall_df = data.frame(row.names = 1:30288)

  for (year_index in year_interval) {
    print('mat')
    matrix = generate_matrix(year_index)
    print('auth')
    print('authorithies length')
    authorities = compute_authority(matrix)
    padding = rep(0, 30288 - length(authorities))
    # # # # # # # overall_df[toString(year_index)] = append(authorities, padding)
    print(length(authorities))
    print('case_id_auth = ')
    case_id_authority = authorities[case_id]
    print(case_id_authority)
    print('auth for case')
    authorities_for_case = append(authorities_for_case, case_id_authority)
    #   plot(authorities_for_case ~ from:year_index,
    # xlab = "Year Interval",
    # ylab = "Authority score")

  }

  # # # # # # # write.csv(overall_df, 'Project/data/exported.csv', row.names = TRUE)
  print(authorities_for_case)
  print('plotting')



}

# x = 1:3
# y = 6:8
# plot( y~ x,type='l')
plot_authorities_for_case(1940, 1960, 18501)

#other stopped at 1965
# year_interval = 1965:1969
# sapply(year_interval, FUN = write_authority_hub_vectors, TRUE, FALSE)
