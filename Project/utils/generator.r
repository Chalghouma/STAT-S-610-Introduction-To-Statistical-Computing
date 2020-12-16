library(RSpectra)
source('Project/source_paths.r')
source(get_selector_file_path())
source(get_hubs_authorities_io_file_path())
source(get_reader_file_path())
library(Matrix)
library(tidyverse)
library(spam)

#Generates the adjacency matrix where
#if node i is citing the node j twice
#M(i,j) = 2
generate_matrix <- function(until_year, csv_path = 'Project/data/judicial.csv', all_cities_path = 'Project/data/allcites.txt') {
  judicial_df = read_judicial_data()
  case_ids_until_year <- get_cases_until(judicial_df, until_year)$caseid
  dimension <- length(case_ids_until_year)
  m <- matrix(rep(0, dimension * dimension), nrow = dimension)
  lines <- readLines(all_cities_path)
  lines_length <- length(lines)
  line_index <- 1
  latest_case_id <- length(case_ids_until_year)
  has_surpassed_latest_case_id <- FALSE
  while (line_index < lines_length & !has_surpassed_latest_case_id) {
    processed_line = parse_line(lines[line_index])
    if (processed_line$citing > latest_case_id) {
      has_surpassed_latest_case_id = TRUE
    } else {
      m[processed_line$citing, processed_line$cited] <- m[processed_line$citing, processed_line$cited] + 1
      line_index <- line_index + 1
      has_surpassed_latest_case_id
    }
  }
  return(m)
}



write_authority_hub_vectors <- function(year, should_write_hub = TRUE, should_write_authority = TRUE) {
  get_largest_eigen_vector <- function(matrix) {
    return(eigs(multiplied, k = 1, which = 'LM')$vectors[, 1])
  }
  A <- generate_matrix(year)
  sparse_A <- Matrix(A, sparse = T)
  sparse_tA <- Matrix(t(sparse_A), sparse = T)

  if (should_write_hub) {
    multiplied <- sparse_A %*% sparse_tA
    write_hubs(get_largest_eigen_vector(multiplied), year)
  }
  if (should_write_authority) {
    multiplied <- sparse_tA %*% sparse_A
    write_authorities(get_largest_eigen_vector(multiplied), year)
  }
}

  print('Generating A')
  print('A generated')
  print('sparse_A generated')
  print('sparse_tA generated')
    print('generating multiplied')
    print('writing authoritiesee multiplied')

compute_authority <- function(matrix) {
  sparse_A <- Matrix(matrix, sparse = T)
  print('sparse_A calculated')
  sparse_tA <- Matrix(t(sparse_A), sparse = T)
  print('sparse_tA calculated')

  multiplied <- sparse_A %*% sparse_tA
  print('multiplied calculated')
  eigen_spectra <- eigs(multiplied, k = 1, which = 'LM')
  print('eigs calculated')
  return(eigen_spectra$vectors[, 1])
}

compute_hub <- function(matrix) {
  sparse_A = Matrix(A, sparse = T)
  sparse_tA = Matrix(t(sparse_A), sparse = T)
  multiplied = sparse_tA %*% sparse_A
}

plot_authorities_for_case <- function(from, until, case_id) {
  year_interval = from:until
  authorities_for_case = c()

  for (year_index in year_interval) {
    print('mat')
    matrix = generate_matrix(year_index)
    print('auth')
    print('authorithies length')
    authorities = compute_authority(matrix)
    padding = rep(0, 30288 - length(authorities))
    print(length(authorities))
    print('case_id_auth = ')
    case_id_authority = authorities[case_id]
    print(case_id_authority)
    print('auth for case')
    authorities_for_case = append(authorities_for_case, case_id_authority)
  }

  print(authorities_for_case)
  print('plotting')
}