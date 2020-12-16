
parse_line <- function(line) {
  splitted = str_split(line, ' ')[[1]]
  citing = splitted[1] %>% strtoi()
  cited = splitted[2] %>% strtoi()

  return(list('citing' = citing, 'cited' = cited))
}

calculate_inward_outward <- function(ids, file_path = 'Project/data/allcites.txt') {
  inward_vector <- rep(0, length(ids))
  outward_vector <- rep(0, length(ids))
  lines <- readLines(file_path)
  #We switched to the for loop to avoid the copy/re-instanciation of the vectors, so we can update them directly
  for (line_index in 1:length(lines)) {
    parsed <- parse_line(lines[line_index])
    line <- lines[line_index]
    splitted <- str_split(line, ' ')[[1]]
    citing <- splitted[1] %>% strtoi()
    cited <- splitted[2] %>% strtoi()

    inward_vector[cited] <- inward_vector[cited] + 1
    outward_vector[citing] <- outward_vector[citing] + 1
  }
  return(list('inward_vector' = inward_vector, 'outward_vector' = outward_vector))
}


