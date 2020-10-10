library(readr)

# 1. Berhu penalty

berhu <- function(x, delta) {
   if(abs(x) < delta) {
       abs(x)
   } else {
       #Putting The operation in one single line
       x^2 / 2 * delta + delta / 2
   }
}

berhu(1,1) ## should be 1
berhu(.5, 1) ## should be .5
berhu(2, 1) ## should be 2.5
xseq = seq(-3, 3, length.out = 200)
plot(sapply(xseq, berhu, 1) ~ xseq, type = 'l')

# 2. Trimmed mean

trimmed_mean <- function(x, trim) {
    #Changed the (1-trim)/2 to (1-trim)
    qlo = quantile(x, probs = (1 - trim))
    qhi = quantile(x, probs = 1 - (1 - trim))

    #We either append NULL or append the element
    #depending on whether they fall within the range or not
    process_element_of_array= function(element){
        if(element <= qlo && element >= qhi)
            return(element)
        return(NULL)
    }


    within_range = sapply(x,FUN=process_element_of_array)
    #We remove the Nulls, those who were FALSE for both checks
    without_nulls = plyr::compact(within_range)
    return(mean(unlist(without_nulls)))
}
## the following two should give the same results
tm = trimmed_mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)


## 3. String processing
process_table = function(str) {
  ## split by line
  lines = read_lines(str)
  ## remove padding lines
  ##We only need to check whether they're equal to 
  lines = lines[lines != "|-"]
  ## remove the class definition line, header line, footer line
  class_line = grep("class=", lines)
  header_line = grep("! Rank !! Overall Ranking", lines)
  footer_line = grep("\\|}", lines)
  lines = lines[-c(class_line, header_line, footer_line)]
  
  #An additional line was generated that was equal to 
  #|-\t\t\t\t\t\t\t\t
  #We remove that line too
  #We could have also applied an *apply function where we check whether the line STARTS with |-
  #That would have been more elegant
  lines = lines[-2]
  ## apply the process_data_line function to all of the lines
  plyr::adply(lines, 1, process_data_line)
}
process_data_line = function(str) {
  split = strsplit(str, ' *\\|\\|? +')[[1]]

  ## extract the city
  city_pattern = "\\[\\[[A-z]*\\]\\]"
  city_idx = grep(city_pattern, split)
  city = regmatches(split[city_idx], regexpr(pattern = city_pattern, split[city_idx]))
  city = gsub("\\[|\\]", "", x = city, perl = TRUE)
  ## extract the rankings and scores
  row = lapply(split[-city_idx], function(x) {
    return(as.numeric(x))
  })
  row = c(row, city)
  row[is.na(row)] = NULL
  #Updating Row Names
  names(row) = c("Rank", "Old.Rank", "Score", "City")
  row = data.frame(row)
  return(row)
}
#Since in the assignement, it was mentionned that
#(don’t worry about the unicode characters).
#Therefore, we removed these unicode characters like ü
city_rankings = '{| class="wikitable sortable"
|-
! Rank !! Overall Ranking 2017 !! City !! 2010 Score 
|-
| 1 || 1 || {{flagicon|Austria}} [[Vienna]] || 108.6
|-					
| 2 || 2 || {{flagicon|Switzerland}} [[Zurich]] || 108.0 
|-
| 3 || 4 || {{flagicon|Germany}} [[Munich]] || 107.0
|-
| 4 || 6 || {{flagicon|Germany}} [[Dusseldorf]] || 107.2
|-
| 5 || 7 || {{flagicon|Germany}} [[Frankfurt]] || 107.0
|}'
process_table(city_rankings)


## 4. Gradient descent
gradient_descent <- function(fn, deriv, start, step_size, epsilon) {
  x = start
  while (TRUE) {
    #We used not to calculate the new_x correctly
    # new_x = x + step_size * deriv(x)
    new_x = x - step_size * deriv(x)

    #We used not to calculate the difference correctly too
    # if (abs(deriv(new_x)) <= epsilon) {
    difference = abs(fn(new_x) - fn(x))
    if (difference <= epsilon) {
      break
    }
    x = new_x
  }
  return(x)
}

## should return something close to 0
gradient_descent(function(x) x ^ 2, function(x) 2 * x, start = 1,
                 step_size = 0.1, epsilon = 1e-10)

## 5. Line search
backtrack_desc <- function(fn, deriv, start, alpha, beta, epsilon) {
  x = start
  while (TRUE) {
    step_size = backtrack(fn, deriv, x, alpha, beta)
    # #wrong calculation of new_x
    # new_x = fn(x) - step_size * deriv(x)
    new_x = x - step_size * deriv(x)

    #wrong if statement
    # if(abs(deriv(new_x)) <= epsilon) {
    difference = abs(fn(new_x)- fn(x))
    if (difference <= epsilon) {
      break
    }
    x = new_x
  }
  return(x)
}

backtrack <- function(fn, deriv, x, alpha, beta) {
  t = 1
  while (TRUE) {
    first = fn(x - t * deriv(x))
    second = (fn(x) - alpha * t * (deriv(x) ^ 2))
    if (first < second) {
      break
    }
    t = beta * t
  }
  return(t)
}

## should return something close to 0
backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 10,
               alpha = .03, beta = .8, epsilon = 1e-10)
backtrack_desc(function(x) x ^ 2, function(x) 2 * x, start = 1,
               alpha = .03, beta = .8, epsilon = 1e-10)