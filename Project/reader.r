library(readr)


read_auth = function(path = 'Project/data/authmat.txt'){
    df = read.csv(path)
}

get_auth_by_year = function(auth_df, year){
    #We already know the first year is 1800
    if(year < 1800 | year > 2002)
        return(warning('Year should be between 1800 and 2002'))
    return (auth_df[, (year - 1800) + 1 + 1 ])
}