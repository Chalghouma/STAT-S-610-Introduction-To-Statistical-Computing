library(readr)


read_auth = function(path = 'Project/data/authmat.txt'){
    df = read.csv(path)
}

get_auth_by_year = function(auth_df, year){
    #We already know the first year is 1800
    # if(year < 1800 )
    if(year < 1800 | year > 2002)
        return(warning('Year should be between 1800 and 2002'))
    return (auth_df[, (year - 1800) + 1 + 1 ])
}

brown_versus_mississipi = function(){
    df = read_auth()
    get_authority_for_case = function(year,case_id){
        return (get_auth_by_year(df,year)[case_id])
    }
    year_interval = 1940:2000
    data = sapply(year_interval,FUN=get_authority_for_case, 18501 )

    plot( data~ year_interval )
}

brown_versus_mississipi()