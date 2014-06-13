round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}


save_csv_list <- function(x, dir='.') {
    for (i in seq(x)) {
        df <-  eval(parse(text=x[[i]]))
        filename <- paste0(x[i], '.csv')
        pathname <-  file.path(dir, filename)
        write.csv(df, pathname)
    }
    
    
}

# x <- grep('measures', ls(), value=TRUE)
# save_csv_list(x, 'data')
