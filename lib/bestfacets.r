largest_correlate <- function(y, x) {
    cors <- abs(cor(y, x))
    max_cor_position <- which.max(as.vector(cors))[1]
    colnames(cors)[max_cor_position]
}

bestfacets_regression <- function(dv='swl', facets=meta$ipipscales$subscale_name,  factors = meta$ipipscales$scale_name,
                                  x=ccases) {
# library(personalityfacets)   
    facet_list <- split(facets, f= factors)
    best_facets <- sapply(facet_list, function(X) largest_correlate(x[,dv], x[,X]))
    fit_facets <- regression(dv, best_facets, x)
    fit_facets  
}
