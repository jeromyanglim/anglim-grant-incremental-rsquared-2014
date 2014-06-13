generate_bootstrapped_dataset <- function(n, x=ccases[,v$allscales]) {
    # Sample with replacement of size n from data frame x
    # x: original data frame
    # n: sample size of simulated data set
    # return sampled data frame
    ids <- sample(seq(nrow(x)), size=n, replace=TRUE)
    x[ids, ]
}

generated_simulated_real_dataset <- function(n) {
    r_factor <- cor(ccases[,c('swl', v$ipip_factors, v$ipip_facets)])
    # generate simulated data for predictors
    x_sim <- mvrnorm(n, mu=rep(0, nrow(r_factor)), Sigma=r_factor)
    # data.frame(x_sim)
    data.frame(x_sim)
}

generate_simulated_dataset <- function(n) {
    # get correlation matrix for predictorS
    r_factor <- cor(ccases[,c(v$ipip_factors, v$ipip_facets)])
    # generate simulated data for predictors
    x_sim <- mvrnorm(n, mu=rep(0, nrow(r_factor)), Sigma=r_factor)
    x_sim <- data.frame(x_sim)
    
    # get the coefficients for the factors from the data
    standardised_ccases <- scale(ccases[,c(v$ipip_factors, 'swl')])
    standardised_cases <- data.frame(standardised_ccases)
    fit <- regression('swl', v$ipip_factors, data.frame(standardised_ccases))
    coefs <- coef(fit)[-1]
    
    # swl is factor regression equation plus error
    x_sim$swl <-  as.matrix(x_sim[,names(coefs)]) %*% coefs +  
        rnorm(n=nrow(x_sim), mean=0,  sd=summary(fit)$sigma)
    
    x_sim
}


estimator_r_square <- function(x) {
    fit_factors <- regression('swl', v$ipip_factors, x)
    fit_facets <- regression('swl', v$ipip_facets, x)
    (summary(fit_facets)$r.square - summary(fit_factors)$r.square)
}


estimator_adj_r_squared_ezekiel <- function(x) {
    fit_factors <- regression('swl', v$ipip_factors, x)
    fit_facets <- regression('swl', v$ipip_facets, x)
     (summary(fit_facets)$adj.r.square - summary(fit_factors)$adj.r.square)
}

estimator_adj_r_squared_olkinpratt <- function(x) {
    fit_factors <- regression('swl', v$ipip_factors, x)
    fit_facets <- regression('swl', v$ipip_facets, x)
    
    rsq_factors <- summary(fit_factors)$r.square
    rsq_facets <- summary(fit_facets)$r.square 
    
    adjrsq_factors <- adjusted_r_squared(rsq_factors, n=nrow(x), p =5, method='olkinpratt')
    adjrsq_facets <- adjusted_r_squared(rsq_facets, n=nrow(x), p =30, method='olkinpratt')
    if ( (adjrsq_facets - adjrsq_factors) < -1) {
        browser()
    }
    adjrsq_facets - adjrsq_factors
}


estimator_stepwise05 <- function(x) {
    fit_factors <- stepwise_regression('swl', v$ipip_factors, x, alpha_in=.05)
    fit_facets <- stepwise_regression('swl', v$ipip_facets, x, alpha_in=.05)
     (summary(fit_facets)$r.square - summary(fit_factors)$r.square)
}

estimator_stepwise0083 <- function(x) {
    alpha_enter <- .05 * (5/30)
    fit_factors <- stepwise_regression('swl', v$ipip_factors, x, alpha_in=alpha_enter)
    fit_facets <- stepwise_regression('swl', v$ipip_facets, x, alpha_in=alpha_enter)
     (summary(fit_facets)$r.square - summary(fit_factors)$r.square)
}

estimator_bestfacet  <- function(x) {
    fit_factors <- regression('swl', v$ipip_factors, x)
    fit_facets <- bestfacets_regression('swl', facets=meta$ipipscales$subscale_name,  factors = meta$ipipscales$scale_name, x=x)
    summary(fit_facets)$r.square - summary(fit_factors)$r.square
}
    
summarise_estimator <- function(estimates, theta)  {
    bias <- mean(estimates) - theta
    se <- sd(estimates)
    mse <- mean((estimates - theta)^2)
    root_mse <- sqrt(mse)
    
    c(bias=bias, se=se, mse=mse, root_mse=root_mse, 
         mean_theta_hat=mean(estimates),
         theta=theta)
}

