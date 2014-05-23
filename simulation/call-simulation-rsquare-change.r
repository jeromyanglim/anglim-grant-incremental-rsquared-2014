# source('main.r') # import data; transform data; load libraries
# source('functions.r') # reload support functions
source('simulation/simulation-rsquare-change.r') # load functions used to run simulation

# get the bootstrapped theta
fit_factors <- regression('swl', v$ipip_factors, ccases);
fit_facets <- regression('swl', v$ipip_facets, ccases);
boot_theta <- summary(fit_facets)$r.square - summary(fit_factors)$r.square


# Specification
specification <- list()
specification$test <- FALSE
if (specification$test) {
    specification$simulations <- 20 # B: (integer) number of simulated datasets (either bootstraps or simulations)
} else {
      specification$simulations <- 10000 # B: (integer) number of simulated datasets (either bootstraps or simulations)
}
specification$generator <- c('real', 'none')
# generator_function <- c(generate_bootstrapped_dataset, generate_simulated_dataset)
generator_function <- c(generated_simulated_real_dataset, generate_simulated_dataset)
specification$theta <- c(.1508342, 0)
specification$n <- c(50, 100, 200, 1000)
specification$estimator <- c('stepwise05', 'stepwise0083', 'bestfacet', 
                             'ezekiel', 'olkinpratt', 'rsquare') # c('stepwise05', 'stepwise0083', 'bestfacet', 'adjrsquare')
estimator_function <- c(estimator_stepwise05, estimator_stepwise0083, estimator_bestfacet, 
                        estimator_adj_r_squared_ezekiel, estimator_adj_r_squared_olkinpratt, estimator_r_square)
specification$seed <- 1234

# generator: (named vector of functions) each function generates a sample
# theta: (numeric vector) each value is true value of theta 
#       (i.e., difference between r-square factor and r-square facet)
# n: (vector of positive integers) sample sizes for simulation
# estimator: (vector of functions) function name for estimating theta (i.e., r-square difference)
# seed: (integer) number that sets the seed for random number generator
set.seed(specification$seed)

# create generator
generator <- data.frame(generator=specification$generator, 
                        theta=specification$theta, stringsAsFactors=FALSE)
generator$generator_id <- seq(nrow(generator))

# create design
design <- expand.grid(generator_id=generator$generator_id, n=specification$n)
design$design_id <- seq(nrow(design))
design <- merge(design, generator)

# create dataindex
dataindex <- expand.grid(simulation_id=seq(specification$simulations), 
                         design_id= design$design_id)
dataindex$dataindex_id <- seq(nrow(dataindex))
dataindex <- merge(dataindex, design)

# create dataset
# for each row of dataindex create a dataset
# each generator has a name and takes a sample size argument
dataindex <- dataindex[order(dataindex$dataindex_id), ]
dataset <- vector('list', length=nrow(dataindex))
for (i in seq(dataindex$dataindex_id)) {
    dataset[[i]] <- generator_function[[dataindex$generator_id[i]]](dataindex$n[i])
    # status bar
    if(i %% 100 == 0) cat("Data:", 100 * i/nrow(dataindex),"% complete...\n")
}

# create estimator
estimator <- data.frame(estimator_id=seq(specification$estimator), 
                        estimator=specification$estimator, 
                        stringsAsFactors=FALSE)          

# create dataindexestimator
dataindexestimator <- expand.grid(dataindex_id=dataindex$dataindex_id,  estimator_id=estimator$estimator_id)
dataindexestimator <- merge(dataindexestimator, dataindex)
dataindexestimator$dataindexestimator_id <- seq(nrow(dataindexestimator))


# estimate thetahat for each dataindexestimator
for (i in seq(nrow(dataindexestimator))) {
    dataindexestimator$theta_hat[i] <- 
        estimator_function[[ dataindexestimator$estimator_id[i] ]](
            dataset[[ dataindexestimator$dataindex_id[i] ]]  
            )
    # status bar
    if(i %% 100 == 0) cat("Estimation: ", 100 * i/nrow(dataindexestimator),"% complete...\n")
}

# estimator_summary
estimator_summary <- expand.grid(design_id=design$design_id,  estimator_id=estimator$estimator_id)
estimator_summary$estimator_summary_id <- seq(nrow(estimator_summary))
estimator_summary <- merge(estimator_summary, design)
estimator_summary <- merge(estimator_summary, estimator)

# add summary statistics
for (i in seq(nrow(estimator_summary))) {
    id1 <- estimator_summary[i,'estimator_id']
    id2 <- estimator_summary[i,'design_id']
    theta_hats <-  subset(dataindexestimator, estimator_id==id1 & design_id==id2)[,'theta_hat']
    theta <-  subset(dataindexestimator, estimator_id==id1 & design_id==id2)[,'theta']
    theta <- unique(theta)
    results <- summarise_estimator(theta_hats, theta)
    
    if (i == 1) {
        estimator_summary[,names(results)] <- NA
    }
    
    estimator_summary[i,names(results)] <- results
}



# reported_table
produce_reported_table  <- function(DV) {
    rtable <- estimator_summary[, c('estimator', 'generator', 'n', DV)]
    rtable <- reshape(rtable, idvar=c('generator', 'estimator'), timevar='n', direction='wide')
    rtable <- rtable[order(rtable$generator), ]
    rtable
}
stats <- c('bias', 'se', 'mse', 'root_mse', 'mean_theta_hat') 
reported_table <- lapply(stats, 
       function(X) produce_reported_table(X))
names(reported_table) <- stats

specification
round_df(reported_table$bias, 3) 
round_df(reported_table$se, 3) 

write.csv(reported_table$bias, file='output/rsquarechange-bias.csv')
write.csv(reported_table$se, file='output/rsquarechange-se.csv')
# estimator_stepwise05(dataset[[ dataindexestimator$dataindex_id[i] ]]  )
# debug(estimator_bestfacet)
# estimator_bestfacet(dataset[[ dataindexestimator$dataindex_id[i] ]]  )
# round_df(subset(estimator_summary, generator=='real'), 3)