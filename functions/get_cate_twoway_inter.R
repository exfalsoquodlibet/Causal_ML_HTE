#' Calculate point estimate and confidence interval bounds of the conditional treatment effect ('cate') at each level of the two-way interactions among all covariates 
#'
#' @param grf_forest An object of class 'causal_forest grf'
#' @param dataset Dataset
#' @param cov_idx A numeric vector containing the column indeces of the categorical covariates 
#' @return A dataframe containing the point estimate and confidence interval bounds of the conditional treatment effect ('cate') at each level of the two-way interactions among all covariates 

get_cate_twoway_inter <- function(
      grf_forest, 
      dataset, 
      cov_idx = numeric()
      ){
      
      # load dependencies
      lapply(c('grf', 'checkmate'), require, character.only=TRUE)
      
      # check that grf_forest is a causal_forest object
      checkmate::assert(
            test_class(
                  grf_forest, c('causal_forest', 'grf')
            )
      )
      
      # check that variables are categorical and coded as factors
      if( any(lapply(dataset[, cov_idx], is.factor) == FALSE)
            
            ){
            warning("Categorical covariates must be factors. Those that are not have been recoded to factors.")
            # recode as factors
            char_vars <- names(which(sapply(dataset[, cov_idx], function(x) !is.factor(x) )))
            dataset[char_vars] <- lapply(dataset[char_vars], function(x) as.factor(as.character(x)))
      }
      
      # data frame to collect results
      tmp_df <- data.frame(
            var1 = rep(NA, 1000), 
            level1 = NA, 
            var2 = rep(NA, 1000), 
            level2 = NA, 
            cate = NA, 
            lb_cate = NA, 
            ub_cate = NA
            )
      
      counter <- 1
      
      # list of covariates as character vector
      cov_names <- names(dataset[, cov_idx])
      
      for (i in seq_along(cov_names)) {
            
            # if last covariate in the list, skip  
            if(seq_along(cov_names)[i] == length(cov_names)) next
            
            else{
                  # given a covariate, for all other subsequent covariates in the list
                  # calculate the CATE for each possible combination of their levels
                  for (p in seq_along(cov_names)[-c(1:i)]) {
                        for (j in levels(dataset[[cov_names[i]]])) {
                              for(q in levels(dataset[[cov_names[p]]])) {
                                    
                                    # calculate CATEs
                                    tmp_cates <- grf::average_treatment_effect(
                                          grf_forest, 
                                          subset = dataset[complete.cases(dataset), cov_names[i]] == j & 
                                                dataset[complete.cases(dataset), cov_names[p]] == q
                                    )
                                    
                                    # populate results dataframe
                                    tmp_df$var1[[counter]] <- cov_names[i]
                                    tmp_df$level1[[counter]] <- j
                                    tmp_df$var2[[counter]] <- cov_names[p]
                                    tmp_df$level2[[counter]] <- q
                                    # point estimate
                                    tmp_df$cate[[counter]] <- tmp_cates[[1]]
                                    # confidence interval bounds
                                    tmp_df$lb_cate[[counter]] <- tmp_cates[[1]] - 1.96 * tmp_cates[[2]]
                                    tmp_df$ub_cate[[counter]] <- tmp_cates[[1]] + 1.96 * tmp_cates[[2]]
                                    
                                    counter <- counter + 1
                              }
                        }
                  }
            }
      }
      
      
      tmp_df <- tmp_df[complete.cases(tmp_df), ]
      
      return(tmp_df)
}

