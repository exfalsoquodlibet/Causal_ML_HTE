#' Function to calculate Conditional Average Treatment Effects 
#'
#' @param data Dataset
#' @param outcome_binary Logical value for whether the outcome is binary or not (default is TRUE)
#' @param outcome_var Name of outcome variable
#' @param outcome_var Name of treatment variable (must be coded as numerically as 0 and 1)
#' @param ... Names of covariates
#' @return Condiional Treatment Effects (CATE) point estimates and their SEs


CATE_summary_fun <- function(data, outcome_binary=TRUE, outcome_var, treat_var,  ...){
      
      outcome_var <- enquo(outcome_var)
      treat_var <- enquo(treat_var)
      group_covars <- enquos(...)
      
      
      # load dependencies
      lapply(c('dplyr'), require, character.only=TRUE)
      
      
      # Checking that treat_var has been coded numerically, with values 1 and 0
      data %>% 
            dplyr::pull(!!treat_var) %>%
            {
                  if(! all(. %in% c(0,1))) stop("treat_var must be coded numerically, with values 1 and 0 only")
            }
      
      
      tmp <- data %>% 
            dplyr::group_by( !!!group_covars, !!treat_var ) %>%
            dplyr::summarise(
                  outcome_mean = mean(!! outcome_var, na.rm=TRUE),
                  outcome_sd = sd(!! outcome_var, na.rm=TRUE ),
                  outcome_n = n()
            )
      
      # print average outcome per condition
      print(tmp)
      
      # capture subgroups with no cases (it'd throw an error)
      tryCatch(
            {
            # code for CATE estimation and their SEs
            
            if(outcome_binary == TRUE){
                  tmp %>%
                        dplyr::summarise(
                              treatm_effect = (outcome_mean[!!treat_var == 1]) - (outcome_mean[!!treat_var == 0]),
                              se_effect = sqrt( (outcome_mean[!!treat_var == 1]*(1 - outcome_mean[!!treat_var == 1]) / outcome_n[!!treat_var == 1]) + (outcome_mean[!!treat_var == 0]*(1 - outcome_mean[!!treat_var == 0]) / outcome_n[!!treat_var == 0]) ),
                              sample_n = outcome_n[!!treat_var == 1] + outcome_n[!!treat_var == 0]
                        )
                  
            } else if (outcome_binary == FALSE){
                  tmp %>%
                        dplyr::summarise(
                              treatm_effect = (outcome_mean[!!treat_var == 1]) - (outcome_mean[!!treat_var == 0]),
                              se_effect = sqrt( (outcome_sd[!!treat_var == 1]^2 / outcome_n[!!treat_var == 1]) + (outcome_sd[!!treat_var == 0]^2 / outcome_n[!!treat_var == 0]) ),
                              sample_n = outcome_n[!!treat_var == 1] + outcome_n[!!treat_var == 0]
                        )
            }
                  },
            
            # error-handling
            error = function(c) "one or more subgroups contain no cases"
            
      )
      
}


