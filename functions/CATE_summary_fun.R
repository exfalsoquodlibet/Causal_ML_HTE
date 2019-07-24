# Function to calculate Conditional Average Treatment Effects #

CATE_summary_fun <- function(data, outcome_binary=TRUE, outcome_var, treat_var,  ...){
      
      outcome_var <- enquo(outcome_var)
      treat_var <- enquo(treat_var)
      group_covars <- enquos(...)
      
      # TODO
      # add checking that terat_var has been coded numerically, with values 1 and 0
      
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
            },
            
            # error-handling
            error = function(c) "one or more subgroups contain no cases"
            
      )
      
}


