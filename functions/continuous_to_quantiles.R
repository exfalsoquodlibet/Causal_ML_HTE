#' Wrapper function to create a factor variable using the quantiles of a continous variable. 
#'
#' @param dataset Dataset
#' @param continuous_vars A character vector containing the name of the continous variables
#' @param q An integer number of equally spaced quantile groups to create (default is q = 4)
#' @return Original datset with additional factor variables, each specifying the level of quantiles for each coninous variable and observation

continuous_to_quantiles <- function(
      dataset, 
      continuous_vars = character(),
      q = 4
      ){
      
      # load dependencies
      lapply(c('gtools', 'checkmate'), require, character.only=TRUE)
      
      # check that "dataset" is a data.frame
      checkmate::check_data_frame(dataset)
      
      # check that coninuous variables are indeed coded as integer vectors
      apply(as.data.frame(dataset[, continuous_vars]), 2, function(x) checkmate::assert(checkmate::check_integer(x)))
      
      # character vector of new variable names (for the factor of quantiles)
      new_cols <- as.character(sapply(continuous_vars, function(x) paste0(x, '_quant')))
      
      # create factors of quantiles
      tmp_df <- apply(as.data.frame(dataset[, continuous_vars]), 2, function(x) gtools::quantcut(x, q = q, na.rm = TRUE))
      
      # add new column names
      colnames(tmp_df) <- new_cols
      
      # add factors as new columns to original dataset
      do.call('cbind', list(dataset, tmp_df))
      
}  

