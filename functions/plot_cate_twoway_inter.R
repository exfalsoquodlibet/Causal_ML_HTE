#' @title plot_cate_twoway_inter
#' @description Function to plot Conditional Average Treatment Effects for two-way interactions
#' @author Alessia Tosi
#' 
#' @param x Dataset returned by get_cate_twoway_inter() function, containing covariates and CATE point estimates and CI bounds
#' @return Plot of Condiional Treatment Effects (CATE) point estimates and their Confidence Intervals


plot_cate_twoway_inter <- function(
      x = NULL
      ){
      
      # load dependencies
      lapply(c('ggplot2'), require, character.only=TRUE)
      
      # check that dataframe has the right structure and column names
      if( !is.data.frame(x) ) stop("x must be a data.frame")
      
      if( any(!c("var1", "level1", "var2", "level2", "cate", 
            "lb_cate", "ub_cate") %in% colnames(x))) stop("Expected column names: var1, level1, var2, level2, cate, lb_cate, ub_cate")
      
      
      x[['subgroup']] <- with(x, interaction(level1, level2, sep=' & '))
      
      ggplot(
            x, 
            aes(x = subgroup, 
                  y = cate, 
                  color = subgroup
                  )
            ) +
            
            theme_light() +
            
            theme(
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  strip.text.y = element_text(colour = "black"),
                  strip.background = element_rect(colour = NA, fill = NA),
                  legend.position = "none"
            ) +
            
            geom_point() +
            
            geom_errorbar(aes(ymin = lb_cate, ymax = ub_cate), width = .2) +
            geom_hline(yintercept = 0, linetype = 3) +
            facet_grid(var1 + var2 ~ ., scales = "free_y") +
            coord_flip()
      
}


# NOT RUN
#plot_cate_twoway_inter(x = subset(cates_df, var1 == 'racename'))


