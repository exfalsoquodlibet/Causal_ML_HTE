# ------------------------------------------ #
# Using Honest Causal Forest to estimate HTE #
# ------------------------------------------ #


### Set up ------

# Packages ---

pckgs <- c("dplyr", 
      "tidyverse", 
      "stringr", 
      "grf",
      "ggplot2")

lapply(pckgs, require, character.only=TRUE)

# User-defined functions ---

source("functions/CATE_summary_fun.R")    # to calculate conditional average treatment effects



### Data ------

fruit_df <- readRDS("data/synth_hte_df.rds")




### What's "causal" and what's "honest" in a causal honest forest? ------

# "CAUSAL" 
# trees explicitly searched for subgroups where treatment effect is higher:
# the underlying algorithm calculates the treatment effect within each leaf for possible splitting variables
# and splits the data into child leaves so that:
# - producing biggest difference in treatment effects across leaves (= "maximising treatment effect")
# - still give an accurate estimate of treatment effect (reducing impurity - entropy/Gini index)

# In order words, 
# the splitting algorithm maximizes a criterion which favors splits (in the covariates) that 
# are increasing the heterogeneity of our in-sample estimation of heterogeneous treatment effects


# "HONEST" 
# one sub-sample is used to make the splits (ie., fit the trees) and 
# another distinct sub-sample is used to estimate the treatment effect (i.e., "coefficients")

# This reduces bias in tree predictions



# How does Honest Causal Forest go around the problem of causal inference? ------

# I.e., The fact that for any individual i, we only observe Y_i(treatment) or Y_i(outcome)?

# As a given individual is only assigned either to the treatment or the control,
# we estimate their treament effect by searching "similar others" and use them as stand-ins for  
# the individual's potential outcome
# ==> we compare people with similar covariate profiles in different experimental conditions
# use these differences as estimates of a treatment effect
# ==> assign that treatment effect to everyone in that covariate-profile subgroup

# Translated in the Random Forest context ---

# See https://github.com/grf-labs/grf/blob/master/REFERENCE.md

# as put it clearly by https://rajpurkar.github.io/mlx/treatment-effects/ 
# Trees can be thought of as nearest neighbor methods with an adaptive neighborhood metric, 
# with the closest points to x_i being in the same leaf that it is. 
# Let’s assume that we have build a classification tree by some method by observing independent samples (Xi,Yi)(Xi,Yi), 
# then a new x_j can be classified by:

# 1. Identify the leaf containing x_j
# 2. In that leaf, take the mean of the Yi's in that leaf.

# In a causal tree, we make use of the assignment labels Wi in the cases, where W is the treatment allocation. 
# To make the prediction τ(x_j) = Y_j(W = 1) - Y_j(W=0):
      
# 1. Identify the leaf containing x_j
# 2. In that leaf, compute the mean of the Yis where Wi=0, and subtract that 
     # from the mean of the Yis where Wi=1.


### Important stuff ------

# Model's assumption: UNCONFOUNDEDNESS
# i.e., the decision of whether or not an individual i gets treatment Pr(W=1 | x_i) 
# is independent of the potential outcomes (Y_i(0),Y_i(1)) when conditioned on Xi. 
# The implication of this assumption is that nearby observations in the x-space can be treated as “having come from a randomized experiment.”

# Issues with package / useful discussion and code
# https://github.com/grf-labs/grf/issues/238



### Enough babbling, let's try it out ------

# We will use the `grf` package ?grf
# https://github.com/grf-labs/grf


# Split data into training (80%) and test (20%) set ------

# we will pretend that the training set is the dataset from our RCT
# whereas the test set contain new "unseen" cases, ie. future potential targets


set.seed(987)
train_idx <- sample(seq_len(nrow(fruit_df)), round(nrow(fruit_df) * .8))

train_fruit_df <- fruit_df[train_idx,]
test_fruit_df <- fruit_df[-train_idx,]

# let's control balance of outcomes in two sets
with(train_fruit_df, prop.table(xtabs(~chose_fruit)))
with(test_fruit_df, prop.table(xtabs(~chose_fruit)))
# GOOD


# Note on Random Forest / trees methods ---
# RF will tend to favour highly variable continuous predictorsor (and also categorical predictors with many more levels than the others) because there are more opportunities to partition the data. 

# This does not really matter if we are purely interested in prediction performance
# But here we are also interested in understanding covariates' contribution to the treatment effect heterogeneity
# MORE THINKING TO DO

# As a result, the feature importance metrics may be biased...

# This won't help, but I'm standardizing the two interval/continuous covariates anyway
train_fruit_df <- train_fruit_df %>%
      mutate(
            s_age = scale(age, center = TRUE, scale = TRUE),
            s_hhinc = scale(hhinc, center = TRUE, scale = TRUE)
      )

# Let's recode the multicategorical race as a factor, and then create dummy variables
# (other binary covariates are already "dummised")
train_fruit_df <- train_fruit_df %>%
      mutate(
            racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))
            )
      
# Let's create the input for the casual_forest algorithm ---

# Matrix of relevant covariates (will create dummy vars automatically)
my_X <- model.matrix(~ ., 
      data = train_fruit_df[, ! names(train_fruit_df) %in% c('chose_fruit', 'treatment', 'cid', 'age', 'hhinc', 'race')]
      )

# Outcome
my_Y <- train_fruit_df$chose_fruit

# Treatment assignment
my_W <- train_fruit_df$treatment

# number of trees
ntrees <- 5000 


# Algorithm ---

fruit_hcf <- grf::causal_forest(
      
      # covariate matrix
      X = my_X,
            
      # outcome variable
      Y = my_Y,
      
      # treatment variable
      W = my_W,
      
      num.trees = ntrees,
      
      # parameters that will be tuned via cross-validation
      mtry = NULL, #default, number of variables tried for each split
      min.node.size = NULL, # minimum number of cases in each node (end leaf)
      
      alpha = NULL, # default >>> controls max imbalance of a split (across child nodes) in terms of treatment vs. control cases
      imbalance.penalty = NULL, # controls how harshly imbalanced split are penalised >>> applies a penalty to discourage child nodes from having very different sizes     
      
      honesty = TRUE, # default >>> honest sub-sample splitting
      honesty.fraction = NULL, # default, 50% in splitting sub-sample, 50% in estimating sub-sumple
      
      tune.parameters = TRUE, #parameters tuned by cross-validation
      seed = 123
      
)


# save the model ---
saveRDS(fruit_hcf, "fruit_hcf.rds")

