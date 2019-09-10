# ------------------------------------------ #
# Using Honest Causal Forest to estimate HTE #
# ------------------------------------------ #


### Set up ------

# Packages ---

pckgs <- c("dplyr", 
      "tidyverse", 
      "stringr", 
      "grf",
      "caret",
      "ggplot2")

sapply(pckgs, require, character.only=TRUE)



# Source user-defined functions ---

source("functions/CATE_summary_fun.R")    # to calculate conditional average treatment effects
source("functions/get_cate_twoway_inter.R")
source("functions/continuous_to_quantiles.R")
source("functions/plot_cate_twoway_inter.R")


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
      
      compute.oob.predictions = TRUE,
      
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


# save the model (DO ONLY ONE) ---
#saveRDS(fruit_hcf, "fruit_hcf.rds")

# read the model ---
fruit_hcf <- readRDS("fruit_hcf.rds")


# let's take a look at the model object ------

summary(fruit_hcf)

# parameters that were tunable
fruit_hcf$tunable.params   # optimal values of tuning parameters
fruit_hcf$tuning.output   

head(fruit_hcf$X.orig) # original set of covariates
head(fruit_hcf$W.orig) # original treatment level
head(fruit_hcf$W.hat)  # predicted treatment level allocation ("propensity score")
head(fruit_hcf$Y.orig) # original outcome
head(fruit_hcf$Y.hat)  # predicted outcome (marginalised over treatment)






# Out-Of-Bag (OOB) predictions for train examples ------

# For each training unit/case ("example"), 
# all the trees that did not use this example during training (~ 1/3 of trees) are identified 
# (the case was 'out-of-bag', or OOB). 
# Then, a prediction of the treatment effect for the example is made using only these trees. 
# These out-of-bag predictions can be useful in understanding the model's goodness-of-fit

train_y_hats <- predict(
      object = fruit_hcf,
      estimate.variance = TRUE   # for confidence interval (IMPORTANT)
)

str(train_y_hats)


# Explore goodness of fit ------

sum(train_y_hats$debiased.error, train_y_hats$excess.error)   #raw error
sum(train_y_hats$debiased.error, train_y_hats$excess.error) / nrow(train_y_hats)  # average error
with(train_y_hats, plot(predictions ~ debiased.error))
with(train_y_hats, hist(debiased.error))

sum(train_y_hats$excess.error)   #recommended to grow enough forests to make it negligible

# hard to assess fit of a single model...


# Other performance metrics based on classification outcome ---
# We'll do it based on outcome predictions as there is no "golden truth" for the treatment effects

# add predicted individual-level treatment effects to original data
train_fruit_df$pred_treatm_effect <- train_y_hats$predictions
train_fruit_df$pred_se_treatm_effect <- sqrt(train_y_hats$variance.estimates)

caret::confusionMatrix(
      data = factor(dplyr::if_else(fruit_hcf$Y.hat > 0.5, 1, 0), levels=c(1,0)) , 
      reference = factor(fruit_hcf$Y.orig, levels=c(1,0)) 
      )

# high recall (sensitivity) : of all those that are acually 1's, how many did we correctly predict (as "1")?
# low specificity:  of all those who are actually 0's, how many have we correctly predict (as "0")?
# precision: how many of those who we labeled as "1" are actually 1?   #0.60


# Individual-level conditional treatment effects and their associated variance estimates ------

#let's plot them with 95% confidence intervals
ggplot(train_y_hats,
      mapping = aes(
            x = rank(predictions), 
            y = predictions
      )
      ) +
      geom_point() +
      labs(x = "Train examples", y = "Estimated Treatment Effect") +
      theme_light() +
      geom_errorbar(
            mapping = aes(
                  ymin = train_y_hats$predictions - 1.96 * sqrt(train_y_hats$variance.estimates),
                  ymax = train_y_hats$predictions + 1.96 * sqrt(train_y_hats$variance.estimates)
            )
      )

# Yep, there clearly seem to be heterogenous treatment effects !



# What is driving the (heterogenous treatment effect)? ------

# Rank of important covariate ---
# danger of biased approach, as it has a tendency to inflate the importance of continuous features or high-cardinality categorical variables...

fruit_hcf %>% 
      variable_importance() %>% 
      as.data.frame() %>% 
      mutate(variable = colnames(fruit_hcf$X.orig)) %>% 
      arrange(desc(V1))

# correctly identified "overweight" and "race: africanAm" as two top covariates driving HTE

# Plot the relationships ---
# between the top important variables and the predicted treatment effects


# overweight
ggplot(
      train_fruit_df, 
      aes(x = as.factor(overweight), 
            y = pred_treatm_effect, 
            fill= as.factor(overweight)
      )
) +
      geom_violin(draw_quantiles=.5, scale = 'count') +
      theme_light()


# race
ggplot(
      train_fruit_df, 
      aes(x = as.factor(racename), 
            y = pred_treatm_effect, 
            fill=as.factor(racename)
            )
      ) +
      geom_violin(draw_quantiles=.5, scale = 'count') +
      theme_light()


# household income
ggplot(train_fruit_df, aes(x = hhinc, y = pred_treatm_effect)) +
      geom_point(aes(colour = racename)) +
      geom_smooth(method = "loess", span = 1) +   #smooth local regression
      theme_light()
# looks like some interaction may be going on



# HTE for subgroups in the population ------
# they can be calculated using the function grf::average_treatment_effect.
# However, below, we'll use my user-defined function that applies this systematically 
# to all two-way interactions between coovariates

# Estimating average treatmen effects (ATE) ------
grf::average_treatment_effect(fruit_hcf, target.sample = "all")
grf::average_treatment_effect(fruit_hcf, target.sample = "treated")
grf::average_treatment_effect(fruit_hcf, target.sample = "control")

# Estimating HTE / CATE, examples ------
grf::average_treatment_effect(fruit_hcf, target.sample = "all", subset=train_fruit_df[, 'overweight'] ==1 )
grf::average_treatment_effect(fruit_hcf, target.sample = "all", subset=train_fruit_df[, 'overweight'] ==0 )
grf::average_treatment_effect(fruit_hcf, target.sample = "all", subset=train_fruit_df[, 'age'] < 5 )
grf::average_treatment_effect(fruit_hcf, target.sample = "all", subset=train_fruit_df[, 'racename'] == "africanAm" & train_fruit_df[, 'overweight'] ==1 )
grf::average_treatment_effect(fruit_hcf, target.sample = "all", subset=train_fruit_df[, 'racename'] == "africanAm" & train_fruit_df[, 'overweight'] ==0 )



# Get conditional average treatment effects for all two-way interaction between each level of each categorical covariate ------
# info: https://github.com/grf-labs/grf/issues/238

# Make sure categorical covariates are coded as factors
train_fruit_df <- train_fruit_df %>%
      mutate(
            malef = factor(male, levels=c(0,1), labels=c('female', 'male')),
            overweightf = factor(overweight, levels=c(0,1), labels=c('non-overweight', 'overweight')),
            siblingsf = factor(siblings, levels=c(0,1), labels=c('no_siblings', 'yes_siblings')),
            stateschf = factor(statesch, levels=c(0,1), labels=c('no_statesch', 'yes_statesch'))
      )


# Create factors of quantiles for continuous covariates age and hhinc
train_fruit_df <- continuous_to_quantiles(train_fruit_df, c('age', 'hhinc'), q=4)

# take a look
str(train_fruit_df)
args(get_cate_twoway_inter)

# get cates for all two-way interactions between covariates
cates_df <- get_cate_twoway_inter(
      grf_forest = fruit_hcf,
      dataset = train_fruit_df, 
      cov_idx = c(13, 16:21)
      )


# Vizualise HTEs ---

# too many, so let's only plot a subset
# for example, only all interactions with race
cates_df %>%
      filter(var1 %in% c('racename')) %>%
      plot_cate_twoway_inter(.)
      
      






### Predict on potential future targets ------

# We had left aside some data to use as new "unseen" data 

# Prepare test data

test_fruit_df <- test_fruit_df %>%
      mutate(
            s_age = scale(age, center = TRUE, scale = TRUE),
            s_hhinc = scale(hhinc, center = TRUE, scale = TRUE),
            racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))
      )

test_X <- model.matrix(~ ., 
      data = test_fruit_df[, ! names(test_fruit_df) %in% c('chose_fruit', 'treatment', 'cid', 'age', 'hhinc', 'race')]
)


# Predict on test data

test_y_hats <- predict(
      object = fruit_hcf,
      newdata = test_X,
      estimate.variance = TRUE   # for confidence interval (IMPORTANT)
)

str(test_y_hats)


# Assess

# add predicted individual-level treatment effects to original data
test_fruit_df$pred_treatm_effect <- test_y_hats$predictions
test_fruit_df$pred_se_treatm_effect <- sqrt(test_y_hats$variance.estimates)


# Individual-level conditional treatment effects and their associated variance estimates ---

#let's plot them with 95% confidence intervals
ggplot(test_y_hats,
      mapping = aes(
            x = rank(predictions), 
            y = predictions
      )
) +
      geom_point() +
      labs(x = "Test examples", y = "Estimated Treatment Effect") +
      theme_light() +
      geom_errorbar(
            mapping = aes(
                  ymin = test_y_hats$predictions - 1.96 * sqrt(test_y_hats$variance.estimates),
                  ymax = test_y_hats$predictions + 1.96 * sqrt(test_y_hats$variance.estimates)
            )
      )

# Yep, here again there clearly seem to be heterogenous treatment effects

# distribution of treatment effect point estimates
with(test_y_hats, hist(predictions, breaks=20))


### Who should get the intervention? ------

# Probably the main goal of running a causal machine-learning model is to understand
# who should be receiving the intervention (if rolled out) and who should not in the future,
# based on the estimated sub-group or individual treatment effects.

with(test_fruit_df, median(pred_treatm_effect))   # median treatment effect


# Option 1: 
# Intervention, if treatment effect is positive (> 0), control if treatment effect is negative (< 0)
# We ignore the uncertainty around these point estimates

test_fruit_df <- test_fruit_df %>%
      mutate(treat_assignment1 = if_else(
            pred_treatm_effect > 0, 1, 0
      ))


# Option 2: 
# Intervention, if treatment effect is positive (> 0), control if treatment effect is negative (< 0)
# and their confidence intervals do not cross 0
# random assignment (or control?), otherwise ('999' as a sign post atm)

test_fruit_df <- test_fruit_df %>%
      mutate(
            lb_te = pred_treatm_effect -1.96*pred_se_treatm_effect,
            ub_te = pred_treatm_effect +1.96*pred_se_treatm_effect) %>%
      mutate(treat_assignment2 = if_else(
            (lb_te > 0) & (ub_te > 0), 1, if_else(
                  (lb_te < 0) & (ub_te < 0), 0, 999
            )
      ))


# let's take a look

with(test_fruit_df, xtabs(~ treatment + treat_assignment1))
with(test_fruit_df, xtabs(~ treatment + treat_assignment2))
with(test_fruit_df, xtabs(~ treat_assignment1 + treat_assignment2))


