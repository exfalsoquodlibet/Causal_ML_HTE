# -------------------------------------------- #
# Understanding Heterogenous Treatment Effects #
# -------------------------------------------- #


### Set up ------

# Packages ---

pckgs <- c("dplyr", 
      "grf",
      "ggplot2")

lapply(pckgs, require, character.only=TRUE)

# User-defined functions ---

source("functions/CATE_summary_fun.R")    # to calculate conditional average treatment effects



### Data ------

fruit_df <- readRDS("data/synth_hte_df.rds")



### What are Heterogenous Treatment Effects (HTE)? ------

# Note: also known as Conditional Treatment Effects (CATE)

# HTE = the positive difference in the outcome that a treatment is expected to have over the control in a subgroup of the population
# ==> i.e., the difference in outcome Y between treatment and control for a subgroup of individuals (defined based on covariate profiles)
# ==> if binary outcome, Pr(Y = 1| treatment_group | subgroup_i) - Pr(Y = 1| control_group | subgroup_i)

# note: subgroup_i can be single individuals!
# for an individual, a treatment effect is the difference in *potential* outcomes if the 
# individual receives the treatment vs. if they don't


# Why are we interested in HTE?

# To understand how effective the treatment would be for a particular individual or subpopulation

# To identify those subgroups where the "treatment effect" is bigger, so that we can 
# subsequently focus our effort on those groups and deliver the intervention only to them
# But also, to identify those subgroups where the treatment "backfired", so that we can
# subsequently decide not to roll out the treatment-intervention on those groups

# So our aim is "maximise the treatment effect" (find where it is bigger)
# ==> estimate the treatment effect for each individual, so that we can decide whether to target it or not


# BUT... problem of causal inference / counterfactual:

# an individual is either assigned to the control or to (one of) the treatment, so
# for any individual i, we only observe Y_i(treatment) or Y_1(control)


# So, how do we do it?

# 1) We run a randomised control experiment, randomly assigning individuals to treatment vs control
# 2) We train a model on the expected treatment effect for every individual 
# the estimates for the treatment effect may vary largely for different subgroups of individuals
# 3) We apply the model to future potential targets, and we deliver our treatment to those
# identified by the model as having the highest treatment effect.

# As a given individual is only assigned either to the treatment or the control,
# we estimate their treament effect by searching "similar others" and use them as stand-ins for  
# the individual's potential outcome
# ==> we compare people with similar covariate profiles in different experimental conditions
# use these differences as estimates of a treatment effect
# ==> assign that treatment effect to everyone in that covariate-profile subgroup

# E.g., my own "similar others" would be "white, female, with a postgraduate degree, etc..."



### HTE (or CATE) in our data ------

# Average treatment effect (ATE) ---
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment)

# Conditional treatment effects (CATEs) ---
# we know these exist as that's how we created-simulated the data
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, overweight)
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, race)
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, overweight, race)

# other covariates
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, male)
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, statesch)
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, race, statesch )
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, siblings )
CATE_summary_fun(fruit_df, outcome_binary=TRUE, chose_fruit, treatment, age )

