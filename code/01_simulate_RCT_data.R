# --------------------------------------------- #
# Simulate data from a randomised control trial #
# --------------------------------------------- #

# -------- #
# Packages #
# -------- #

pckgs <- c("dplyr", "tidyverse", "stringr", 
           "simstudy")
lapply(pckgs, require, character.only=TRUE)

# see here for Ref: https://www.rdatagen.net/page/define_and_gen/ 
# https://cran.rstudio.com/web/packages/simstudy/vignettes/simstudy.html

set.seed(777)


### I took inspiration from this study:

# The effect of images of Michelle Obama’s face on trick-or-treaters’ dietary choices
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0189693 

# Eligibility
# Trick-or-treaters over the age of three that approached the house.

# Intervention
# Random assignment to the Michelle Obama side of the porch or the Comparison side of the porch.

# Main outcome measure (binary)
# Selection of fruit over candy.



### User input ------

n_sample <- 1000

### Create covariates ------

# gender (53/47 prob male/female)
def <- simstudy::defData(varname = "male", dist = "binary", formula = .53 , link = "identity", id="cid")

# age
def <- simstudy::defData(def, varname = "age", dist = "uniformInt", 
      formula = "4;12", link="identity")

# child's race (caucasian; africanAm; asian; hispanic)
def <- simstudy::defData(def, varname = "race", formula = "0.4;0.3;0.1;0.2", dist = "categorical")

# is child overweight? (log-odds is a function of gender)
def <- simstudy::defData(def, varname = "overweight", dist = "binary", formula = "0.8 + 0.3*male", link = "logit")

# have siblings? (prob)
def <- simstudy::defData(def, varname = "siblings", dist = "binary", formula = ".6", link = "identity")

# household income (as a function of race)
def <- simstudy::defData(def, varname = "hhinc", dist = "negBinomial", formula = "log(97000 + 30000*(1/race))", variance = 0.05, link = "log")

# does it go to state school? (log-odds as a function of household income)
def <- simstudy::defData(def, varname = "statesch", dist = "binary", formula = ".6 + 10000/hhinc", link = "logit")


# Generate data ------
dt_study <- genData(n_sample, def)


# Treatment assignmnet ------
# Let's go for *balanced* treatment assignment (with stratification by gender and race)

study1 <- trtAssign(
  dt_study,   # data 
  n = 2,      # two experimental condition (intervention and control)
  balanced = TRUE, 
  strata = c("male", "race"), 
  grpName = "treatment"
  )


# Outcome (and thus, treatment effect) ------

# binary outcome (child chose fruit 1 or candy 0)
# let's simulate a positive treatment effect for overweight children and a negative effect for non-overweight black children 
# so the outcome is a function of treatment, overweight, and race
# the other are nuisance covariates

study1[['chose_fruit']] <- with(study1, ifelse(
  overweight == 1 & treatment == 1,
  sample(c(1, 0), nrow(study1), TRUE, c(.70, .30)), ifelse(
    race == 2 & treatment == 1,
         sample(c(1, 0), nrow(study1), TRUE, c(.15, .85)),
         sample(c(1, 0), nrow(study1), TRUE, c(.50, .50))
  )))

  
study1 %>% group_by(overweight, race, treatment) %>% summarise(mean(chose_fruit, na.rm=TRUE), n = n())





# Let's take a look at the treatment effect in the descriptive
View(study1)

# Average treatment effect
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment)

# Conditional treatment effects
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, overweight)
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, race)
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, overweight, race)


CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, male)
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, statesch)
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, race, statesch )
CATE_summary_fun(study1, outcome_binary=TRUE, chose_fruit, treatment, siblings )




### Design ------

# show that it is indeed balanced
study1 %>%
  mutate(treatment = factor(treatment)) %>%
  ggplot(., 
      aes(x = treatment)) +
      geom_bar(aes(fill=treatment), stat = 'count') +
      theme_light()
      
# show that it has been stratified by gender and race
study1 %>%
  mutate(
    treatment = factor(treatment),
    gender = factor(male, levels=c(0,1), labels = c('F', 'M')),
    racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))) %>%
  ggplot(.) +
  geom_bar(aes(x = treatment, fill=racename), stat = 'count') +
  theme_light()


study1 %>%
  mutate(
    treatment = factor(treatment),
    gender = factor(male, levels=c(0,1), labels = c('F', 'M')),
    racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))) %>%
  ggplot(.) +
  geom_bar(aes(x = treatment, fill=gender), stat = 'count') +
  theme_light()

# but not by - e.g., statesch
study1 %>%
  mutate(
    treatment = factor(treatment),
    statesch_f = factor(statesch, levels=c(0,1), labels = c('no', 'yes')),
    racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))) %>%
  ggplot(.) +
  geom_bar(aes(x = treatment, fill=statesch_f), stat = 'count') +
  theme_light()





# https://roamanalytics.com/2016/10/28/are-categorical-variables-getting-lost-in-your-random-forests/ 
# RF will tend to favour highly variable continuous predictorsor (and also categorical predictors with many more levels than the others) because there are more opportunities to partition the data. 
# 

study1 <- study1 %>%
  mutate(
    s_age = scale(age, center = TRUE, scale = TRUE),
    s_hhinc = scale(hhinc, center = TRUE, scale = TRUE)
  )




# Fit the causal model ------

trans_cf <- grf::causal_forest(
      # dummy-code categorical variables
      X = model.matrix(~ ., data = study1[, ! names(study1) %in% c('chose_fruit', 'treatment', 'cid', 'age', 'hhinc')]),
      
      # outcome variable
      Y = study1$chose_fruit,
      
      # treatment variable
      W = study1$treatment,
      
      mtry = NULL, #default, number of variables tried for each split
      num.trees = 5000,
      min.node.size = NULL, # minimum number of cases in each node (end leaf)
      
      honesty = TRUE, # default >>> honest sub-sample splitting
      honesty.fraction = NULL, # default, 50% in splitting sub-sample, 50% in estimating sub-sumple
      
      tune.parameters = TRUE, #parameters tuned by cross-validation
      seed = 123
      
)


summary(trans_cf)

trans_cf$num.trees

# parameters that were tunable
trans_cf$tunable.params
trans_cf$tuning.output   #Optimal values of tuning parameters

# Rank of important covariate -------
# biased approach, as it has a tendency to inflate the importance of continuous features or high-cardinality categorical variables...

trans_cf %>% 
      variable_importance() %>% 
      as.data.frame() %>% 
      mutate(variable = colnames(trans_cf$X.orig)) %>% 
      arrange(desc(V1))

# Estimating average treatmen effects ------
grf::average_treatment_effect(trans_cf, target.sample = "all")
grf::average_treatment_effect(trans_cf, target.sample = "treated")
grf::average_treatment_effect(trans_cf, target.sample = "control")

grf::average_treatment_effect(trans_cf, target.sample = "all", subset=study1[, 'overweight'] ==1 )
grf::average_treatment_effect(trans_cf, target.sample = "all", subset=study1[, 'overweight'] ==0 )
grf::average_treatment_effect(trans_cf, target.sample = "all", subset=study1[, 'age'] < 5 )
grf::average_treatment_effect(trans_cf, target.sample = "all", subset=study1[, 'race'] == 2 )




# Reference:
# https://github.com/grf-labs/grf
# https://www.researchgate.net/publication/330266878_The_influence_of_maternal_agency_on_severe_child_undernutrition_in_conflict-ridden_Nigeria_Modeling_heterogeneous_treatment_effects_with_machine_learning
# https://github.com/grf-labs/grf/issues/238
# https://github.com/grf-labs/grf/blob/master/r-package/grf/R/causal_forest.R
# http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf 
