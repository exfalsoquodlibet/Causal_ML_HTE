# --------------------------------------------- #
# Simulate data from a randomised control trial #
# --------------------------------------------- #

# -------- #
# Packages #
# -------- #

pckgs <- c(
      "dplyr", 
      "tidyverse", 
      "stringr",
      "ggplot2",
      "simstudy"   # see here for Ref: https://www.rdatagen.net/page/define_and_gen/  https://cran.rstudio.com/web/packages/simstudy/vignettes/simstudy.html
      )

lapply(pckgs, require, character.only=TRUE)





### I took inspiration from this study:

# The effect of images of Michelle Obama’s face on trick-or-treaters’ dietary choices
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0189693 

# Eligibility
# Trick-or-treaters over the age of three that approached the house.

# Intervention
# Random assignment to the Michelle Obama side of the porch or the Comparison side of the porch.

# Main outcome measure (binary)
# Selection of fruit over candy.



set.seed(777)


### User input ------

n_sample <- 10000


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
         sample(c(1, 0), nrow(study1), TRUE, c(.55, .45))
  )))


# Let's take a look at proportion of children who chose fruits  
study1 %>% 
      group_by(overweight, race, treatment) %>% 
      summarise(
            mean(chose_fruit, na.rm=TRUE), 
            n = n()
            )



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
    gender = factor(male, levels=c(0,1), labels = c('F', 'M'))
        ) %>%
      ggplot(.) +
      geom_bar(aes(x = treatment, fill=gender), stat = 'count') +
      theme_light()


study1 %>%
  mutate(
    treatment = factor(treatment),
    racename = factor(race, levels=c(1,2,3,4), labels=c('caucasian', 'africanAm', 'asian', 'hispanic'))) %>%
  ggplot(.) +
  geom_bar(aes(x = treatment, fill=racename), stat = 'count') +
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


### Save data ------
saveRDS(study1, "data/synth_hte_df.rds")


### Clean up ------
rm(list=c("def", "study1", "pckgs"))
