  ##  RSF models for TWS 2019
  ##  Sarah Bassing
  ##  Washingotn Predator-Prey Project
  ##  Sept 2019
  ##  ===========================
  ##  Testing hypotheses about habitat selection using GPS data from collared
  ##  cougars in eastern Washington. To be compared wtih occupancy models 
  ##  populated with data from camera traps in same study area.
  ##  ===========================
  
  #  Load packages
  library(tidyverse)
  library(lme4)
  library(maxlike)  ###???????? 
  
  #  Read in data
  dat <- read.csv("./Input/dat.csv")
  #dat <- load("Input/dat.RData")  # not working right now :( 
  
  head(dat)
  str(dat)
  
  ####  Models  ####
  #  ===========================
  #  Logistic mixed effects models that include random effect for individual
  #  Using test data right now with only one collared animal so can't include
  #  a random effect. Also don't forget that covariate data is FAKE!
  
  #  Null
  mod1 <- glm(used ~ 1, data = dat, family = binomial(link = "logit"))
  summary(mod1)
  
  #  Distance to closest open road
  mod2 <- glm(used ~ road_dist_z, data = dat, family = binomial(link = "logit"))
  summary(mod2)
  
  
  ####  Model selection  ####
  #  ===========================
  #  Should probably use K-fold model selection but since I'm pressed for time...
  #  it's gonna be AIC
  AIC(mod1, mod2)
  
  
  