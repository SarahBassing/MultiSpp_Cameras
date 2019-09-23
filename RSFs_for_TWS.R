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
  
  #  Read in data and organize
  coug_18_used_covs <- read.csv("Input/coug18_used_covs.csv") %>%
    dplyr::select("Used", "Animal_ID", "Region", "Longitude", "Latitude", 
                  "NLCD", "Elev", "TRI", "Rd_Density_km", "Water_Density_km")
  coug_18_avail_covs <- read.csv("Input/coug18_avail_covs.csv") %>%
    dplyr::select("Used", "Animal_ID", "Region", "Longitude", "Latitude", 
                  "NLCD", "Elev", "TRI", "Rd_Density_km", "Water_Density_km")
  
  head(coug_18_used_covs)
  str(coug_18_used_covs)
  head(coug_18_avail_covs)
  str(coug_18_avail_covs)
  
  #  Combine into a single dataframe
  coug18_rsf_data <- rbind(coug_18_used_covs, coug_18_avail_covs)
  str(coug18_rsf_data)
  
  #  Center and scale covariates
  coug18_rsf_dataz <- coug18_rsf_data %>%
    transmute(
      used = as.factor(Used),
      Animal_ID = as.factor(Animal_ID),
      Region = as.factor(Region),
      Longitude = as.numeric(scale(Longitude)),
      Latitude = as.numeric(scale(Latitude)),
      nlcd = as.factor(NLCD),
      elev = as.numeric(scale(Elev)),
      tri = as.numeric(scale(TRI)),
      rd_dnsty = as.numeric(scale(Rd_Density_km)),
      h20_dnsty = as.numeric(scale(Water_Density_km))
    )
  head(coug18_rsf_dataz)
  
  ####  Models  ####
  #  ===========================
  #  Logistic mixed effects models that include random effect for individual
  
  #  Null
  mod0 <- glm(used ~ 1, data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod1)
  
  #  Random effect for individual
  mod1 <- glmer(used ~ 1 + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod1)
  
  #anova(mod1, mod2)
  
  #  NLCD
  mod2 <- glmer(used ~ nlcd + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod2)
  
  #  Elevation
  mod3 <- glmer(used ~ elev + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod3)
  
  #  Quadratic Elevation
  mod4 <- glmer(used ~ elev + I(elev^2) + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod4)
  
  #  Terrain Ruggedness Index
  mod5 <- glmer(used ~ tri + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod5)

  #  Density of Roads
  mod6 <- glm(used ~ rd_dnsty + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod6)
  
  #  Density of Streams
  mod7 <- glmer(used ~ h20_dnsty + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod7)
  
  #  It's a terrain thing: elev (+ elev^2 depending on if it makes sense) + tri
  mod8 <- glmer(used ~ elev + tri + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod8)
  
  #  Habitat as an index for prey availablity: nlcd + h20_dnsty
  mod9 <- glmer(used ~ nlcd + h20_dnsty + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod9)
  
  #  Hunting strategies: nlcd + tri
  mod10 <- glmer(used ~ nlcd + tri + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod10)

  #  Hunting strategy & locomotion: nlcd + tri + rd_dnsty
  mod11 <- glmer(used ~ nlcd + tri + rd_dnsty + (1|Animal_ID), data = coug18_rsf_dataz, family = binomial(link = "logit"))
  summary(mod11)
  
  
  ####  Model selection  ####
  #  ===========================
  #  Should probably use K-fold model selection but since I'm pressed for time...
  #  it's gonna be AIC
  
  #  Is the random effect needed? Probably going to use it even if not
  AIC(mod0, mod1)
  
  #  All models except mod0
  AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11)
  
  #  backtransform top model
  
  
  