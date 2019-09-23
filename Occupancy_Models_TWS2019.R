  ##  Occupancy models for TWS
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ===================================
  ##  Occupancy models for cougars and mule deer in the Okanogan study area between
  ##  June 13 and Sept. 30, 2018. Detection data collected via camera traps,
  ##  covariate data derived from remotely sensed data and collected at camera
  ##  sites. Study objectives are to evaluate discrepencies in habitat use for 
  ##  these two species compared to GPS collar based RSF estiamtes of habitat use.
  ##  Do multispieces monitoring designs misrepresent habitat use/preferences for
  ##  some species when they are not the sole target of the study?
  ##  ===================================
  
  #  Load packages
  library(unmarked)
  
  #  Read in data (generated with camtrapR)
  cougar_dh <- read.csv("Output/Cougar__detection_history__no_effort__7_days_per_occasion__occasionStart0h__first_day_2018-06-13__2019-09-21.csv")
  muledeer_dh <- read.csv("Output/Mule Deer__detection_history__no_effort__7_days_per_occasion__occasionStart0h__first_day_2018-06-13__2019-09-21.csv")
  covs <- read.csv("Input/cam_covs.csv")
  
  
  #  Format detection and covariate data for unmarked
  #  ===========================
  #  Trim down detection history
  #  Only need occasions1 - 16 (i.e., weeks of June 13 - Oct 3, 2018)
  cougar_dh <- cougar_dh[, 1:17]
  muledeer_dh <- muledeer_dh[, 1:17]
  
  #  Merge detection history with covariates dataframe
  cougdat <- merge(cougar_dh, covs, by.x = "X", by.y = "Cell_ID")
  muledat <- merge(muledeer_dh, covs, by.x = "X", by.y = "Cell_ID")

  #  Set aside just the detection history
  coughist <- as.matrix(cougdat[, 2:ncol(cougar_dh)])
  mulehist <- as.matrix(muledat[, 2:ncol(muledeer_dh)])
  
  #  Center and scale continuous covariates
  #  Turn categorical covariates into factors
  #  Covariates same for both species so only need to do this once
  NLCD <- as.factor(cougdat$NLCD)
  Elev <- scale(cougdat$Elev)
  Ruggedness <- scale(cougdat$Ruggedness)
  Rd_Density <- scale(cougdat$Rd_Density)
  Nearest_Rd <- scale(cougdat$Nearest_Rd)
  Water_Density <- scale(cougdat$Water_Density)
  Dist_focal_pt <- scale(cougdat$Dist_focal_pt)
  Monitoring <- as.factor(cougdat$Monitoring)
  Canopy_Cover <- scale(cougdat$Canopy_Cover)
  Land_mgnt <- as.factor(cougdat$Land_mgnt)
  Habitat_type <- as.factor(cougdat$Habitat_type_reduced)
  Height_o1 <- scale(cougdat$Height_o1)
  Height_o2 <- scale(cougdat$Height_o2)
  Height_o3 <- scale(cougdat$Heigh_o3)  # note the typo
  Height_o4 <- scale(cougdat$Height_o4)
  Height_o5 <- scale(cougdat$Height_o5)
  Height_o6 <- scale(cougdat$Height_o6)
  Height_o7 <- scale(cougdat$Height_o7)
  Height_o8 <- scale(cougdat$Height_o8)
  Height_o9 <- scale(cougdat$Height_o9)
  Height_o10 <- scale(cougdat$Height_o10)
  Height_o11 <- scale(cougdat$Height_o11)
  Height_o12 <- scale(cougdat$Height_o12)
  Height_o13 <- scale(cougdat$Height_o13)
  Height_o14 <- scale(cougdat$Height_o14)
  Height_o15 <- scale(cougdat$Height_o15)
  Height_o16 <- scale(cougdat$Height_o16)
  
  #  How correlated are road density and distance to nearest road?
  cor(Rd_Density, Nearest_Rd)
  
  #  Set dimensions for survey-level covaraite matrix
  nrows <- nrow(covs)
  ncols <- 16
  
  srvy_covs <- list(
    Height = matrix(c(Hgto1 = Height_o1, Hogt2 = Height_o2,
                      Hgto3 = Height_o3, Hgto4 = Height_o4,
                      Hgto5 = Height_o5, Hgto6 = Height_o6,
                      Hgto7 = Height_o7, Hgto8 = Height_o8,
                      Hgto9 = Height_o9, Hgto10 = Height_o10,
                      Hgto11 = Height_o11, Hgto12 = Height_o12,
                      Hgto13 = Height_o13, Hgto14 = Height_o14,
                      Hgto15 = Height_o15, Hgto16 = Height_o16), 
    nrow = nrows, ncol = ncols, byrow = TRUE)) 
  
  
  #  Create unmarked dataframes
  #  ==========================
  #  Cougars
  cougUMF<-unmarkedFrameOccu(coughist,
                             siteCovs = data.frame(landcov = NLCD, 
                                                   elev = Elev,
                                                   tri = Ruggedness, 
                                                   rd_density = Rd_Density,
                                                   rd_dist = Nearest_Rd, 
                                                   water = Water_Density,
                                                   dist_focal = Dist_focal_pt, 
                                                   focal_feat = Monitoring,
                                                   canopy = Canopy_Cover, 
                                                   mngt = Land_mgnt,
                                                   habitat = Habitat_type), 
                             obsCovs = srvy_covs)
  
  #  Mule deer
  muledeerUMF<-unmarkedFrameOccu(mulehist,
                             siteCovs = data.frame(landcov = NLCD, 
                                                   elev = Elev,
                                                   tri = Ruggedness, 
                                                   rd_density = Rd_Density,
                                                   rd_dist = Nearest_Rd, 
                                                   water = Water_Density,
                                                   dist_focal = Dist_focal_pt, 
                                                   focal_feat = Monitoring,
                                                   canopy = Canopy_Cover, 
                                                   mngt = Land_mgnt,
                                                   habitat = Habitat_type), 
                             obsCovs = srvy_covs)
  
  
  #  Occupancy models
  #  ========================
  #  Remember: detection comes first, then occupancy in the occu function
  
  #  Cougar models
  #  Null model
  (coug_null <- occu(~1 ~1, cougUMF))
  
  #  Detection models
  #  Determine most supported detection model first
  (coug_det1 <- occu(~focal_feat ~1, cougUMF)) # focal feature
  (coug_det2 <- occu(~dist_focal ~1, cougUMF)) # distance to focal point    ---- dropped rows due to missing data
  (coug_det3 <- occu(~Height ~1, cougUMF))     # camera height from ground  ---- dropped rows due to missing data
  
  coug_det_mods <- fitList(m1 = coug_null, m2 = coug_det1)  
  modSel(coug_det_mods)
  #  Can't compare m3 = coug_det2, m4 = coug_det3 to null & det1 b/c likelihoods different
  
  
  #  Occurrence models with best detection model
  #  Currently the null model is best given that nothing was significant
  (coug_mod1 <- occu(~1 ~landcov, cougUMF))        # land cover type
  (coug_mod2 <- occu(~1 ~elev, cougUMF))           # elevation
  (coug_mod2.5 <- occu(~1 ~elev + I(elev^2), cougUMF))  # quadratic elevation
  (coug_mod3 <- occu(~1 ~tri, cougUMF))            # terrain ruggedness index
  (coug_mod4 <- occu(~1 ~rd_density, cougUMF))     # road density
  (coug_mod5 <- occu(~1 ~rd_dist, cougUMF))        # distance to nearest road
  (coug_mod6 <- occu(~1 ~water, cougUMF))          # density of streams
  (coug_mod7 <- occu(~1 ~canopy, cougUMF))         # percent canopy cover   ---- drops rows due to missing data
  (coug_mod8 <- occu(~1 ~habitat, cougUMF))        # site-specific habitat type
  (coug_mod9 <- occu(~1 ~mngt, cougUMF))           # land management
  
  #  Determine which road covariate to use in the final model selection process
  coug_rd_mods <- fitList(m4 = coug_mod4, m5 = coug_mod5)
  modSel(coug_rd_mods)
  
  #  Model selection of occupancy models
  coug_occ_mods <- fitList(m1 = coug_mod1, m2 = coug_mod2, m3 = coug_mod3, 
                           m4 = coug_mod4, m6 = coug_mod6, m8 = coug_mod8, 
                           m9 = coug_mod9, m10 = coug_null, m2.5 = coug_mod2.5)  
  modSel(coug_occ_mods)
  #  Only included road density model in final model selection process
  #  coug_mod7 dropped rows and can't be compared to rest of model set
  
  #  Backtransform null model
  #  Only works (.) parameters
  backTransform(coug_null, type = "det")    # det = 0.075, se = 0.024 (VERY low!)
  backTransform(coug_null, type = "state")  # psi = 0.409, se = 0.12
  
  #  Backtransform parameters when covaraite = 0 (which is the mean of that 
  #  covariate since continuous variables have been centered around 0 and scaled
  #  to 1 SD from the mean)... RIGHT? or should it be 0.5 since we want to 
  #  transform to the probability scale and 0.5 on prob scale = 0 on logit scale???

    #  Looking at elevation (mod2) & density of streams (mod6)
  lc_mod2 <- linearComb(coug_mod2, c(1, 0), type = "state")
  lc_mod2.5 <- linearComb(coug_mod2.5, c(1, 0, 0), type = "state")
  lc_mod6 <- linearComb(coug_mod6, c(1, 0), type = "state")
  
  backTransform(lc_mod2)   # psi = 0.948, se = 0.143 at mean elevation???
  backTransform(coug_mod2, type = "det")   # det = 0.0506, se = 0.011
  backTransform(lc_mod2.5) # psi = 0.521, se = 0.259 at mean elevation & elevation^2... not sure how to interperate this
  backTransform(coug_mod2.5, type = "det") # det = 0.069, se = 0.025
  backTransform(lc_mod6)   # psi = 0.38, se = 0.147 at mean density of streams
  backTransform(coug_mod6, type = "det")

  
  
  #  Mule deer models
  #  Null model
  (mule_null <- occu(~1 ~1, muledeerUMF))
  
  #  Detection models
  #  Determine most supported detection model first
  (mule_det1 <- occu(~focal_feat ~1, muledeerUMF)) # focal feature
  (mule_det2 <- occu(~dist_focal ~1, muledeerUMF)) # distance to focal point    ---- dropped rows due to missing data
  (mule_det3 <- occu(~Height ~1, muledeerUMF))     # camera height from ground  ---- dropped rows due to missing data
  
  mule_det_mods <- fitList(m1 = mule_null, m2 = mule_det1)
  modSel(mule_det_mods)
  #  m3 = mule_det2, m4 = mule_det3 cannot be included in this model selection
  #  Although can't be compared with anything else, height is + associated with
  #  mule deer detection, p-value = 0.0501
  
  #  Occurrence models with best detection model
  #  Detection left null
  (mule_mod1 <- occu(~1 ~landcov, muledeerUMF))        # land cover type
  (mule_mod2 <- occu(~1 ~elev, muledeerUMF))           # elevation
  (mule_mod2.5 <- occu(~1 ~elev + I(elev^2), muledeerUMF))  # quadratic elevation
  (mule_mod3 <- occu(~1 ~tri, muledeerUMF))            # terrain ruggedness index
  (mule_mod4 <- occu(~1 ~rd_density, muledeerUMF))     # road density
  (mule_mod5 <- occu(~1 ~rd_dist, muledeerUMF))        # distance to nearest road
  (mule_mod6 <- occu(~1 ~water, muledeerUMF))          # density of streams
  (mule_mod7 <- occu(~1 ~canopy, muledeerUMF))         # percent canopy cover   ---- drops rows due to missing data
  (mule_mod8 <- occu(~1 ~habitat, muledeerUMF))        # site-specific habitat type
  (mule_mod9 <- occu(~1 ~mngt, muledeerUMF))           # land management
  
  
  #  Model selection
  #  Determine which road covariate to use in the final model selection process
  mule_rd_mods <- fitList(m4 = mule_mod4, m5 = mule_mod5)
  modSel(mule_rd_mods)
  
  #  Model selection of occupancy models
  mule_occ_mods <- fitList(m1 = mule_mod1, m2 = mule_mod2, m3 = mule_mod3, m4 = mule_mod4,
                           m5 = mule_mod5, m6 = mule_mod6, m8 = mule_mod8, 
                           m9 = mule_mod9, m10 = mule_null, m2.5 = mule_mod2.5)  
  modSel(mule_occ_mods)
  #  Only included distance-to-road model in final model selection process

  #  Backtransform null model
  #  Only works (.) parameters
  backTransform(mule_null, type = "det")      # det = 0.487, se = 0.021
  backTransform(mule_null, type = "state")    # psi = 0.759, se = 0.055
  
  #  Backtransform parameters when covaraite = 0 (which is the mean of that 
  #  covariate since continuous variables have been centered around 0 and scaled
  #  to 1 SD from the mean)... RIGHT? or should it be 0.5 since we want to 
  #  transform to the probability scale and 0.5 on prob scale = 0 on logit scale???
  
  #  Looking at elevation (mod2), elevation^2 (mod2.5), & terrain ruggedness (mod3) on occupancy
  lc_mod2 <- linearComb(mule_mod2, c(1, 0), type = "state")
  lc_mod2.5 <- linearComb(mule_mod2.5, c(1, 0, 0), type = "state")
  lc_mod3 <- linearComb(mule_mod3, c(1, 0), type = "state")
  
  backTransform(lc_mod2)    # psi = 0.777, se = 0.058 at mean elevation
  backTransform(mule_mod2, type = "det")    # det 0.487, se = 0.021
  backTransform(lc_mod2.5)  # psie = 0.818, se = 0.069 at mean elevation & elevation^2
  backTransform(mule_mod2.5, type = "det")  # det = 0.487, se = 0.021
  backTransform(lc_mod3)    # psi = 0.776, se = 0.057 at mean ruggedness
  backTransform(mule_mod3, type = "det")    # det = 0.487, set = 0.021
  
  

  #  New set of models w/ height on detection
  #  Keep in mind these cannot be compared to the null or the above models
  #  because different likelihoods
  (mule_mod1 <- occu(~Height ~landcov, muledeerUMF))      # land cover type
  (mule_mod2 <- occu(~Height ~elev, muledeerUMF))         # elevation
  (mule_mod2.5 <- occu(~Height ~elev + I(elev^2), muledeerUMF))  # quadratic elevation
  (mule_mod3 <- occu(~Height ~tri, muledeerUMF))          # terrain ruggedness index
  (mule_mod4 <- occu(~Height ~rd_density, muledeerUMF))   # road density
  (mule_mod5 <- occu(~Height ~rd_dist, muledeerUMF))      # distance to nearest road
  (mule_mod6 <- occu(~Height ~water, muledeerUMF))        # density of streams
  (mule_mod7 <- occu(~Height ~canopy, muledeerUMF))       # percent canopy cover   ---- drops rows due to missing data
  (mule_mod8 <- occu(~Height ~habitat, muledeerUMF))      # site-specific habitat type
  (mule_mod9 <- occu(~Height ~mngt, muledeerUMF))         # land management
  
  #  Determine which road covariate to use in the final model selection process
  mule_rd_mods <- fitList(m4 = mule_mod4, m5 = mule_mod5)
  modSel(mule_rd_mods)
  
  #  Model selection of occupancy models
  mule_occ_mods <- fitList(m1 = mule_mod1, m2 = mule_mod2, m3 = mule_mod3, 
                           m5 = mule_mod5, m6 = mule_mod6, m8 = mule_mod8, 
                           m9 = mule_mod9, m2.5 = mule_mod2.5)  
  modSel(mule_occ_mods)
  #  Only included distance-to-road model in final model selection process
  #  mule_mod7 dropped rows and can't be compared to rest of model set
  

  #  Backtransform parameters when covaraite = 0 
  #  Looking at height (det3) on detection
  lc_det3 <- linearComb(mule_det3, c(1, 0), type = "det")
  #  Looking at elevation (mod2), elevation^2 (mod2.5), & terrain ruggedness (mod3) on occupancy
  lc_mod2 <- linearComb(mule_mod2, c(1, 0), type = "state")
  lc_mod2det <- linearComb(mule_mod2, c(1, 0), type = "det")
  lc_mod2.5 <- linearComb(mule_mod2.5, c(1, 0, 0), type = "state")
  lc_mod2.5det <- linearComb(mule_mod2.5, c(1, 0), type = "det")
  lc_mod6 <- linearComb(mule_mod6, c(1, 0), type = "state")
  lc_mod6det <- linearComb(mule_mod6, c(1, 0), type = "det")
  
  backTransform(lc_det3)      # det = 0.488, se = 0.021 at mean camera height
  backTransform(lc_mod2)    # psi = 0.776, se = 0.058 at mean elevation
  backTransform(lc_mod2det)   # det = 0.488, se = 0.021 at mean camera height
  backTransform(lc_mod2.5)  # psi = 0.818, se = 0.07 at mean elevation & elevation^2
  backTransform(lc_mod2.5det) # det = 0.488, se = 0.021 at mean camera height
  backTransform(lc_mod6)    # psie = 0.761, se = 0.056 at mean ruggedness
  backTransform(lc_mod6det)   # det = 0.488, se = 0.021 at mean camera height
  
  
  
  #  Summary stats
  #  ========================
  #  Summarize covariates
  #  Calculate mean value for each continuous variable
  covs_mean <- as.data.frame(apply(covs[,c(5:9,15,17,21:36)], 2, mean, na.rm = T))
  colnames(covs_mean) <- "Average"
  #  Calculate standard error of each continuous variable (accounts for missing data)
  covs_se <- as.data.frame(apply(covs[,c(5:9,15,17,21:36)], 2,
                                 function(x, na.rm = T) {
                                          if(na.rm) x <- na.omit(x)
                                          sqrt(var(x)/length(x)) }))
                                 # function(x) sqrt(var(x)/length(x))
  colnames(covs_se) <- "SE"
  #  Calculate min, max, & median of each continuous variable
  covs_min <- as.data.frame(apply(covs[,c(5:9,15,17,21:36)], 2, min, na.rm = T))
  colnames(covs_min) <- "Min"
  covs_max <- as.data.frame(apply(covs[,c(5:9,15,17,21:36)], 2, max, na.rm = T))
  colnames(covs_max) <- "Max"
  covs_median <- as.data.frame(apply(covs[,c(5:9,15,17,21:36)], 2, median, na.rm = T))
  colnames(covs_median) <- "Median"
  
  #  merge together
  covs_summary <- cbind(covs_mean, covs_se, covs_median, covs_min, covs_max)
  covs_summary <- round(covs_summary, digits = 2)
  
  #  Count levels for each categorical covariate
  NLCD_levels <- nlevels(as.factor(covs$NLCD))
  Monitor_levels <- nlevels(covs$Monitoring)
  Mgnt_levels <- nlevels(covs$Land_mgnt)
  Habitat_leves <- nlevels(covs$Habitat_type_reduced)
  cov_levels <- as.data.frame(c(NLCD_levels, Monitor_levels, Mgnt_levels, Habitat_leves))
  rownames(cov_levels) <- (c("NLCD", "Monitoring", "Land_mgnt", "Habitat_type"))
  colnames(cov_levels) <- "Levels"
  
  
  #  Plot covariates
  #  ====================
  par(mfrow = c(3,3))
  hist(covs$Elev, main = "Elevation", xlab = "Elevation (m)")
  hist(covs$Ruggedness, main = "Terrain Ruggedness", xlab = "TRI")
  hist(covs$Rd_Density, main = "Density of Roads", xlab = "Rd density (km/km2)")
  hist(covs$Nearest_Rd, main = "Distance to Nearest Road", xlab = "Nearest road (m)")
  hist(covs$Water_Density, main = "Density of Streams", xlab = "H20 density (km/km2)")
  hist(covs$Dist_focal_pt, main = "Distance to Feature", xlab = "Distance (m)")
  hist(covs$Canopy_Cover, main = "Percent Canopy Cover", xlab = "Canopy (%)")
  hist(covs$Height_o1, main = "Camera Height, early season", xlab = "Height (m)")
  hist(covs$Height_o16, main = "Camera Height, late season", xlab = "Height (m)")


  
  