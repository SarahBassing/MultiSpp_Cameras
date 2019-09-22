  ##  Occupancy models for TWS
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ===================================
  ##  Occupancy models for cougars and mule deer in the Okanogan study area between
  ##  June 13 and Sept. 30, 2018. Detection data collected via camera traps,
  ##  covariate data derived from remotely sensed data and collected at camera
  ##  sites. Study objectives are to evaluate discrepencies in habitat use for 
  ##  these two species compared to GIS collar based RSF estiamtes of habitat use.
  ##  Do multispieces monitoring designs misrepresent habitat use/preferences for
  ##  some species when they are not the sole target of the study?
  ##  ===================================
  
  #  Load packages
  library(unmarked)
  
  #  Read in data (generated with camtrapR)
  cougar_dh <- read.csv("Output/Cougar__detection_history__no_effort__7_days_per_occasion__occasionStart0h__first_day_2018-06-13__2019-09-21.csv")
  muledeer_dh <- read.csv("Output/Mule Deer__detection_history__no_effort__7_days_per_occasion__occasionStart0h__first_day_2018-06-13__2019-09-21.csv")
  covs <- read.csv("Input/cam_covs.csv")
  
  #  Trim down detection history
  #  Only need occasions1 - 16 (i.e., weeks of June 13 - Oct 3, 2018)
  cougar_dh <- cougar_dh[,1:17]
  
  #  Merge detection history with covariates dataframe
  cougdat <- merge(cougar_dh, covs, by.x = "X", by.y = "Cell_ID")

  #  Set aside just the detection history
  coughist<-as.matrix(cougdat[,2:ncol(cougar_dh)])
  
  #  Center and scale continuous covariates
  #  Turn categorical covariates into factors
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
  
  nrows <- nrow(cougdat)
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
  
  #  Create unmarked dataframe
  cougUMF<-unmarkedFrameOccu(coughist,
                             siteCovs = data.frame(landcov = NLCD, elev = Elev,
                                        tri = Ruggedness, rd_density = Rd_Density,
                                        rd_dist = Nearest_Rd, water = Water_Density,
                                        dist_focal = Dist_focal_pt, focal_feat = Monitoring,
                                        canopy = Canopy_Cover, mngt = Land_mgnt,
                                        habitat = Habitat_type), 
                             obsCovs = srvy_covs)
  
  #  Occupancy models
  #  Remember, detection comes first, then occupancy
  #  Null model
  coug_null <- occu(~1 ~1, cougUMF)
  
  #  Detection models
  coug_det1 <- occu(~focal_feat ~1, cougUMF) # focal feature
  coug_det2 <- occu(~dist_focal ~1, cougUMF) # distance to focal point    ---- dropped rows due to missing data
  coug_det3 <- occu(~Height ~1, cougUMF)     # camera height from ground  ---- dropped rows due to missing data
  
  det_mods <- fitList(m1 = coug_null, m2 = coug_det1, m3 = coug_det2, m4 = coug_det3)
  modSel(det_mods)
  
  #  Occurrence models with best detection model
  #  Currently the null model is best given that nothing was significant
  coug_mod1 <- occu(~1 ~landcov, cougUMF)        # land cover type
  coug_mod2 <- occu(~1 ~elev, cougUMF)           # elevation
  coug_mod3 <- occu(~1 ~tri, cougUMF)            # terrain ruggedness index
  coug_mod4 <- occu(~1 ~rd_density, cougUMF)     # road density
  coug_mod5 <- occu(~1 ~rd_dist, cougUMF)        # distance to nearest road
  coug_mod6 <- occu(~1 ~water, cougUMF)          # density of streams
  coug_mod7 <- occu(~1 ~canopy, cougUMF)         # percent canopy cover   ---- drops rows due to missing data
  coug_mod8 <- occu(~1 ~habitat, cougUMF)        # site-specific habitat type
  coug_mod9 <- occu(~1 ~mngt, cougUMF)           # land management
  
  #  Determine which road covariate to use in the final model selection process
  rd_mods <- fitList(m1 = coug_mod4, m2 = coug_mod5)
  modSel(rd_mods)
  
  #  Model selection of occupancy models
  occ_mods <- fitList(m1 = coug_mod1, m2 = coug_mod2, m3 = coug_mod3, m4 = coug_mod4, 
                      m5 = coug_mod6, m6 = coug_mod8, m7 = coug_mod9)  #  coug_mod7 dropped rows and can't be compared to rest of model set
  modSel(occ_mods)
  