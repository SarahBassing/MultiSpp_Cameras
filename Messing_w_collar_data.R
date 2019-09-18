## Messing with collar data for now

  #  Load required packages
  library(tidyverse)    
  library(sf)
  library(sp)
  library(adehabitatHR)
  library(rgeos)
  library(ggplot2)
  library(anytime)
  
  #  Read in data and format
  
  cougars <- read.csv("Data/Cougar_AllPoints.csv", header = TRUE, stringsAsFactors = FALSE) %>%
    transmute(
      No = as.integer(No),
      Animal_ID = as.character(ID),
      Collar_ID = as.character(Collar),
      TimeStamp = as.character(LMT_DateTime),
      DateTime = anytime(TimeStamp),        # formats date & time to POSIXct
      location_long = as.numeric(Long),
      location_lat = as.numeric(Lat)
    )
  # DateTime <- anytime(cougars$TimeStamp)
  
  #  Add column for study area- First 2 letters in Animal ID stand for study area
  
  
  view(cougars)
  
  #  I only want collar data from 06/01/2018 - 08/31/2018 for now
  cougars_summer18 <- cougars[cougars$DateTime > "2018-05-31 23:59:59" & cougars$DateTime < "2018-09-01 00:00:00", ]
  
  min(cougars_summer18$DateTime); max(cougars_summer18$DateTime)
  
  #  Make GPS data into a sf spatial dataframe and reproject
  cougarlocs <- cougars_summer18[,c("location_long", "location_lat")]
  cougarlocs_sp <- SpatialPoints(cougarlocs, proj4string = CRS("+proj=longlat +ellps=WGS84"))
  
  #  Reproject into UTMs
  #  For now, we're using a projection specific to northern WA which covers both
  #  study areas: Proj4js.defs["EPSG:2855"] = "+proj=lcc +lat_1=48.73333333333333 
  #  +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 
  #  +ellps=GRS80 +units=m +no_defs"
  cougarlocsUTM_sp <- spTransform(cougarlocs_sp, 
                                  CRS("+init=epsg:2855"))
  cougarpoints_UTM <- coordinates(cougarlocsUTM_sp)
  
  #  Add UTMs to cougars dataframe
  cougars_summer18$Longitude <- cougarpoints_UTM[,1]
  cougars_summer18$Latitude <- cougarpoints_UTM[,2]
  view(cougars_summer18)
  
  #  Import study area shapefiles, reproject, and plot to double check everything
  OK <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/fwdstudyareamaps/StudyAreaPolygons/METHOW_SA.shp")
  OK <- st_transform(OK, CRS("+init=epsg:2855"))
  NE <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/fwdstudyareamaps/StudyAreaPolygons/NE_SA.shp")
  NE <- st_transform(NE, CRS("+init=epsg:2855"))
  wa <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Washington_State_Boundary/Washington_State_Boundary.shp")
  WA <- st_transform(wa, CRS("+init=epsg:2855"))
  
  #  Double check that it reprojected correctly
  st_crs(NE)
  
  
  ####  Home range estimation  ####
  #  Estimate home range for each individual animal so "available" points can be
  #  randomly selected within each animal's home range--- 3rd order selection
  #  Start with just one individual
  Cougar1 <- cougars_summer18[cougars_summer18$Animal_ID == "MVC202F",]
  Cougar1locs <- Cougar1[, c("Longitude", "Latitude")]
  Cougar1locs_sp <- SpatialPoints(Cougar1locs, proj4string = CRS("+init=epsg:2855"))
  Cougar1locs_sp2 <- st_as_sf(Cougar1locs, coords = 1:2, crs = "+init=epsg:2855")
  
  #  Create bivariate normal utilization distribution based on individual animal's home range
  UD <- kernelUD(Cougar1locs_sp, kern = "bivnorm")
  #  Create polygon of 95% UD
  UD95 <- getverticeshr(UD, 95)
  #  Calculate area of 95% UD in square km
  kernel.area(UD, 95, unin = "m", unout ="km")
  
  ####  Selecting "available" points  ####
  #  Count number of "used" points per individual animal
  nlocs <- nrow(Cougar1)
  
  #  Select random points within the home range
  rndpts <- spsample(UD95, nlocs, type = "random")
  rndpts_sp <- SpatialPoints(rndpts, CRS("+init=epsg:2855"))
  rndpts_sp2 <- st_as_sf(rndpts, coords = 1:2, crs = "+init=epsg:2855")
  
  
  ####  Covariates  ####
  #  Read in shapefiles for each covariate of interest: 
  #  -distance to road (continuous)
  #roads <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WADNR_Washington_Active_Roads", layer = "WADNR_Washington_Active_Roads")
  # roads <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WADNR_Washington_Active_Roads/WADNR_Washington_Active_Roads.shp")
  # roads <- st_transform(roads, CRS("+init=epsg:2855"))
  # roads_OK <- st_crop(roads, OK)
  # roads_NE <- st_crop(roads, NE)
  # st_write(roads_OK, dsn = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Roads_OK", layer = "roads_OK", driver = "ESRI Shapefile")
  # st_write(roads_NE, dsn = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Roads_NE", layer = "roads_NE", driver = "ESRI Shapefile")
  roads_OK <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Roads_OK/roads_OK.shp", 
                      crs = "+init=epsg:2855") 
  roads_NE <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Roads_NE/roads_NE.shp", 
                      crs = "+init=epsg:2855")
  
  
  #  -distance to water (continuous)
  #  -landcover type (within a certain buffer of each point? percent land cover type?) (categrorical or continuous depending)
  #  -NDVI (continuous)
  #  -percent forest cover (continuous)
  #  -burn severity and/or perimeter (categorical)
  #  -elevation (continuous)
  #  -ruggedness (continuous)
  
  #  Extract covariate data for "used" and "available" samples
  road_coug_dist <- st_distance(y = Cougar1locs_sp2, x = roads_OK)
  Cougar1locs_sp2$road_dist <- apply(road_coug_dist, 2, min)
  road_rnd_dist <- st_distance(y = rndpts_sp2, x = roads_OK)
  
  ####  Input data for model  ####
  #  "used" = 1, "available" = 0
  use <- rep(1, nlocs)
  av <- rep(0, nlocs)
  
  #  Standardize covariates
  
  
  ####  GLMMs with random effect for individual  ####
  #  Logistic regressions with the lme4 package, which allows for random effects
  
  #  Model seleciton... should really do k-fold but probably gonna use AIC for now
  
  
  
  
  
  
  
  
  
  
  
  
  #  If I can't handle the sf package...
  OK <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/fwdstudyareamaps/StudyAreaPolygons", layer = "METHOW_SA")
  OK <- spTransform(OK, CRS("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47
                               +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80
                               +units=m +no_defs"))
  NE <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/fwdstudyareamaps/StudyAreaPolygons", layer = "NE_SA")
  NE <- spTransform(NE, CRS("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47
                               +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80
                               +units=m +no_defs"))
  
  wa <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Washington_State_Boundary", layer = "Washington_State_Boundary")
  WA <- spTransform(wa, CRS("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47
                             +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80
                             +units=m +no_defs"))
  
  #  Take a quick look
  plot(WA, col = "lightgray")
  plot(OK, col = "lightgreen", add = T)
  plot(NE, col = "lightgreen", add = T)
  plot(cougarlocsUTM_sp, pch = 19, col = "darkblue", add = T)
  
  
  # gmu <- st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/fwdstudyareamaps/GMU_Generalized.shp")
  # dem <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WA DEM rasters/wa_dem1.img")
