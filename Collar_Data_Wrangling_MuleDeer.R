  ##  RSF Data Wrangling for Mule Deer Collar Data
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ===========================
  ##  Script to organize & filter GPS collar data (mule deer), generate individual
  ##  home ranges with kernel utilization distributions (KUD), ranomly sample
  ##  locations from within those home ranges, and extract covaraite data for the 
  ##  "used" & "available locations. Final output is a dataframe of the response 
  ##  variable (0/1) and explanatory covariates for each individual animal.
  
  ##  NOTE: Only considering collar data collected Summer 2018 (Jun 1 - Sept 30)
  ##  for animals that have >100 GPS locations during that time period. 
  ##  Currently focusing on animals in Okanogan study area.
  
  ##  Acknowledgments: skeleton of this code is based on scripts generously 
  ##  provided by Briana Abrahms
  ##  ===========================
  
  #  Load required packages
  library(tidyverse)    
  library(sf)
  library(sp)
  library(adehabitatHR)
  library(rgeos)
  library(rgdal)
  library(ggplot2)
  library(anytime)
  library(stringr)
  
  #  Read in data and format
  mulies <- read.csv("Data/muledeersummer2018.csv", header = TRUE, stringsAsFactors = FALSE) %>%
    transmute(
      No = as.integer(OBJECTID),
      Collar_ID = as.character(CollarID),
      location_lat = as.numeric(Latitude),
      location_long = as.numeric(Longitude), 
      ObsDTPST = as.character(ObsDateTimePST), 
      DateTime = anytime(datetime),        # formats date & time to POSIXct
      Julian = as.numeric(julian)
    )
  # DateTime <- anytime(cougars$TimeStamp)

  head(mulies)
  
  
  ####  Filter and prep data for spatial work  
  #  ========================================
  
  #  I only want collar data from 06/13/2018 - 09/30/2018 for now
  #  First camera in OK went out 06/13/2018
  mulies_summer18 <- mulies[mulies$DateTime > "2018-06-12 23:59:59" & mulies$DateTime < "2018-10-01 00:00:00", ]
  
  #  Double check that I've filtered the data correctly
  min(mulies_summer18$DateTime); max(mulies_summer18$DateTime)
  
  #  Make GPS data into a sf spatial dataframe, reproject, & extract UTMs
  mulie_pts <- mulies_summer18[,c("location_long", "location_lat")] %>%
    st_as_sf(., coords = 1:2, crs = "+proj=longlat +ellps=WGS84") %>%
    st_transform("+init=epsg:2855") %>%
    st_coordinates(.)  # no longer spatial
  
  #  Add UTMs to cougars dataframe
  mulies_summer18$Longitude <- mulie_pts[,1]
  mulies_summer18$Latitude <- mulie_pts[,2] 
  
  #  Count number of points per individual animal
  n_mulie_pts <- mulies_summer18 %>%
    group_by(Collar_ID) %>%
    tally() %>%
    as.data.frame() %>%
    ungroup() %>%
    print()
  
  #  Toss location data from animals with <100 locations in summer 2018
  mulies_summer18 <- mulies_summer18[mulies_summer18$Collar_ID != "23038" & mulies_summer18$Collar_ID != "23056" & mulies_summer18$Collar_ID != "23085",]
  
  #  Review and save (csv, sf, spdf)
  head(mulies_summer18)
  mulies18_used_sf <- st_as_sf(mulies_summer18, coords = 8:9, crs = "+init=epsg:2855")
  proj <- CRS("+init=epsg:2855")
  mulies18_used_spdf <- SpatialPointsDataFrame(coordinates(mulies_summer18[,c("Longitude", "Latitude")]),
                                             proj4string = proj,
                                             data = as.data.frame(mulies_summer18[,1:7]))
  # write.csv(mulies_summer18, file = "./input/mulies_summer18.csv")
  # writeOGR(mulies18_used_spdf, dsn = "./input", layer = "mulies18_used_spdf", driver = "ESRI Shapefile", overwrite = TRUE)
  
  #  Count number of points based on final number of mule deer
  n_studyarea_pts <- mulies_summer18 %>%
    tally() %>%
    as.data.frame() %>%
    print()
  
  
  
  
  ####  Selecting "available" points  
  #  ===============================
  #  Map home ranges for each individual (using kernel utilization distributions)
  #  Randomly sample available points from within each home range
  #  Sampling 10,000 points per home range to have a sufficiently large sample
  #  size to compare used points with (can thin if needed)
  
  #  Dataframe of unique animal IDs
  unq_id <- as.data.frame(mulies_summer18$Collar_ID) %>%
    distinct() 
  colnames(unq_id) <- "Collar_ID"
  
  #  How many unique animals are there?
  n_animals <- nrow(unq_id)
  
  
  #  Function to create SpatialPoints for each indvidual, estimate KUD, and
  #  randomly sample points within KUD. Output is a list of locations by 
  #  individual to use as "available" points for RSF.
  #  ================================================
  
  #  First, split df into list of dataframes by unique animal ID
  ind_animal <- group_split(mulies_summer18, mulies_summer18$Collar_ID)
  
  #  Function
  avail_pts <- function(locations, plotit = F) {
    
    #  Step 1: Make each animal's locations spatial
    locations_sp <- SpatialPoints(locations[,c("Longitude", "Latitude")], 
                                  proj4string = CRS("+init=epsg:2855"))
    
    #  Plot to make sure step 1 worked
    if(plotit) {
      plot(locations_sp, col = "blue", pch = 19)
    }
    
    #  Step 2: Create KUDs for each animal
    #  Bivariate normal utilization distributions
    UD <- kernelUD(locations_sp, kern = "bivnorm")
    #  Create polygons of 95% UD
    UD95 <- getverticeshr(UD, 95)
    
    #  Plot to make sure step 2 worked
    if(plotit) {
      plot(UD95, border = "darkgreen", col = NA)
    }
    
    #  Step 3: Randomly select points within each home range
    set.seed(1234)
    rndpts <- spsample(UD95, 10000, type = "random")
    #  Turn them into SpatialPoints
    rndpts_sp <- SpatialPoints(rndpts, CRS("+init=epsg:2855"))
    
    #  Plot to make sure step 3 worked
    if(plotit) {
      plot(rndpts_sp, col = "red", pch = 19)
    }
    
    #  Step 4: Make list of locations non-spatial
    rndpts_df <- as.data.frame(rndpts_sp)
    
    #  Return list of randomly selected available points per animal
    return(rndpts_df)
    
  }
  
  #  Call function, F = don't plot
  ind_df <- lapply(ind_animal, avail_pts, F)
  
  
  
  #  Mega df to hold all AVAILABLE points corresponding to unique Collar_ID
  #  ====================================
  
  #  List to hold 1000's of randomly selected locations/unique individual
  ID_list <- list(NA)
  
  #  Loop over each unique animal ID and repeat 10000 times
  for(i in 1:n_animals) {
    ID_list[i] <- as.data.frame(rep(unq_id[i,], each = 10000))
  }
  
  #  Unlist the lists and combine into single df
  selected_pts <- plyr::ldply(ind_df, data.frame)
  
  ind_avail_pts <- plyr::ldply(ID_list, data.frame) %>%
    cbind(selected_pts)
  colnames(ind_avail_pts) <- c("Collar_ID", "Longitude", "Latitude")
  
  #  Save for extracting with other scripts (csv, sf, spdf)
  mulies18_avail_sf <- st_as_sf(ind_avail_pts, coords = 2:3, crs = "+init=epsg:2855")
  require(rgdal)
  proj <- CRS("+init=epsg:2855")
  mulies18_avail_spdf <- SpatialPointsDataFrame(coordinates(ind_avail_pts[,c("Longitude", "Latitude")]),
                                              proj4string = proj,
                                              data = as.data.frame(ind_avail_pts[,c("Collar_ID")]))
  mulies18_avail_df <- as.data.frame(mulies18_avail_spdf)
  # write.csv(mulies18_avail_df, file = "./Input/mulies18_avail_df.csv")
  # writeOGR(mulies18_avail_spdf, dsn = "./Input", layer = "mulies18_avail_spdf", driver = "ESRI Shapefile", overwrite = TRUE)
  
  #  Count total number of available points in each study area
  n_rnd_pts <- mulies18_avail_df %>%
    tally() %>%
    as.data.frame() %>%
    print()
  
  
  ####  Extract covariate data per point  
  #  ===================================
  #  Read in shapefiles & extract covariate data for used & available points
  
  #  Import study area shapefiles, reproject, and plot to double check everything
  OK <- st_read("Shapefiles/StudyAreaPolygons/METHOW_SA.shp") %>%
    st_transform("+init=epsg:2855")
  WA <- st_read("Shapefiles/Washington_State_Boundary/Washington_State_Boundary.shp") %>%
    st_transform("+init=epsg:2855")
  
  #  Double check that it reprojected correctly
  st_crs(NE)
  
  #  Visualize
  ggplot() +
    geom_sf(data = OK) +
    geom_sf(data = mulies18_used_sf, aes(color = Collar_ID))
  
  
  #  1. Distance to nearest road (continuous)
  #  ========================================
  #  Read in shapefile (these are big so they take awhile)
  #  Currently reading in shapefile that was cropped to 2018 cougar locations bbox
  # roads_SA <- st_read("Shapefiles/Roads/roads_SA.shp", crs = "+init=epsg:2855")
  roads_OK <- st_read("Shapefiles/Roads/roads_OK.shp", crs = "+init=epsg:2855") 
 
  #  Visualize
  # ggplot() +
  #   geom_sf(data = roads_OK) +
  #   geom_sf(data = mulies18_used_sf, aes(color = Collar_ID))
  
  #  For all cougar locations (used & available)
  #  Measure distance btwn each point and every road in study area
  #  THIS TAKES FOREVER
  road_dist_used_mulies <- st_distance(y = mulies18_used_sf, x = roads_OK)
  road_dist_avail_mulies <- st_distance(y = mulies18_avail_sf, x = roads_OK)
  
  #save(road_dist_avail_mulies, file = "./input/road_dist_avail_mulies.RData")
  
  #  Find closest road (minimum distance) to each point & added to dataframe
  mulies18_used_sf$road_dist <- apply(road_dist_used_mulies, 2, min)
  mulies18_avail_sf$road_dist <- apply(road_dist_avail_mulies, 2, min)
  #  ERROR: Error: cannot allocate vector of size 19.0 Gb
  #  it's too big!!! What do I do?!?!?!?!
  
  
  ####  HOW DO I ADD THE NE STUFF W/O MAKING NEW COLUMN OR OVERWRITING CURRENT COLUMN?

  
  mulies18_used_rd_dist <- as.data.frame(mulies18_used_sf)
  write.csv(mulies18_used_rd_dist, "./Input/mulies18_used_rd_dist.csv")
  mulies18_avail_rd_dist <- as.data.frame(mulies18_avail_sf)
  write.csv(mulies18_avail_rd_dist, "./Input/mulies18_avail_rd_dist.csv")
  
  
  #  1.5  Road density (continuous)
  #  ==============================
  #  Road density = total length of roads (in km) per 1 sq. km
  #  Resolution: 999 x 999 m
  #  Data extracted with Raster_Covariate_Extractions.R script
  mulies_used_rddnsty <- read.csv("./Input/mulies_used_rddnsty.csv")
  mulies_avail_rddnsty <- read.csv("./Input/mulies_avail_rddnsty.csv")
  #  Convert measuremetn units to square kilometers
  mulies_used_rddnsty$rd_dnsty_km <- mulies_used_rddnsty$rd_dnsty/1000
  mulies_avail_rddnsty$rd_dnsty_km <- mulies_avail_rddnsty$rd_dnsty/1000
  head(mulies_used_rddnsty)
  
  
  #  2. Distance to water (continuous)
  #  ================================
  hydro_OK <- st_read("Shapefiles/Hydro/hydro_OK.shp", crs = "+init=epsg:2855") 
  hydro_Ch <- st_read("Shapefiles/Hydro/hydro_Ch.shp", crs = "+init=epsg:2855") 
  hydro_St <- st_read("Shapefiles/Hydro/hydro_St.shp", crs = "+init=epsg:2855")
  hydro_PO <- st_read("Shapefiles/Hydro/hydro_PO.shp", crs = "+init=epsg:2855")
  hydro_Sp <- st_read("Shapefiles/Hydro/hydro_Sp.shp", crs = "+init=epsg:2855")
  
  #  Visualize
  # ggplot() +
  #   geom_sf(data = hydro_OK) +
  #   geom_sf(data = hydro_Ch) +
  #   geom_sf(data = mulie18_used_sf, aes(color = Collar_ID))
  
  #  Measure distance btwn each point and every stream in study area...
  #  this takes AWHILE
  hydro_OK_dist_mulies <- st_distance(y = mulies18_used_sf, x = hydro_OK)
  hydro_Ch_dist_mulies <- st_distance(y = mulies18_used_sf, x = hydro_Ch)
  hydro_OK_dist_rnd <- st_distance(y = mulies18_avail_sf, x = hydro_OK) 
  hydro_Ch_dist_rnd <- st_distance(y = mulies18_avail_sf, x = hydro_Ch) 
  
  #  Find closest stream (minimum distance) to each point & added to dataframe
  mulies18_used_sf$hydro_OK_dist <- apply(hydro_OK_dist_mulies, 2, min)
  mulies18_used_sf$hydro_Ch_dist <- apply(hydro_Ch_dist_mulies, 2, min)  
  mulies18_avail_sf$hydro_OK_dist <- apply(hydro_OK_dist_rnd, 2, min)
  mulies18_avail_sf$hydro_Ch_dist <- apply(hydro_Ch_dist_rnd, 2, min)
  
  #  2.5. Water density
  #  =================================
  
  
  
  #  3. Land cover type (categrorical)
  #  =================================
  #  Currently based on 30x30 m cell where point fell
  #  Data extracted with Raster_Covariate_Extractions.R script
  mulies_used_landcov <- read.csv("input/mulies_used_landcov.csv")
  mulies_avail_landcov <- read.csv("input/mulies_avail_landcov.csv")
  
  #  Bin similar land cover types to reduce number of categories
  sort(unique(mulies_used_landcov$NLCD))
  sort(unique(mulies_avail_landcov$NLCD))
  
  mulies_used_landcov <- mutate(mulies_used_landcov,
                                # merge developed habitats into one
                                NLCD_adj = ifelse(NLCD == "21", "22", NLCD),     
                                NLCD_adj = ifelse(NLCD == "23", "22", NLCD_adj),
                                NLCD_adj = ifelse(NLCD == "24", "22", NLCD_adj),
                                # merge forested habitats into one
                                NLCD_adj = ifelse(NLCD == "41", "43", NLCD_adj), 
                                NLCD_adj = ifelse(NLCD == "42", "43", NLCD_adj),
                                # merge agricultural habitats into one
                                NLCD_adj = ifelse(NLCD == "81", "82", NLCD_adj),
                                NLCD_adj = as.factor(NLCD_adj)
  ) %>%
    dplyr::select(-NLCD)
  
  mulies_avail_landcov <- mutate(mulies_avail_landcov,
                                 # merge developed habitats into one
                                 NLCD_adj = ifelse(NLCD == "21", "22", NLCD),     
                                 NLCD_adj = ifelse(NLCD == "23", "22", NLCD_adj),
                                 NLCD_adj = ifelse(NLCD == "24", "22", NLCD_adj),
                                 # merge forested habitats into one
                                 NLCD_adj = ifelse(NLCD == "41", "43", NLCD_adj), 
                                 NLCD_adj = ifelse(NLCD == "42", "43", NLCD_adj),
                                 # merge agricultural habitats into one
                                 NLCD_adj = ifelse(NLCD == "81", "82", NLCD_adj),
                                 NLCD_adj = as.factor(NLCD_adj)
  ) %>%
    dplyr::select(-NLCD)
  
  
  #  4. Elevation (& elevation^2?) & Ruggedness (continuous)
  #  =======================================================
  #  Currently based on 30x30 m cell where point fell
  #  Data extracted with Raster_Covariate_Extractions.R script
  mulies_used_dem <- read.csv("input/mulies_used_dem.csv")
  mulies_avail_dem <- read.csv("input/mulies_avail_dem.csv")
  
  mulies_used_tri <- read.csv("input/mulies_used_TRI.csv")
  mulies_avail_tri <- read.csv("input/mulies_used_TRI.csv")
  
  
  
  
  #  OTHERS?
  #  -NDVI (continuous)
  #  -burn severity and/or perimeter (categorical)
  #  -percent forest cover (continuous)  
  
  
  ####  Input data for model  ####
  #  =============================
  
  #  Combine covariate data into a single df
  #  Used 
  mulies_used_covs <- mulies_summer18[,c(2:10)] %>%
    cbind(mulies_used_landcov$NLCD) %>%
    cbind(mulies_used_dem$elev) %>%
    cbind(mulies_used_tri$TRI) %>%
    cbind(mulies_used_rddnsty$rd_dnsty_km) #%>%
  cbind(mulies_used_sf$road_dist) %>%
    cbind(mulies18_used_sf$hydro_OK_dist) %>%
    cbind(mulies18_used_sf$hydro_Ch_dist)
  colnames(mulies18_used_covs) <- c("Region", "Animal_ID", "Collar_ID", "TimeStamp", 
                                  "DateTime", "location_long", "location_lat", 
                                  "Longitude", "Latitude", "NLCD", "Elev", "TRI", 
                                  "Rd_Density_km", "Nearest_Rd", "Nearest_H20_OK", 
                                  "Nearest_H20_Ch") 
  
  head(mulies18_used_covs)
  
  #  Available
  coug_18_avail_covs <- coug18_avail_df %>%
    cbind(cougar_avail_landcov$NLCD) %>%
    cbind(cougar_avail_dem$elev) %>%
    cbind(cougar_avail_tri$TRI) %>%
    cbind(cougar_avail_rddnsty$rd_dnsty_km) 
  cbind(coug18_avail_sf$road_dist) %>%
    cbind(coug18_avail_sf$hydro_OK_dist) %>%
    cbind(coug18_avail_sf$hydro_Ch_dist)
  colnames(coug_18_avail_covs) <- c("Animal_ID", "Region","Longitude", "Latitude", 
                                    "NLCD", "Elev", "TRI", "Rd_Density_km", 
                                    "Nearest_Rd", "Nearest_H20_OK", "Nearest_H20_Ch") 
  
  head(coug_18_avail_covs)
  
  
  write.csv(coug18_used_covs, "Input/coug18_used_covs.csv")
  write.csv(coug_18_avail_covs, "Input/coug_18_avail_covs.csv")
  
  
  #  Combine covariate data for available locations
  
  # #  Generate used-available response variable
  # #  "used" = 1, "available" = 0
  # used <- rep(1, n_studyarea_pts[1,2])
  # avail <- rep(0, n_rnd_pts[1,2])
  # 
  # 
  # #  NEED TO UPDATE THIS FOR MULTIPLE ANIMALS
  # #  Matrix for regression
  # dat <- as.data.frame(rep("MVC202F", n_cougar_pts[n_cougar_pts$Animal_ID == "MVC202F", 2]))
  # dat$used <- used
  # dat$road_dist <- cougar1_sf$road_dist
  # colnames(dat) <- c("Animal_ID", "used", "road_dist")
  # 
  # #  NEED TO UPDATE THIS FOR MULTIPLE ANIMALS
  # available <- as.data.frame(rep("MVC202F", 10000))
  # available$used <- avail
  # available$road_dist <- rndpts_sf$FAKE_road_dist
  # colnames(available) <- c("Animal_ID", "used", "road_dist")
  # 
  # dat <- rbind(dat, available)
  # 
  # #  Standardize covariates
  # standard_dat <- scale(dat$road_dist, center = TRUE, scale = TRUE)
  # 
  # dat$road_dist_z <- standard_dat
  
  #write.csv(dat, file = "./input/dat.csv")
  #save(dat, file = "input/dat.RData")
  #  My .RData files aren't working- might have to do with administrative 
  #  privilage to update R on this computer... need to talk to IT
  
  
  
  
  
  
  # #  No longer need but pains me to delete it completely
  # ####  Home range estimation for ONE animal
  # #  =======================================
  # #  Estimate home range for each individual animal so "available" points can be
  # #  randomly selected within each home range--- 3rd order selection
  # 
  # #  Extract data from 1 animal and create SpatialPoints dataframe
  # cougar1 <- cougars_summer18 %>%
  #   dplyr::filter(Animal_ID == "MVC202F") %>%  
  #   dplyr::select(Longitude, Latitude) %>%
  #   SpatialPoints(., proj4string = CRS("+init=epsg:2855"))
  # 
  # #  Create bivariate normal utilization distribution based on individual animal's home range
  # UD <- kernelUD(cougar1, kern = "bivnorm")  #Cougar1locs_sp
  # #  Create polygon of 95% UD
  # UD95 <- getverticeshr(UD, 95)
  # #  Calculate area of 95% UD in square km
  # kernel.area(UD, 95, unin = "m", unout ="km")
  # 
  # ####  Selecting "available" points for ONE animal  
  # #  ==============================================
  # #  We do not want the number of available points dictating the outcome of 
  # #  the results (need to have a good measure of availability to compare 
  # #  use to). So starting with 10,000 random points per home range and thin
  # #  if needed.
  # 
  # #  Randomly select SpatialPoints within an individual's home range
  # set.seed(1234)
  # rndpts <- spsample(UD95, 10000, type = "random") # assuming spsample() is w/ replacement?!?!?
  # rndpts_sp <- SpatialPoints(rndpts, CRS("+init=epsg:2855"))
  # rndpts_sf <- st_as_sf(rndpts, coords = 1:2, crs = "+init=epsg:2855")
  # 
  # #  Save for extracting with other scripts
  # require(rgdal)
  # rndpts_spdf <- SpatialPointsDataFrame(rndpts_sp, data.frame(ID = 1:10000))
  # #writeOGR(rndpts_spdf, dsn = "./input", layer = "rndpts_spdf", driver = "ESRI Shapefile")
  #
  # #  Turn SpatialPoints df into a sf df
  # cougar1_sf <- st_as_sf(cougar1, coords = 1:2, crs = "+init=epsg:2855")
  # cougar_pts_sf <- st_as_sf(cougars_summer18, coords = 8:9, crs = "+init=epsg:2855")
  #
  # #  Measure distance btwn each point ("used" and "available") and every road in 
  # #  study area... this takes AWHILE
  # road_dist_coug <- st_distance(y = cougar1_sf, x = roads_OK)
  # road_dist_rnd <- st_distance(y = rndpts_sf, x = roads_OK) 
  # 
  # #  Find closest road (minimum distance) to each point & added to dataframe
  # cougar1_sf$road_dist <- apply(road_dist_coug, 2, min)
  # rndpts_sf$road_dist <- apply(road_dist_rnd, 2, min)
  # 
  # # give myself something to practice with for RSF b/c road_dist_rnd takes FOREVER
  # require(truncnorm) # truncates normal distribution by lower (a) & upper (b) bounds
  # road_dist_rnd_FAKE <- rtruncnorm(n = 10000, a = 0, b = 2000, mean = 350, sd = 150)
  # rndpts_sf$FAKE_road_dist <- road_dist_rnd_FAKE
