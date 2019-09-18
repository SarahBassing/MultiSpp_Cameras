  ##  RSF Data Wrangling
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ===========================
  ##  Script to organize & filter GPS collar data (cougars), generate individual
  ##  home ranges with kernel utilization distributions (KUD), ranomly sample
  ##  locations from within those home ranges, and extract covaraite data for the 
  ##  "used" & "available locations. Final output is a dataframe of the response 
  ##  variable (0/1) and explanatory covariates for each individual animal.
  
  ##  NOTE: Only considering collar data collected Summer 2018 (Jun 1 - Aug 31)
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
  library(ggplot2)
  library(anytime)
  library(stringr)

  #  Read in data and format
  cougars <- read.csv("Data/Cougar_AllPoints.csv", header = TRUE, stringsAsFactors = FALSE) %>%
    transmute(
      No = as.integer(No),
      Region = as.character(str_sub(ID, 1, 2)), # extracts 1st 2 charcters of ID
      Animal_ID = as.character(ID),
      Collar_ID = as.character(Collar),
      TimeStamp = as.character(LMT_DateTime),
      DateTime = anytime(TimeStamp),        # formats date & time to POSIXct
      location_long = as.numeric(Long),
      location_lat = as.numeric(Lat)
    )
  # DateTime <- anytime(cougars$TimeStamp)
  # str_sub() extracts study area initials where 1,2 = start & end of string to extract
  
  head(cougars)
  
  
  ####  Filter and prep data for spatial work  
  #  ========================================
  
  #  I only want collar data from 06/01/2018 - 08/31/2018 for now
  cougars_summer18 <- cougars[cougars$DateTime > "2018-05-31 23:59:59" & cougars$DateTime < "2018-09-01 00:00:00", ]
  
  #  Double check that I've filtered the data correctly
  min(cougars_summer18$DateTime); max(cougars_summer18$DateTime)
  
  #  Make GPS data into a sf spatial dataframe, reproject, & extract UTMs
  cougar_pts <- cougars_summer18[,c("location_long", "location_lat")] %>%
    st_as_sf(., coords = 1:2, crs = "+proj=longlat +ellps=WGS84") %>%
    st_transform("+init=epsg:2855") %>%
    st_coordinates(.)  # no longer spatial
  
   #  Add UTMs to cougars dataframe
  cougars_summer18$Longitude <- cougar_pts[,1]
  cougars_summer18$Latitude <- cougar_pts[,2] 
  
  #  Count number of points per individual animal
  n_cougar_pts <- cougars_summer18 %>%
    group_by(Animal_ID) %>%
    tally() %>%
    as.data.frame() %>%
    ungroup() %>%
    print()
  
  #  Toss location data from animals with <20 locations in summer 2018
  #  i.e., MVC226F & NEC108M
  cougars_summer18 <- cougars_summer18[cougars_summer18$Animal_ID != "MVC226F" & cougars_summer18$Animal_ID != "NEC108M",]
  
  #  Review and save (csv, sf, spdf)
  head(cougars_summer18)
  coug18_used_sf <- st_as_sf(cougars_summer18, coords = 9:10, crs = "+init=epsg:2855")
  proj <- CRS("+init=epsg:2855")
  coug18_used_spdf <- SpatialPointsDataFrame(coordinates(cougars_summer18[,c("Longitude", "Latitude")]),
                                              proj4string = proj,
                                              data = as.data.frame(cougars_summer18[,1:8]))
  # write.csv(cougars_summer18, file = "./input/cougars_summer18.csv")
  # writeOGR(coug18_used_spdf, dsn = "./input", layer = "coug18_used_spdf", driver = "ESRI Shapefile")
  
  
  
  
  
  ####  Selecting "available" points  
  #  ===============================
  #  Map home ranges for each individual (using kernel utilization distributions)
  #  Randomly sample available points from within each home range
  #  Sampling 10,000 points per home range to have a sufficiently large sample
  #  size to compare used points with (can thin if needed)
  
  #  Dataframe of unique animal IDs
  unq_id <- as.data.frame(cougars_summer18$Animal_ID) %>%
    distinct() 
  colnames(unq_id) <- "Animal_ID"
  
  #  How many unique animals are there?
  n_animals <- nrow(unq_id)
  
  
  #  Function to create SpatialPoints for each indvidual, estimate KUD, and
  #  randomly sample points within KUD. Output is a list of locations by 
  #  individual to use as "available" points for RSF.
  #  ================================================
  
  #  First, split df into list of dataframes by unique animal ID
  ind_animal <- group_split(cougars_summer18, cougars_summer18$Animal_ID)
  
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
  

  
  #  Mega df to hold all AVAILABLE points corresponding to unique Animal_ID
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
  colnames(ind_avail_pts) <- c("Animal_ID", "Longitude", "Latitude")
  #  Add study area to dataframe
  ind_avail_pts <- mutate(ind_avail_pts, 
                          Region = as.character(str_sub(Animal_ID, 1, 2))
                          )
 
  #  Save for extracting with other scripts (csv, sf, spdf)
  coug18_avail_sf <- st_as_sf(ind_avail_pts, coords = 2:3, crs = "+init=epsg:2855")
  require(rgdal)
  proj <- CRS("+init=epsg:2855")
  coug18_avail_spdf <- SpatialPointsDataFrame(coordinates(ind_avail_pts[,c("Longitude", "Latitude")]),
                                                         proj4string = proj,
                                                         data = as.data.frame(ind_avail_pts[,c("Animal_ID", "Region")]))
  coug18_avail_df <- as.data.frame(coug18_avail_spdf)
  # write.csv(coug18_avail_df, file = "./Input/coug18_avail_df.csv")
  # writeOGR(coug18_avail_spdf, dsn = "./Input", layer = "coug18_avail_spdf", driver = "ESRI Shapefile")
  
  
  
  
  ####  Creating bounding boxes to manipulate spatial data with  
  #  ==========================================================
  
  #  What's the spatial extent of these locations?
  min(cougars_summer18$Longitude); max(cougars_summer18$Longitude) # x
  min(cougars_summer18$Latitude); max(cougars_summer18$Latitude)   # y
  
  #  Different bounding boxes to expand this extent (for croping shapefiles)
  
  #  Full extent including both study areas and then some:
  #  min and max values based on extent of both study areas and cougar locations
  #  that went outside the study areas plus a 2000 m buffer beyond that extent
  #  to capture all covaraite data available within the study
  full_bbox <- st_bbox(c(xmin = 502659, xmax = 783979.9, ymin = 100808.3, ymax = 222849.8))
  # 5000 m buffer: xmin = 499659, xmax = 786979.9, ymin = 97808.3, ymax = 225849.8
  
  #  OK cougar extent based on summer 2018 collar data
  min(cougars_summer18$Longitude[cougars_summer18$Region == "MV"]); max(cougars_summer18$Longitude[cougars_summer18$Region == "MV"]) # x
  min(cougars_summer18$Latitude[cougars_summer18$Region == "MV"]); max(cougars_summer18$Latitude[cougars_summer18$Region == "MV"])   # y
  #  Cougar specific bbox of OK study area plus 2000 m buffer
  OK_coug_bbox <- st_bbox(c(xmin = 527199.2, xmax = 590176.2, ymin = 119645, ymax = 222849.8))
  
  #  NE cougar extent based on summer 2018 collar data
  min(cougars_summer18$Longitude[cougars_summer18$Region == "NE"]); max(cougars_summer18$Longitude[cougars_summer18$Region == "NE"]) # x
  min(cougars_summer18$Latitude[cougars_summer18$Region == "NE"]); max(cougars_summer18$Latitude[cougars_summer18$Region == "NE"])   # y
  #  Cougar specific bbox of NE study area plus 2000 m buffer
  NE_coug_bbox <- st_bbox(c(xmin = 679337.3, xmax = 783979.9, ymin = 109497.6, ymax = 195370.5))
  
  
  
  ####  Extract covariate data per point  
  #  ===================================
  #  Read in shapefiles & extract covariate data for used & available points
  
  #  Import study area shapefiles, reproject, and plot to double check everything
  OK <- st_read("Shapefiles/StudyAreaPolygons/METHOW_SA.shp") %>%
    st_transform("+init=epsg:2855")
  NE <- st_read("Shapefiles/StudyAreaPolygons/NE_SA.shp") %>%
    st_transform("+init=epsg:2855")
  WA <- st_read("Shapefiles/Washington_State_Boundary/Washington_State_Boundary.shp") %>%
    st_transform("+init=epsg:2855")
  
  #  Double check that it reprojected correctly
  st_crs(NE)
  
  #  Visualize
  ggplot() +
    geom_sf(data = OK) +
    geom_sf(data = NE) +
    geom_sf(data = coug18_used_sf, aes(color = Animal_ID))
  
  
  #  Remove NE cougars for now
  OKcoug18_used_sf <- coug18_used_sf %>%
    filter(Region == "MV")
  OKcoug18_avail_sf <- coug18_avail_sf %>%
    filter(Region == "MV")
  
  ggplot() +
    geom_sf(data = OK) +
    geom_sf(data = OKcoug18_used_sf, aes(col = Animal_ID))
  ggplot() +
    geom_sf(data = OK) +
    geom_sf(data = OKcoug18_avail_sf, aes(col = Animal_ID))
  
  
  #  1. Distance to road (continuous)
  #  ================================
  #  Read in shapefile (these are big so they take awhile)
  # roads_SA <- st_read("Shapefiles/Roads/roads_SA.shp", crs = "+init=epsg:2855")
  roads_OK <- st_read("Shapefiles/Roads/roads_OKcoug.shp", crs = "+init=epsg:2855") 
  # roads_NE <- st_read("Shapefiles/Roads/roads_NEcoug.shp", crs = "+init=epsg:2855")
  
  #  Visualize
  ggplot() +
    geom_sf(data = roads_OK) +
    geom_sf(data = OKcoug18_used_sf, aes(color = Animal_ID))
  
  #  For all cougar locations (used)
  #  Measure distance btwn each point and every road in study area
  #  THIS TAKES FOREVER
  road_dist_used_OKcoug <- st_distance(y = OKcoug18_used_sf, x = roads_OK)
  road_dist_avail_OKcoug <- st_distance(y = OKcoug18_avail_sf, x = roads_OK)
  # road_dist_used_NEcoug <- st_distance(y = coug18_used_sf, x = roads_NE)
  # road_dist_avail_NEcoug <- st_distance(y = coug18_avail_sf, x = roads_NE)
  
  save(road_dist_avail_OKcoug, file = "./input/road_dist_avail_OKcoug.RData")
  
  #  Find closest road (minimum distance) to each point & added to dataframe
  coug18_used_sf$road_dist <- apply(road_dist_used_OKcoug, 2, min)
  coug18_avail_sf$road_dist <- apply(road_dist_avail_OKcoug, 2, min)
  #  ERROR: Error: cannot allocate vector of size 19.0 Gb
  #  it's too big!!! What do I do?!?!?!?!

  
    ####  HOW DO I ADD THE NE STUFF W/O MAKING NEW COLUMN OR OVERWRITING CURRENT COLUMN?
  # coug18_used_sf$road_dist <- apply(road_dist_used_NEcoug, 2, min)
  # coug18_avail_sf$road_dist <- apply(road_dist_avail_NEcoug, 2, min)
  
  coug18_used <- as.data.frame(coug18_used_sf)
  write.csv(coug18_used, "./Input/coug18_used.csv")

  
  
  #  2. distance to water (continuous)
  #  ================================
  hydro_OK <- st_read("Shapefiles/Hydro/hydro_OK.shp", crs = "+init=epsg:2855") 
  hydro_Ch <- st_read("Shapefiles/Hydro/hydro_Ch.shp", crs = "+init=epsg:2855") 
  hydro_St <- st_read("Shapefiles/Hydro/hydro_St.shp", crs = "+init=epsg:2855")
  hydro_PO <- st_read("Shapefiles/Hydro/hydro_PO.shp", crs = "+init=epsg:2855")
  hydro_Sp <- st_read("Shapefiles/Hydro/hydro_Sp.shp", crs = "+init=epsg:2855")
  
  #  Visualize
  ggplot() +
    geom_sf(data = hydro_OK) +
    geom_sf(data = hydro_Ch) +
    geom_sf(data = coug18_used_sf, aes(color = Animal_ID))
  
  #  Measure distance btwn each point and every stream in study area...
  #  this takes AWHILE
  hydro_OK_dist_coug <- st_distance(y = cougar1_sf, x = hydro_OK)
  hydro_Ch_dist_coug <- st_distance(y = cougar1_sf, x = hydro_Ch)
  hydro_OK_dist_rnd <- st_distance(y = rndpts_sf, x = hydro_OK) 
  hydro_Ch_dist_rnd <- st_distance(y = rndpts_sf, x = hydro_Ch) 
  
  #  Find closest stream (minimum distance) to each point & added to dataframe
  cougar1_sf$hydro_OK_dist <- apply(hydro_OK_dist_coug, 2, min)
  cougar1_sf$hydro_Ch_dist <- apply(hydro_Ch_dist_coug, 2, min)  
  rndpts_sf$hydro_OK_dist <- apply(hydro_OK_dist_rnd, 2, min)
  rndpts_sf$hydro_Ch_dist <- apply(hydro_Ch_dist_rnd, 2, min)
  
  #  Find shortest distance btwn each point and streams in different counties 
  #  and only save that one to df (Only need one measurement per point)
  min_dist_coug <- transform(cougar1_sf, min_dist = pmin(hydro_OK_dist, hydro_Ch_dist))
  min_dist_rnd <- transform(rndpts_sf, min_dist = pmin(hydro_OK_dist, hydro_Ch_dist))
  #dont know if this works yet
  
  # give myself something to practice with for RSF b/c hydro_dist_rnd takes FOREVER
  require(truncnorm) # truncates normal distribution by lower (a) & upper (b) bounds
  road_dist_rnd_FAKE <- rtruncnorm(n = 10000, a = 0, b = 2000, mean = 350, sd = 150)
  rndpts_sf$FAKE_road_dist <- road_dist_rnd_FAKE
  
  
  #  3. Land cover type (categrorical)
  #  =================================
  #  Currently based on 30x30 m cell where point fell
  #  Data extracted with LandCover_Covaraite_Extraction.R script
  cougar_used_landcov <- read.csv("input/cougar_used_landcov.csv")
  cougar_avail_landcov <- read.csv("input/cougar_avail_landcov.csv")
  
  #  Bin similar land cover types to reduce number of categories
  
  
  #  -elevation (& elevation 2?) & ruggedness (continuous)
  #  -NDVI (continuous)
  #  -burn severity and/or perimeter (categorical)
  #  -percent forest cover (continuous)  
  
  
  ####  Input data for model  ####
  #  =============================
  #  "used" = 1, "available" = 0
  used <- rep(1, n_cougar_pts[n_cougar_pts$Animal_ID == "MVC202F", 2])
  avail <- rep(0, 10000)
  
  #  Matrix for regression
  dat <- as.data.frame(rep("MVC202F", n_cougar_pts[n_cougar_pts$Animal_ID == "MVC202F", 2]))
  dat$used <- used
  dat$road_dist <- cougar1_sf$road_dist
  colnames(dat) <- c("Animal_ID", "used", "road_dist")
  
  
  available <- as.data.frame(rep("MVC202F", 10000))
  available$used <- avail
  available$road_dist <- rndpts_sf$FAKE_road_dist
  colnames(available) <- c("Animal_ID", "used", "road_dist")
  
  dat <- rbind(dat, available)
  
  #  Standardize covariates
  standard_dat <- scale(dat$road_dist, center = TRUE, scale = TRUE)
  
  dat$road_dist_z <- standard_dat
  
  #write.csv(dat, file = "./input/dat.csv")
  #save(dat, file = "input/dat.RData")
  #  My .RData files aren't working- might have to do with administrative 
  #  privilage to update R on this stupid computer... need to talk to IT
  

  
  
  
  
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
  