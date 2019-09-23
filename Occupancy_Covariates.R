  ##  Occupancy Covariates
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  =================================
  ##  Extracting location (site-level) covariates for occupancy models. 
  
  #  Load packages
  library(tidyverse)    
  library(sf)
  library(sp)
  library(raster)
  library(rgeos)
  library(ggplot2)
  
  #  Read in data
  #  ==================================
  #  Camera station data
  cam_stations <- read.csv("./Data/OK_cam_stations.csv")
  survey_covs <- read.csv("./Data/OK_Camera_Survey_Covariates_18-19.csv")
  
  #  Shapefiles
  OK <- st_read("Shapefiles/StudyAreaPolygons/METHOW_SA.shp") %>%
    st_transform("+init=epsg:2855")
  roads_OK <- st_read("Shapefiles/Roads/roads_OK.shp", crs = "+init=epsg:2855") 
  hydro_OK <- st_read("Shapefiles/Hydro/hydro_OK.shp", crs = "+init=epsg:2855") 
  hydro_Ch <- st_read("Shapefiles/Hydro/hydro_Ch.shp", crs = "+init=epsg:2855") 
  
  #  Rasters
  rd_dnsty <- raster("Shapefiles/Roads/roaddensity/road.density_km2_IMG.img")
  water_dnsty <- raster("Shapefiles/Hydro/merged_waterdensity_OK.img")
  landcov <- raster("Shapefiles/National_Land_Cover_Database/land_use_land_cover_NLCD_wa/land_use_land_cover/nlcd_wa_utm10.tif") # 2011 land cover
  DEM <- raster("Shapefiles/WA_DEM/wa_dem1.img")
  TRI <- raster("Shapefiles/Terrain_Ruggedness/TRI.img")
  
  #  Make camera stations spatial
  stations_sf <- st_as_sf(cam_stations, coords = 4:5, crs = "+proj=longlat +ellps=WGS84") %>%
    st_transform("+init=epsg:2855")
  stations_spdf <- SpatialPointsDataFrame(coordinates(cam_stations[, c("UTM_X", "UTM_Y")]),
                                          proj4string = CRS("+proj=longlat +ellps=WGS84"),
                                          data = as.data.frame(cam_stations[,2:3]))
  stations_spdf <- spTransform(stations_spdf, crs("+init=epsg:2855"))
    
  #  Double check projections
  st_crs(stations_sf)
  proj4string(stations_spdf)
  
  
  
  #  1. Distance to nearest road
  #  =============================
  #  Measure distance btwn each point and open roads in the OK
  road_dist_cams <- st_distance(y = stations_sf, x = roads_OK)
  #  Find closest road (minimum distance) to each point & added to dataframe
  stations_sf$road_dist <- apply(road_dist_cams, 2, min)
  
  head(stations_sf)
  
  # ggplot() +
  #   geom_sf(data = OK) +
  #   geom_sf(data = roads_OK) +
  #   geom_sf(data = stations_sf, aes(color = "Cell_ID"))
  
  #  1.5  Road Density
  #  =============================
  #  Resolution: 999 x 999 m
  #  Density of roads is in meters of road per 1000 x 1000 m cell
  #  Need to divide by 1000 to put in km2
  
  #  Make camera staions spdf non-spatial
  stations_rddnsty_df <- as.data.frame(stations_spdf)
  
  #  Extract land cover type at location of each camera station
  stations_rddnsty_df$rd_dnsty_m <- raster::extract(x = rd_dnsty, y = stations_rddnsty_df[,3:4])
  #  Convert measuremetn units to square kilometers
  stations_rddnsty_df$rd_dnsty_km <- stations_rddnsty_df$rd_dnsty_m/1000
  head(stations_rddnsty_df)
  
  # plot(stations_spdf)
  # plot(rd_dnsty_km, add = TRUE)
  # plot(stations_spdf, add = TRUE, pch = 19, col = "red")
  
  #  2. Distance to nearest stream
  #  =============================
  #  Measure distance btwn each point and steams in the OK study area
  hydro_OK_cams <- st_distance(y = stations_sf, x = hydro_OK)
  hydro_Ch_cams <- st_distance(y = stations_sf, x = hydro_Ch)
  #  Find closest stream (minimum distance) to each point & added to dataframe
  stations_sf$hydro_OK_dist <- apply(hydro_OK_cams, 2, min)
  stations_sf$hydro_Ch_dist <- apply(hydro_Ch_cams, 2, min)
  
  #  NEXT STEP!!! For each camera station I only need one value between hydro_OK_cams
  #  and hydro_Ch_cams (same for collar data) but I'm not sure how to look at 2
  #  different columns and retain only the smaller value for the final covariate
  
  head(stations_sf)
  
  # ggplot() +
  #   geom_sf(data = OK) +
  #   geom_sf(data = hydro_OK) +
  #   geom_sf(data = hydro_Ch) +
  #   geom_sf(data = stations_sf, aes(color = "Cell_ID"))
  
  
  #  2.5.  Water Density
  #  ============================
  #  Resolution 997.4 m x 999 m
  #  Density of water is in meters of stream per 1000 x 1000 m cell
  #  Need to divide by 1000 to put in km2
  
  #  Make camera staions spdf non-spatial
  stations_waterdnsty_df <- as.data.frame(stations_spdf)
  
  #  Extract water densigy at location of each camera station
  stations_waterdnsty_df$water_dnsty_m <- raster::extract(x = water_dnsty, y = stations_waterdnsty_df[,3:4])
  #  Convert measurement units to square kilometers
  stations_waterdnsty_df$water_dnsty_km <- stations_waterdnsty_df$water_dnsty_m/1000
  head(stations_waterdnsty_df)
  
  # plot(stations_spdf)
  # plot(water_dnsty, add = TRUE)
  # plot(stations_spdf, add = TRUE, pch = 19, col = "red")
  
  
  #  3. Land cover (2011 National Land Cover Database)
  #  =============================
  #  Resolution: 30 x 30 m
  
  #  Reproject camera locations to match NLCD
  nlcd_proj <- projection(landcov)
  stations_lc_spdf <- spTransform(stations_spdf, crs(nlcd_proj))
  
  #  Turn spdf into df
  stations_lc_df <- as.data.frame(stations_lc_spdf)
  head(stations_lc_df)
  #  Extract land cover type at location of each camera station
  stations_lc_df$NLCD <- extract(x = landcov, y = stations_lc_df[,3:4])
  head(stations_lc_df)
  
  #  Bin similar habitat types
  sort(unique(stations_lc_df$NLCD))
  stations_lc_df <- mutate(stations_lc_df,
                          # merge developed habitats into one
                          NLCD_adj = ifelse(NLCD == "21", "22", NLCD),     
                          NLCD_adj = ifelse(NLCD == "23", "22", NLCD_adj),
                          NLCD_adj = as.factor(NLCD_adj)
                          ) %>%
    dplyr::select(-NLCD)
  
  # plot(stations_lc_spdf)
  # plot(landcov, add = TRUE)
  # plot(stations_lc_spdf, add = TRUE, pch = 19, col = "red")
  
  #  4. Elevation
  #  =========================
  #  Resolution: 30 x 30 m
  
  #  Reproject camera locations to match DEM
  demproj <- projection(DEM)
  stations_dem_spdf <- spTransform(stations_spdf, crs(demproj))
  
  #  Turn spdf into df
  stations_dem_df <- as.data.frame(stations_dem_spdf)
  head(stations_dem_df)
  #  Extract elevation at location of each camera station
  stations_dem_df$elev <- extract(x = DEM, y = stations_dem_df[,3:4])
  head(stations_dem_df)

  # plot(stations_dem_spdf)
  # plot(DEM, add = TRUE)
  # plot(stations_dem_spdf, add = TRUE, pch = 19, col = "red")
  
  #  5. Terrain Ruggedness Index 
  #  =============================
  #  Using TRI generated from DEM (30 x 30 m resolution)
  triproj <- projection(TRI)
  stations_tri_spdf <- spTransform(stations_spdf, crs(triproj))

  #  Turn spdf into df
  stations_tri_df <- as.data.frame(stations_tri_spdf)
  head(stations_tri_df)
  #  Extract elevation at location of each camera station
  stations_tri_df$TRI <- extract(x = TRI, y = stations_tri_df[,3:4])
  head(stations_tri_df)
  
  
  
  #  5. NDVI???
  
  
  #  6. Burn severity???
  
  #  7. Survey specific covariates
  #  Filter out camera stations that don't match current date range
  srvy_covs <- survey_covs %>%
    filter(Notes != "burned") %>% 
    filter(Notes != "currently held hostage") %>%
    filter(Cell_ID != "OK1474" & Cell_ID != "OK2051")
  
  
  #  Merge into a single df: site-level covariates for occupancy models
  stations_df <- as.data.frame(stations_sf)
  
  #  Double check the site-level and survey-level dfs are going to line up
  dblcheck <- full_join(stations_df, srvy_covs, by = c("Cell_ID", "Camera_ID"))
  View(dblcheck)
  
  cam_covs <- stations_lc_df[,c(1:2, 5)] %>%
    cbind(stations_dem_df$elev) %>%
    cbind(stations_tri_df$TRI) %>%
    cbind(stations_rddnsty_df$rd_dnsty_km) %>%
    cbind(stations_df$road_dist) %>%
    cbind(stations_waterdnsty_df$water_dnsty_km) %>%
    cbind(stations_df$hydro_OK_dist) %>%
    cbind(stations_df$hydro_Ch_dist) %>%
    full_join(srvy_covs[,3:29], by = c("Cell_ID", "Camera_ID"))
  colnames(cam_covs) <- c("Cell_ID", "Camera_ID", "NLCD", "Elev", "Ruggedness",
                          "Rd_Density", "Nearest_Rd", 
                          "Water_Density", "Nearest_H20_OK", "Nearest_H20_Ch",
                          "Study_Area", "Camera_Lat", "Camera_Long", 
                          "Dist_focal_pt", "Monitoring", "Canopy_Cover", 
                          "Land_mgnt", "Habitat_type", "Habitat_type_reduced", 
                          "Height_o1", "Height_o2", "Heigh_o3", "Height_o4",
                           "Height_o5", "Height_o6", "Height_o7", "Height_o8", 
                          "Height_o9", "Height_o10", "Height_o11", "Height_o12", 
                          "Height_o13", "Height_o14", "Height_o15", "Height_o16")
  
  head(cam_covs)
  
  write.csv(cam_covs, "Input/cam_covs.csv")
  
  