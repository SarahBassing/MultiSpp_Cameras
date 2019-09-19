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
  
  #  Shapefiles
  OK <- st_read("Shapefiles/StudyAreaPolygons/METHOW_SA.shp") %>%
    st_transform("+init=epsg:2855")
  roads_OK <- st_read("Shapefiles/Roads/roads_OK.shp", crs = "+init=epsg:2855") 
  hydro_OK <- st_read("Shapefiles/Hydro/hydro_OK.shp", crs = "+init=epsg:2855") 
  hydro_Ch <- st_read("Shapefiles/Hydro/hydro_Ch.shp", crs = "+init=epsg:2855") 
  
  #  Rasters
  rd_dnsty <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/Roads/roaddensity/road.density_km2_IMG.img")
  landcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/USDA shapefiles/National_Land_Cover_Database/land_use_land_cover_NLCD_wa/land_use_land_cover/nlcd_wa_utm10.tif") # 2011 land cover
  DEM <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WA DEM rasters/wa_dem1.img") # 2011 land cover
  
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
  
  # ggplot() +
  #   geom_sf(data = OK) +
  #   geom_sf(data = roads_OK) +
  #   geom_sf(data = stations_sf, aes(color = "Cell_ID"))
  
  #  1.5  Road Density
  #  =============================
  #  Resolution: 999 x 999 m
  
  #  Make camera staions spdf non-spatial
  stations_rddnsty_df <- as.data.frame(stations_spdf)
  
  #  Extract land cover type at location of each camera station
  stations_rddnsty_df$rd_dnsty <- raster::extract(x = rd_dnsty, y = stations_rddnsty_df[,3:4])
  head(stations_rddnsty_df)
  
  # plot(stations_spdf)
  # plot(rd_dnsty, add = TRUE)
  # plot(stations_spdf, add = TRUE, pch = 19, col = "red")
  
  #  2. Distance to nearest stream
  #  =============================
  #  Measure distance btwn each point and steams in the OK study area
  hydro_OK_cams <- st_distance(y = stations_sf, x = hydro_OK)
  hydro_Ch_cams <- st_distance(y = stations_sf, x = hydro_Ch)
  #  Find closest stream (minimum distance) to each point & added to dataframe
  stations_sf$hydro_OK_dist <- apply(hydro_OK_cams, 2, min)
  stations_sf$hydro_Ch_dist <- apply(hydro_Ch_cams, 2, min)
  
  # ggplot() +
  #   geom_sf(data = OK) +
  #   geom_sf(data = hydro_OK) +
  #   geom_sf(data = hydro_Ch) +
  #   geom_sf(data = stations_sf, aes(color = "Cell_ID"))
  
  #  3. Land cover (National Land Cover Database)
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
  
  #  5. NDVI???
  
  
  #  6. Burn severity???
  
  
  #  Take all covariates and merge into a single df