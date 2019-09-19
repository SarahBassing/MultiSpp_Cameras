  ##  Extract Land Type from the National Land Cover Database (NLCD)
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept 2019
  ##  ==============================================================
  ##  Script to extract landcover type from specific geographic locations
  ##  Originally created by Staci Amburgey & modified by Olivia Sanderfoot
  ##  ==============================================================
  
  #   Load packages
  
  library(raster)
  library(sp)
  library(rgdal)
  library(ggplot2)
  
  #  Read in and transform data so they match with NLCD projection
  #  =============================================================
  #  Import NLCD data (2011 but also have 2016...)
  landcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/USDA shapefiles/National_Land_Cover_Database/land_use_land_cover_NLCD_wa/land_use_land_cover/nlcd_wa_utm10.tif") # 2011 land cover
  #landcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")  # this one looks very course... is it really 30x30 m resolution?
  #  Check resolution & projection
  res(landcov)
  landproj <- projection(landcov)
  
  #  Read in location data
  #  Used cougar locations
  cougar <- read.csv("./Input/cougars_summer18.csv")
  cougar_spdf <- SpatialPointsDataFrame(cougar[,10:11], cougar, proj4string = CRS("+init=epsg:2855"))
  #  Reproject so it matches landcov (way faster than trying to reproject landcov)
  cougar_nlcd_proj <- spTransform(cougar_spdf, crs(landproj))
  
  #  Randomly selected "available points"
  available <- read.csv("./Input/coug18_avail_df.csv")
  available_spdf <- SpatialPointsDataFrame(available[,4:5], available, proj4string = CRS("+init=epsg:2855"))
  #  Reproject so it matches landcov
  available_nlcd_proj <- spTransform(available_spdf, crs(landproj))
  
  #  Double check projections match
  projection(landcov)
  projection(cougar_nlcd_proj)
  projection(available_nlcd_proj)
  
  plot(cougar_nlcd_proj)
  plot(landcov, add = TRUE)
  plot(cougar_nlcd_proj, add = TRUE, pch = 19, col = "red")

  #  Convert SpatialPointsDataFrames into a regular old dataframe
  used_locs <- as.data.frame(cougar_nlcd_proj) 
  class(used_locs)
  str(used_locs)
  
  avail_locs <- as.data.frame(available_nlcd_proj)
  class(avail_locs)
  str(avail_locs)
  
  
  #  Extract covaraite data at used & available locations
  #  ====================================================
  #  Extract NLCD values at geographic locations
  used_locs$NLCD <- extract(x = landcov, y = used_locs[,12:13])
  avail_locs$NLCD <- extract(x = landcov, y = avail_locs[,6:7])
  
  #  Check output
  used_locs$NLCD[1:5]
  head(used_locs)
  
  avail_locs$NLCD[1:5]
  head(avail_locs)
  cougar_avail_landcov <- avail_locs
  
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_landcov <- used_locs[,c(2:11,14)]
  
  #  Save
  write.csv(cougar_used_landcov, "input/cougar_used_landcov.csv")
  write.csv(cougar_avail_landcov, "input/cougar_avail_landcov.csv")
  
  
  
  #  Elevation
  #  =====================
  #  Read in DEM (30x30 m resolution)
  DEM <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WA DEM rasters/wa_dem1.img") # 2011 land cover
  #  Check resolution & projection
  res(DEM)
  demproj <- projection(DEM)
  
  #  Reproject location data to match DEM projection
  #  Used cougar locations
  cougar_dem_proj <- spTransform(cougar_spdf, crs(demproj))
  #  Randomly selected "available points"
  available_dem_proj <- spTransform(available_spdf, crs(demproj))
  
  #  Double check projections match
  projection(DEM)
  projection(cougar_dem_proj)
  projection(available_dem_proj)
  
  plot(cougar_dem_proj)
  plot(DEM, add = TRUE)
  plot(cougar_dem_proj, add = TRUE, pch = 19, col = "red")
  
  #  Extract covaraite data at used & available locations
  #  ====================================================
  #  Keep in mind these values are for a 30 x 30 m pixel of which the geographic
  #  location falls within
  
  #  Convert spdf into a df with DEM projection
  used_locs <- as.data.frame(cougar_dem_proj) 
  class(used_locs)
  str(used_locs)
  
  avail_locs <- as.data.frame(available_dem_proj)
  class(avail_locs)
  str(avail_locs)
  
  #  Extract elevation at geographic locations
  used_locs$elev <- extract(x = DEM, y = used_locs[,12:13])
  avail_locs$elev <- extract(x = DEM, y = avail_locs[,6:7])
  
  #  Check output
  used_locs$elev[1:5]
  head(used_locs)
  
  avail_locs$elev[1:5]
  head(avail_locs)
  cougar_avail_dem <- avail_locs
  
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_dem <- used_locs[,c(2:11,14)]
  
  
  #  Attempt to extract terrain ruggedness for these locations as well
  #  Terrain Ruggedness Index
  require(spatialEco)
  tri <- spatialEco::tri(DEM)  #  OH MY GOD THIS IS SLOW... maybe not for right now
