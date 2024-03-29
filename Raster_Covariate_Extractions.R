  ##  Extract covariate data from rasters
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept 2019
  ##  ==============================================================
  ##  Script to extract covariate data from various raster files at 
  ##  specific geographic locations. Specifically, land cover data from
  ##  the National Land Cover Databse, elevation & terrain ruggedness
  ##  from a WA digital elevation model (DEM), and road density (km/km^2 
  ##  of roads).
  ##  Originally created by Staci Amburgey & modified by Olivia Sanderfoot
  ##  ==============================================================
  
  #   Load packages
  
  library(raster)
  library(sp)
  library(rgdal)
  library(ggplot2)
  
  
  #  Read in location data for each species
  #  ======================================
  #  Cougars
  #  Used locations
  cougar <- read.csv("./Input/cougars_summer18.csv")
  cougar_spdf <- SpatialPointsDataFrame(cougar[,10:11], cougar, proj4string = CRS("+init=epsg:2855"))
  #  Randomly selected "available points"
  available <- read.csv("./Input/coug18_avail_df.csv")
  available_spdf <- SpatialPointsDataFrame(available[,4:5], available, proj4string = CRS("+init=epsg:2855"))
  
  #  Mule deer
  #  Used locations
  mulies <- read.csv("./Input/mulies_summer18.csv")
  mulies_spdf <- SpatialPointsDataFrame(mulies[,9:10], mulies, proj4string = CRS("+init=epsg:2855"))
  #  Randomly selected "available points"
  available_mulies <- read.csv("./Input/mulies18_avail_df.csv")
  available_mulies_spdf <- SpatialPointsDataFrame(available_mulies[,3:4], 
                                                  available_mulies, proj4string = CRS("+init=epsg:2855"))
  
  
  #  National Land Cover Database 
  #  =============================================================
  #  Read in and transform data so they match with NLCD projection
  #  Import NLCD data (2011 but also have 2016...)
  landcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/USDA shapefiles/National_Land_Cover_Database/land_use_land_cover_NLCD_wa/land_use_land_cover/nlcd_wa_utm10.tif") # 2011 land cover
  #landcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")  # this one looks very course... is it really 30x30 m resolution?
  #  Check resolution & projection
  res(landcov)
  landproj <- projection(landcov)
  
  #  Reproject used & available locations to match landcov 
  #  Way faster than trying to reproject landcov
  cougar_nlcd_proj <- spTransform(cougar_spdf, crs(landproj))
  available_nlcd_proj <- spTransform(available_spdf, crs(landproj))
  
  mulies_nlcd_proj <- spTransform(mulies_spdf, crs(landproj))
  mulies_avail_nlcd_proj <- spTransform(available_mulies_spdf, crs(landproj))
  
  #  Double check projections match
  projection(landcov)
  projection(cougar_nlcd_proj)
  projection(available_nlcd_proj)
  
  # plot(cougar_nlcd_proj)
  # plot(landcov, add = TRUE)
  # plot(cougar_nlcd_proj, add = TRUE, pch = 19, col = "red")
  
  
  #  Extract covaraite data at used & available locations
  #  ====================================================
  #  Cougars
  #  Convert SpatialPointsDataFrames into a regular old dataframe
  used_locs_cg <- as.data.frame(cougar_nlcd_proj) 
  class(used_locs_cg)
  str(used_locs_cg)
  
  avail_locs_cg <- as.data.frame(available_nlcd_proj)
  class(avail_locs_cg)
  str(avail_locs_cg)
  
  #  Extract NLCD values at geographic locations
  used_locs_cg$NLCD <- extract(x = landcov, y = used_locs_cg[,12:13])
  avail_locs_cg$NLCD <- extract(x = landcov, y = avail_locs_cg[,6:7])
  
  #  Check output
  used_locs_cg$NLCD[1:5]
  head(used_locs_cg)
  
  avail_locs_cg$NLCD[1:5]
  head(avail_locs_cg)
  
  #  Save
  cougar_avail_landcov <- avail_locs_cg
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_landcov <- used_locs_cg[,c(2:11,14)]
  write.csv(cougar_used_landcov, "input/cougar_used_landcov.csv")
  write.csv(cougar_avail_landcov, "input/cougar_avail_landcov.csv")
  
  
  #  Mule Deer
  #  Convert SpatialPointsDataFrames into a regular old dataframe
  used_locs_md <- as.data.frame(mulies_nlcd_proj) 
  class(used_locs_md)
  str(used_locs_md)
  
  avail_locs_md <- as.data.frame(mulies_avail_nlcd_proj)
  class(avail_locs_md)
  str(avail_locs_md)
  
  #  Extract NLCD values at geographic locations
  used_locs_md$NLCD <- extract(x = landcov, y = used_locs_md[,11:12])
  avail_locs_md$NLCD <- extract(x = landcov, y = avail_locs_md[,5:6])
  
  #  Check output
  used_locs_md$NLCD[1:5]
  head(used_locs_md)
  
  avail_locs_md$NLCD[1:5]
  head(avail_locs_md)
  
  #  Save
  mulies_avail_landcov <- avail_locs_md
  #  Exclude 3rd reprojected coordiantes in used data
  mulies_used_landcov <- used_locs_md[,c(2:10,13)]
  write.csv(mulies_used_landcov, "input/mulies_used_landcov.csv")
  write.csv(mulies_avail_landcov, "input/mulies_avail_landcov.csv")
  
  
  
  #  Elevation
  #  =====================
  #  Read in DEM (30 x 30 m resolution when it's not in longlat)
  DEM <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/WA_DEM/wa_dem1.img")
  #  Check resolution & projection
  res(DEM)
  demproj <- projection(DEM)
  
  #  Reproject location data to match DEM projection
  #  Cougar locations
  cougar_dem_proj <- spTransform(cougar_spdf, crs(demproj))
  available_dem_proj <- spTransform(available_spdf, crs(demproj))
  
  #  Mule deer locations
  mulies_dem_proj <- spTransform(mulies_spdf, crs(demproj))
  mulies_avail_dem_proj <- spTransform(available_mulies_spdf, crs(demproj))
  
  #  Double check projections match
  projection(DEM)
  projection(mulies_dem_proj)
  projection(mulies_avail_dem_proj)
  
  # plot(cougar_dem_proj)
  # plot(DEM, add = TRUE)
  # plot(cougar_dem_proj, add = TRUE, pch = 19, col = "red")
  
  #  Extract covaraite data at used & available locations
  #  ====================================================
  #  Keep in mind these values are for a 30 x 30 m pixel of which the geographic
  #  location falls within
  
  #  Cougars
  #  Convert spdf into a df with DEM projection
  used_locs_cg <- as.data.frame(cougar_dem_proj) 
  class(used_locs_cg)
  str(used_locs_cg)
  
  avail_locs_cg <- as.data.frame(available_dem_proj)
  class(avail_locs_cg)
  str(avail_locs_cg)
  
  #  Extract elevation at geographic locations
  used_locs_cg$elev <- extract(x = DEM, y = used_locs_cg[,12:13])
  avail_locs_cg$elev <- extract(x = DEM, y = avail_locs_cg[,6:7])
  
  #  Check output
  used_locs_cg$elev[1:5]
  head(used_locs_cg)
  
  avail_locs_cg$elev[1:5]
  head(avail_locs_cg)

  #  Save
  cougar_avail_dem <- avail_locs_cg[,c(2:8)]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_dem <- used_locs_cg[,c(2:11,14)]
  write.csv(cougar_used_dem, "input/cougar_used_dem.csv")
  write.csv(cougar_avail_dem, "input/cougar_avail_dem.csv")
  
  
  #  Mule deer
  #  Convert spdf into a df with DEM projection
  used_locs_md <- as.data.frame(mulies_dem_proj) 
  class(used_locs_md)
  str(used_locs_md)
  
  avail_locs_md <- as.data.frame(mulies_avail_dem_proj)
  class(avail_locs_md)
  str(avail_locs_md)
  
  #  Extract elevation at geographic locations
  used_locs_md$elev <- extract(x = DEM, y = used_locs_md[,11:12])
  avail_locs_md$elev <- extract(x = DEM, y = avail_locs_md[,5:6])
  
  #  Check output
  used_locs_md$elev[1:5]
  head(used_locs_md)
  
  avail_locs_md$elev[1:5]
  head(avail_locs_md)
  
  #  Save
  mulies_avail_dem <- avail_locs_md[,c(2:7)]
  #  Exclude 3rd reprojected coordiantes in used data
  mulies_used_dem <- used_locs_md[,c(2:10,13)]
  write.csv(mulies_used_dem, "input/mulies_used_dem.csv")
  write.csv(mulies_avail_dem, "input/mulies_avail_dem.csv")
  
  
  
  
  #  Terrain Ruggedness Index (TRI)
  #  Generate TRI from DEM
  #TRI <- terrain(DEM, opt = "TRI", filename = "TRI.img")
  TRI <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/Terrain_Ruggedness/TRI.img")
  
  #  Create new df based on dem projection
  #  Cougar
  used_locs_cg <- as.data.frame(cougar_dem_proj) 
  avail_locs_cg <- as.data.frame(available_dem_proj)
  
  #  Mule Deer
  used_locs_md <- as.data.frame(mulies_dem_proj)
  avail_locs_md <- as.data.frame(mulies_avail_dem_proj)
  
  #  Cougars
  #  Extract ruggedness values at geographic locations
  used_locs_cg$TRI <- extract(x = TRI, y = used_locs_cg[,12:13])
  avail_locs_cg$TRI <- extract(x = TRI, y = avail_locs_cg[,6:7])
  
  #  Check output
  used_locs_cg$TRI[1:5]
  head(used_locs_cg)
  
  avail_locs_cg$TRI[1:5]
  head(avail_locs_cg)
  
  #  Save
  cougar_avail_TRI <- avail_locs_cg[,c(2:8)]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_TRI <- used_locs_cg[,c(2:11,14)]
  write.csv(cougar_used_TRI, "input/cougar_used_TRI.csv")
  write.csv(cougar_avail_TRI, "input/cougar_avail_TRI.csv")
  
  #  Mule deer
  #  Extract ruggedness values at geographic locations
  used_locs_md$TRI <- extract(x = TRI, y = used_locs_md[,11:12])
  avail_locs_md$TRI <- extract(x = TRI, y = avail_locs_md[,5:6])
  
  #  Check output
  used_locs_md$TRI[1:5]
  head(used_locs_md)
  
  avail_locs_md$TRI[1:5]
  head(avail_locs_md)
  
  #  Save
  mulies_avail_TRI <- avail_locs_md[,c(2:7)]
  #  Exclude 3rd reprojected coordiantes in used data
  mulies_used_TRI <- used_locs_md[,c(2:10,13)]
  write.csv(mulies_used_TRI, "input/mulies_used_TRI.csv")
  write.csv(mulies_avail_TRI, "input/mulies_avail_TRI.csv")
  
  
  
  #  Road Density
  #  =============================
  #  Road density = total length of roads (in km) per 1 sq. km
  #  Read in road density raster (999 x 999 m resolution) - created by L. Satterfield
  rd_dnsty <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/Roads/roaddensity/road.density_km2_IMG.img")
  res(rd_dnsty)
  projection(rd_dnsty)  # already matches cougar data
  
  #  Make locations spdf non-spatial
  #  Cougars
  cougar_rddnsty_df <- as.data.frame(cougar_spdf)
  available_rddnsty_df <- as.data.frame(available_spdf)
  #  Mule deer
  mulies_rddnsty_df <- as.data.frame(mulies_spdf)
  mulies_avail_rddnsty_df <- as.data.frame(available_mulies_spdf)
  
  # plot(cougar_spdf)
  # plot(rd_dnsty, add = TRUE)
  # plot(cougar_spdf, add = TRUE, pch = 19, col = "red")
  
  #  Cougars
  #  Extract road density of pixel where location falls
  cougar_rddnsty_df$rd_dnsty_m <- extract(x = rd_dnsty, y = cougar_rddnsty_df[,10:11])
  available_rddnsty_df$rd_dnsty_m <- extract(x = rd_dnsty, y = available_rddnsty_df[,4:5])
  #  Convert measurement units to square kilometers
  cougar_rddnsty_df$rd_dnsty_km <- cougar_rddnsty_df$rd_dnsty_m/1000
  head(cougar_rddnsty_df)
  available_rddnsty_df$rd_dnsty_km <- available_rddnsty_df$rd_dnsty_m/1000
  head(available_rddnsty_df)
  
  #  Check output
  cougar_rddnsty_df$rd_dnsty_km[1:5]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_rddnsty <- cougar_rddnsty_df[,c(2:11,15)]
  head(cougar_used_rddnsty)
  
  available_rddnsty_df$rd_dnsty_km[1:5]
  cougar_avail_rddnsty <- available_rddnsty_df[,c(2:5,9)]
  head(cougar_avail_rddnsty)
  
  #  Save
  write.csv(cougar_used_rddnsty, "input/cougar_used_rddnsty.csv")
  write.csv(cougar_avail_rddnsty, "input/cougar_avail_rddnsty.csv")
  
  
  #  Mule Deer
  #  Extract road density of pixel where location falls
  mulies_rddnsty_df$rd_dnsty_m <- extract(x = rd_dnsty, y = mulies_rddnsty_df[,9:10])
  mulies_avail_rddnsty_df$rd_dnsty_m <- extract(x = rd_dnsty, y = mulies_avail_rddnsty_df[,3:4])
  #  Convert measurement units to square kilometers
  mulies_rddnsty_df$rd_dnsty_km <- mulies_rddnsty_df$rd_dnsty_m/1000
  head(mulies_rddnsty_df)
  mulies_avail_rddnsty_df$rd_dnsty_km <- mulies_avail_rddnsty_df$rd_dnsty_m/1000
  head(mulies_avail_rddnsty_df)
  
  #  Check output
  #  Exclude 3rd reprojected coordiantes in used data
  mulies_used_rddnsty <- mulies_rddnsty_df[,c(2:10,14)]
  head(mulies_used_rddnsty)
  
  mulies_avail_rddnsty <- mulies_avail_rddnsty_df[,c(2:4,8)]
  head(mulies_avail_rddnsty)
  
  #  Save
  write.csv(mulies_used_rddnsty, "input/mulies_used_rddnsty.csv")
  write.csv(mulies_avail_rddnsty, "input/mulies_avail_rddnsty.csv")
  
  
  
  #  Denisty of streams (OK only)
  #  ============================
  #  Density of roads is in meters of road per 1000 x 1000 m cell
  #  Resolution 997.4 m x 999 m
  #  Need to divide by 1000 to put in km2
  #  Read in water density raster & check projection
  water_dnsty <- raster("Shapefiles/Hydro/merged_waterdensity_OK.img")
  res(water_dnsty)
  projection(water_dnsty) # same as cougar locations
  
  #  Make used and available cougar location spdfs non-spatial
  #  Cougars
  cougar_waterdnsty_df <- as.data.frame(cougar_spdf)
  available_waterdnsty_df <- as.data.frame(available_spdf)
  
  #  Mule Deer
  mulies_waterdnsty_df <- as.data.frame(mulies_spdf)
  mulies_avail_waterdnsty_df <- as.data.frame(available_mulies_spdf)
  
  # plot(cougar_spdf)
  # plot(water_dnsty, add = TRUE)
  # plot(cougar_spdf, add = TRUE, pch = 19, col = "blue")
  
  #  Cougars
  #  Extract water density at location of each point
  cougar_waterdnsty_df$water_dnsty_m <- raster::extract(x = water_dnsty, y = cougar_waterdnsty_df[,10:11])
  available_waterdnsty_df$water_dnsty_m <- raster::extract(x = water_dnsty, y = available_waterdnsty_df[,4:5])
  #  Convert measurement units to square kilometers
  cougar_waterdnsty_df$water_dnsty_km <- cougar_waterdnsty_df$water_dnsty_m/1000
  head(cougar_waterdnsty_df)
  available_waterdnsty_df$water_dnsty_km <- available_waterdnsty_df$water_dnsty_m/1000
  head(available_waterdnsty_df)
  
  #  Check output
  cougar_waterdnsty_df$water_dnsty_km[1:5]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_h20dnsty <- cougar_waterdnsty_df[,c(2:11,15)]
  head(cougar_used_h20dnsty)
  
  available_waterdnsty_df$water_dnsty_km[1:5]
  cougar_avail_h20dnsty <- available_waterdnsty_df[,c(2:5,9)]
  head(cougar_avail_h20dnsty)  
  
  #  Save
  write.csv(cougar_used_h20dnsty, "input/cougar_used_h20dnsty.csv")
  write.csv(cougar_avail_h20dnsty, "input/cougar_avail_h20dnsty.csv")
  
  #  Mule Deer
  #  Extract water density at location of each point
  mulies_waterdnsty_df$water_dnsty_m <- raster::extract(x = water_dnsty, y = mulies_waterdnsty_df[,9:10])
  mulies_avail_waterdnsty_df$water_dnsty_m <- raster::extract(x = water_dnsty, y = mulies_avail_waterdnsty_df[,3:4])
  #  Convert measurement units to square kilometers
  mulies_waterdnsty_df$water_dnsty_km <- mulies_waterdnsty_df$water_dnsty_m/1000
  head(mulies_waterdnsty_df)
  mulies_avail_waterdnsty_df$water_dnsty_km <- mulies_avail_waterdnsty_df$water_dnsty_m/1000
  head(mulies_avail_waterdnsty_df)
  
  #  Check output
  #  Exclude 3rd reprojected coordiantes in used data
  mulies_used_h20dnsty <- mulies_waterdnsty_df[,c(2:10,14)]
  head(mulies_used_h20dnsty)
  
  mulies_avail_h20dnsty <- mulies_avail_waterdnsty_df[,c(2:4,8)]
  head(mulies_avail_h20dnsty)  
  
  #  Save
  write.csv(mulies_used_h20dnsty, "input/mulies_used_h20dnsty.csv")
  write.csv(mulies_avail_h20dnsty, "input/mulies_avail_h20dnsty.csv")
  
  
  
  
  
  #  NDVI Amplitude (2018)
  #  Maximum increase in canopy photosynthetic activity above the baseline
  #  Difference between MAXN(Maximum level of photosynthetic activity in the canopy) 
  #  & SOSN (Level of photosynthetic activity at the beginning of measurable photosynthesis)
  #  Data: C6 Aqua Western U.S. 250 m eMODIS Remote Sensing Phenology Data 2018
  #  https://phenology.cr.usgs.gov/get_data_Aqua_C6_250w.php
  #  More about this product: https://www.usgs.gov/centers/eros/science/usgs-eros-archive-vegetation-monitoring-emodis-remote-sensing-phenology?qt-science_center_objects=0#qt-science_center_objects
  #  Citation: DOI https://doi.org//10.5066/F7PC30G1
  #  Projection seems to be WGS_1984_Lambert_Azimuthal_Equal_Area
  #  Proj4js.defs["SR-ORG:7963"] = "+proj=laea +lat_0=5 +lon_0=19 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  ndvi <- raster("Shapefiles/MODIS_Aqua_NDVI_Amplitude/C6_eMODIS_West_AMP2018/amp_2018w.ovr")
  res(ndvi)
  projection(ndvi)
  ndviproj <- CRS("+proj=laea +lat_0=5 +lon_0=19 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  crs(ndvi) <- ndviproj
  plot(ndvi)
  
  #  Reproject location data to match
  cougar_ndvi_proj <- spTransform(cougar_spdf, crs(ndviproj))
  available_ndvi_proj <- spTransform(available_spdf, crs(ndviproj))
  
  cougar_ndvi_df <- as.data.frame(cougar_ndvi_proj)
  available_ndvi_df <- as.data.frame(available_ndvi_proj)

  cougar_ndvi_df$ndvi <- extract(x = ndvi, y = cougar_ndvi_df[,10:11])
  available_ndvi_df <- extract(x = ndvi, y = available_ndvi_df[,4:5])
  
  
  
  
  
  
  
  #  Nead to convert individual 16-day files from HDF4 format to GEOtiff
  #  No idea what projection these will come in
  #  Code from (https://stackoverflow.com/questions/36772341/reading-hdf-files-into-r-and-converting-them-to-geotiff-rasters)
  
  library(gdalUtils)
  # setwd("G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1")
  #  What's inside one of these awful .hdf files?
  gdalinfo("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018145.h09v04.006.2018162001604.hdf")
  #  Save the layer names within a single .hdf file
  #  Not the pretties way to do this but couldn't get the for loop to work (see below)
  sds1 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018145.h09v04.006.2018162001604.hdf")
  sds2 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018145.h10v04.006.2018162000930.hdf")
  sds3 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018161.h09v04.006.2018177235716.hdf")
  sds4 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018161.h10v04.006.2018177235445.hdf")
  sds5 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018177.h09v04.006.2018197102832.hdf")
  sds6 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018177.h10v04.006.2018197102816.hdf")
  sds7 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018193.h09v04.006.2018210001831.hdf")
  sds8 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018193.h10v04.006.2018210001414.hdf")
  sds9 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018209.h09v04.006.2018227125018.hdf")
  sds10 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018209.h10v04.006.2018227125257.hdf")
  sds11 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018225.h09v04.006.2018241235848.hdf")
  sds12 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018225.h10v04.006.2018241235850.hdf")
  sds13 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018241.h09v04.006.2018258000752.hdf")
  sds14 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018241.h10v04.006.2018258000703.hdf")
  sds15 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018257.h09v04.006.2018282125825.hdf")
  sds16 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018257.h10v04.006.2018282130308.hdf")
  sds17 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018273.h09v04.006.2018295105653.hdf")
  sds18 <- get_subdatasets("Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018273.h10v04.006.2018295105706.hdf")
  
  #  Which layer are you interested in? NDVI is 1st layer
  sds14
  
  #  Generate new names for these files once they're reformatted
  files <- dir(path = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1", pattern = ".hdf")
  filename <- substr(files,29, 41)
  filename <- paste0("NDVI_", filename, ".tif")
  filename
  
  #  One at a time
  gdal_translate(sds1[1], dst_dataset = filename[1])
  rast1 <- raster(filename[1])
  gdal_translate(sds1[2], dst_dataset = filename[2])
  rast2 <- raster(filename[2])
  gdal_translate(sds1[3], dst_dataset = filename[3])
  rast3 <- raster(filename[3])
  gdal_translate(sds1[4], dst_dataset = filename[4])
  rast4 <- raster(filename[4])
  gdal_translate(sds1[5], dst_dataset = filename[5])
  rast5 <- raster(filename[5])
  gdal_translate(sds1[6], dst_dataset = filename[6])
  rast6 <- raster(filename[6])
  gdal_translate(sds1[7], dst_dataset = filename[7])
  rast7 <- raster(filename[7])
  gdal_translate(sds1[8], dst_dataset = filename[8])
  rast8 <- raster(filename[8])
  gdal_translate(sds1[9], dst_dataset = filename[9])
  rast9 <- raster(filename[9])
  gdal_translate(sds1[10], dst_dataset = filename[10])
  rast10 <- raster(filename[10])
  gdal_translate(sds1[11], dst_dataset = filename[11])
  rast11 <- raster(filename[11])
  gdal_translate(sds1[12], dst_dataset = filename[12])
  rast12 <- raster(filename[12])
  #  Stopped working here... WHY?!?!
  gdal_translate(sds1[13], dst_dataset = filename[13])
  rast13 <- raster(filename[13])
  gdal_translate(sds1[14], dst_dataset = filename[14])
  rast14 <- raster(filename[14])
  gdal_translate(sds1[15], dst_dataset = filename[15])
  rast15 <- raster(filename[15])
  gdal_translate(sds1[16], dst_dataset = filename[16])
  rast16 <- raster(filename[16])
  gdal_translate(sds1[17], dst_dataset = filename[17])
  rast17 <- raster(filename[17])
  gdal_translate(sds1[18], dst_dataset = filename[18])
  rast18 <- raster(filename[18])
  plot(rast12)
  MODres <- res(rast12)
  
  #  Try this for some more trouble shooting since only half the files seemed to work....
  #  https://gis.stackexchange.com/questions/199647/hdf-files-in-gdal-throwing-an-error-in-r-gdalinfo
  
  #  Make a raser stack of all NDVI rasters
  NDVI_stack <- stack(rast1, rast2, rast3, rast4, rast5, rast6, rast7, rast8, 
                      rast9, rast10, rast11, rast12)
  writeRaster(NDVI_stack, filename = "Shapefiles/MODIS_NDVI_MOD13Q1/NDVI_stack", format = "GTiff", overwrite = TRUE)
  #  Read this in with rasterstack()
  
  #  Average across all rasters in stack for each pixel to get mean value over time period
  mean_NDVI <- mean(NDVI_stack)
  writeRaster(mean_NDVI, filename = "Shapefiles/MODIS_NDVI_MOD13Q1/mean_NDVI_summer18", format = "HFA", overwrite = TRUE)
  mean_NDVI <- raster("Shapefiles/MODIS_NDVI_MOD13Q1/mean_NDVI_summer18.img")
  writeRaster(mean_NDVI, "Shapefiles/MODIS_NDVI_MOD13Q1/mean_NDVI.tif", format="GTiff", overwrite=TRUE)
  
  #  Reproject used & available locations to match weird NDVI projection
  NDVIproj <- proj4string(mean_NDVI)
  projeciton(NDVIproj)
  
  cougar_ndvi_proj <- spTransform(cougar_spdf, crs(NDVIproj))
  available_ndvi_proj <- spTransform(available_spdf, crs(NDVIproj))
  projection(cougar_ndvi_proj)
  
  #  Convert SpatialPointsDataFrames into a regular old dataframe
  used_locs <- as.data.frame(cougar_ndvi_proj) 
  class(used_locs)
  str(used_locs)
  
  avail_locs <- as.data.frame(available_ndvi_proj)
  class(avail_locs)
  str(avail_locs)
  
  #  Extract covaraite data at used & available locations
  #  ====================================================
  #  Extract NLCD values at geographic locations
  used_locs$NDVI <- extract(x = mean_NDVI, y = used_locs[,12:13])
  avail_locs$NDVI <- extract(x = mean_NDVI, y = avail_locs[,6:7])
  
  #  Check output
  used_locs$NDVI[1:5]
  head(used_locs)
  
  avail_locs$NDVI[1:5]
  head(avail_locs)
  
  #  Save
  cougar_avail_NDVI <- avail_locs
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_NDVI <- used_locs[,c(2:11,14)]
  write.csv(cougar_used_NDVI, "input/cougar_used_NDVI.csv")
  write.csv(cougar_avail_NDVI, "input/cougar_avail_NDVI.csv")
  
  
  
  
  #### Second make a raster stack of all the .tif files
  #### Just reproject location data to match .tif files
  #### Extract from there
  #### Worry about reporjecting NDVI layers later
  #  Have I told you how much I hate NDVI?
  
  
  gdal_translate(sds[1], dst_dataset = "NDVI.2018162001604.tif")
  rast <- raster("NDVI.2018162001604.tif")
  plot(rast)
  MODres <- res(rast)
  
  #  For lots of files
  files <- dir(path = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1", pattern = ".hdf")
  filename <- substr(files,29, 41)
  filename <- paste0("NDVI_", filename, ".tif")
  filename
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #  Loop over files to create .tif files
  #  I can't get this to work
  i <- 1
  for (i in 1:18){
    sds <- get_subdatasets(files[i])
    gdal_translate(sds[1], dst_dataset = filename[i])
  }
  
  #  An alternative way to create new file names
  #  Create list of MODIS files
  rlist = list.files(path = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1", pattern="hdf$", full.names=FALSE)
  
  #  Chop up file names and create new ones
  #  Function to save only last 9 characters in string (only keep unique numbers at end of file name)
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  #  Run function 
  filenames0 <- substrRight(rlist,9)
  #  Save first 5 characters in string (chop off ".hdf")
  filenamessuffix <- substr(filenames0,1,5)
  #  Create start of new file names
  listofnewnames <- rep("NDVI_", 18)
  # Combine pieces to create new names for converted files
  newnames <- vector()
  for (i in 1:length(listofnewnames)) {
    newnames[i] <- paste0(listofnewnames[i], filenamessuffix[i], ".tif")
  }
  
  # Loop over files to convert into rasters
  #  Still doensn't work
  ####  Error in split1[[1]] : subscript out of bounds  ####  WHAT THE HECK IS split1[[1]]???
  for (i in 1:length(rlist)) {
    sds <- get_subdatasets(rlist[i])
    gdal_translate(sds[1], dst_dataset = newnames[i])
  }
  