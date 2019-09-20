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
  
  
  #  Read in location data
  #  Used cougar locations
  cougar <- read.csv("./Input/cougars_summer18.csv")
  cougar_spdf <- SpatialPointsDataFrame(cougar[,10:11], cougar, proj4string = CRS("+init=epsg:2855"))
  #  Randomly selected "available points"
  available <- read.csv("./Input/coug18_avail_df.csv")
  available_spdf <- SpatialPointsDataFrame(available[,4:5], available, proj4string = CRS("+init=epsg:2855"))
  
  
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
  
  #  Double check projections match
  projection(landcov)
  projection(cougar_nlcd_proj)
  projection(available_nlcd_proj)
  
  # plot(cougar_nlcd_proj)
  # plot(landcov, add = TRUE)
  # plot(cougar_nlcd_proj, add = TRUE, pch = 19, col = "red")

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
  
  #  Save
  cougar_avail_landcov <- avail_locs
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_landcov <- used_locs[,c(2:11,14)]
  write.csv(cougar_used_landcov, "input/cougar_used_landcov.csv")
  write.csv(cougar_avail_landcov, "input/cougar_avail_landcov.csv")
  
  
  
  #  Elevation
  #  =====================
  #  Read in DEM (30 x 30 m resolution when it's not in longlat)
  DEM <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/WA_DEM/wa_dem1.img")
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
  
  # plot(cougar_dem_proj)
  # plot(DEM, add = TRUE)
  # plot(cougar_dem_proj, add = TRUE, pch = 19, col = "red")
  
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

  #  Save
  cougar_avail_dem <- avail_locs[,c(2:8)]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_dem <- used_locs[,c(2:11,14)]
  write.csv(cougar_used_dem, "input/cougar_used_dem.csv")
  write.csv(cougar_avail_dem, "input/cougar_avail_dem.csv")
  
  
  #  Terrain Ruggedness Index (TRI)
  #  Generate TRI from DEM
  #TRI <- terrain(DEM, opt = "TRI", filename = "TRI.img")
  TRI <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/Terrain_Ruggedness/TRI.img")
  
  #  Create new df based on dem projection
  used_locs <- as.data.frame(cougar_dem_proj) 
  avail_locs <- as.data.frame(available_dem_proj)
  
  #  Extract ruggedness values at geographic locations
  used_locs$TRI <- extract(x = TRI, y = used_locs[,12:13])
  avail_locs$TRI <- extract(x = TRI, y = avail_locs[,6:7])
  
  #  Check output
  used_locs$TRI[1:5]
  head(used_locs)
  
  avail_locs$TRI[1:5]
  head(avail_locs)
  
  #  Save
  cougar_avail_TRI <- avail_locs[,c(2:8)]
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_TRI <- used_locs[,c(2:11,14)]
  write.csv(cougar_used_TRI, "input/cougar_used_TRI.csv")
  write.csv(cougar_avail_TRI, "input/cougar_avail_TRI.csv")
  
  # require(spatialEco)
  # tri <- spatialEco::tri(DEM)
  
  
  
  #  Road Density
  #  =============================
  #  Road density = total length of roads (in km) per 1 sq. km
  #  Read in road density raster (999 x 999 m resolution) - created by L. Satterfield
  rd_dnsty <- raster("G:/My Drive/1_Repositories/MultiSpp_Cameras/Shapefiles/Roads/roaddensity/road.density_km2_IMG.img")
  res(rd_dnsty)
  projection(rd_dnsty)  # already matches cougar data
  
  #  Make camera staions spdf non-spatial
  cougar_rddnsty_spdf <- as.data.frame(cougar_spdf)
  available_rddnsty_spdf <- as.data.frame(available_spdf)
  
  # plot(cougar_rddnsty_spdf)
  # plot(DEM, add = TRUE)
  # plot(cougar_rddnsty_spdf, add = TRUE, pch = 19, col = "red")
  
  #  Convert spdf into a df with DEM projection
  used_locs <- as.data.frame(cougar_rddnsty_spdf) 
  class(used_locs)
  str(used_locs)
  
  avail_locs <- as.data.frame(available_rddnsty_spdf)
  class(avail_locs)
  str(avail_locs)
  
  #  Extract elevation at geographic locations
  used_locs$rd_dnsty <- extract(x = rd_dnsty, y = used_locs[,10:11])
  avail_locs$rd_dnsty <- extract(x = rd_dnsty, y = avail_locs[,4:5])
  
  ####  DON'T FORGET TO CHANGE UNITS TO SQ KM!!!!  Divide by 1000  ####
  
  #  Check output
  used_locs$rd_dnsty[1:5]
  head(used_locs)
  #  Exclude 3rd reprojected coordiantes in used data
  cougar_used_rddnsty <- used_locs[,c(2:11,14)]
  
  avail_locs$rd_dnsty[1:5]
  head(avail_locs)
  cougar_avail_rddnsty <- avail_locs[,c(2:8)]
  
  #  Save
  write.csv(cougar_used_rddnsty, "input/cougar_used_rddnsty.csv")
  write.csv(cougar_avail_rddnsty, "input/cougar_avail_rddnsty.csv")
  
  
  
  #  NDVI
  #  Nead to convert individual 16-day files from HDF4 format to GEOtiff
  #  No idea what projection these will come in
  #  Code from (https://stackoverflow.com/questions/36772341/reading-hdf-files-into-r-and-converting-them-to-geotiff-rasters)
  
  # library(gdalUtils)
  # # setwd("G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1")
  # gdalinfo("G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018145.h09v04.006.2018162001604.hdf")
  # sds <- get_subdatasets("G:/My Drive/1 Dissertation/Analyses/Shapefiles/MODIS_NDVI_MOD13Q1/MOD13Q1.A2018145.h09v04.006.2018162001604.hdf")
  # sds
  # gdal_translate(sds[1], dst_dataset = "MOD13Q1_A04.tif")
  # rast <- raster("MOD13Q1_A04.tif")
  # plot(rast)
  # MODres <- res(rast)
  # #### First get everything converted to .tif
  # #### Second make a raster stack of all the .tif files
  # #### Just reproject location data to match .tif files
  # #### Extract from there
  # #### Worry about reporjecting NDVI layers later
  # 
  # # rddnsty_prj <- projection(rd_dnsty)
  # # tst <- projectRaster(rast, res = MODres, crs = rddnsty_prj)
  # # plot(tst)
  # #  Have I told you how much I hate NDVI?
  # 
  # #  For lots of files
  # files <- dir(pattern = ".hdf")
  # filename <- substr(files,11,14)
  # filename <- paste0("NPP", filename, ".tif")
  # filename
  # i <- 1
  # for (i in 1:15){
  #   sds <- get_subdatasets(files[i])
  #   gdal_translate(sds[1], dst_dataset = filename[i])
  # }
  # 