  ##  Estiamting density of linear features 
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2018
  ##  ========================================
  ##  Generate density estimates of linear features such as streams or roads 
  ##  Takes SpatialLines and converst to Rasters (units in meters/sq meter)
  ##  using kernel density methods
  ##  Original script provided by Lauren Satterfield and
  ##  https://gis.stackexchange.com/questions/138861/calculating-road-density-in-r-using-kernel-density
  
  #  Load libraries
  library(sp)
  library(raster)
  library(maptools)
  library(spatstat)  # not available with newest version of R! :(
  
  #  Read in shapefiles and transform to study area projection
  h20_OK <- readOGR("./Hydro", "hydro_OK")
  h20_Ch <- readOGR("./Hydro", "hydro_Ch")
  h20_OK <- spTransform(h20_OK, CRS("+init=epsg:2855"))
  h20_Ch <- spTransform(h20_Ch, CRS("+init=epsg:2855"))
  crs(h20_OK)
  class(h20_OK)  #  Confirm that you have a spatial lines dataframe
  
  # Convert SpatialLines to psp object using maptools library
  pspSl_OK <- as.psp(h20_OK)
  pspSl_Ch <- as.psp(h20_Ch)
  str(pspSl_OK)
  
  #  Pixellate with resolution of 1000 m (1 km x 1 km pixels)
  px_OK <- pixellate(pspSl_OK, eps = 1000)
  px_Ch <- pixellate(pspSl_Ch, eps = 1000)
  
  # This can be converted to raster as desired
  rLength_OK <- raster(px_OK)
  plot(rLength_OK)
  h20.out_OK <- rLength_OK
  plot(h20.out_OK)
  h20.out_OK@crs <- CRS("+init=epsg:2855")
  crs(h20.out_OK)
  plot(h20.out_OK)
  class(h20.out_OK)
  
  rLength_Ch <- raster(px_Ch)
  plot(rLength_Ch)
  h20.out_Ch <- rLength_Ch
  plot(h20.out_Ch)
  h20.out_Ch@crs <- CRS("+init=epsg:2855")
  crs(h20.out_Ch)
  plot(h20.out_Ch)
  class(h20.out_Ch)
  
  #  Write rasters so you don't have to rerun this every time
  #  Rasters are specific to waterways in each county
  writeRaster(h20.out_OK, "water_density_OK_km2", format = "HFA")
  writeRaster(h20.out_Ch, "water_density_Ch_km2", format = "HFA")
  # h20.out_OK <- raster("C:/Users/sb89/Spatial Analyses/Shapefiles/water_density_OK_km2.img")
  # h20.out_Ch <- raster("C:/Users/sb89/Spatial Analyses/Shapefiles/water_density_Ch_km2.img")
  
  #  Resample Chelan raster to fit within the Okanogan raster
  resampled_Ch <- resample(h20.out_Ch, h20.out_OK, "bilinear")
  
  #  Double check their origns now match up (essential for mosaic function)
  origin(merged_h20)
  origin(h20.out_OK)
  
  #  Merge county-specific rasters into single water density raster for OK
  merged_water <- mosaic(h20.out_OK, resampled_Ch, fun = sum)
  #  Save!
  writeRaster(merged_water_OK, "merged_water_OK", format = "HFA")
  
  