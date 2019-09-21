  ##  Covariate preparation
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ================================
  ##  Notes: 
  ##  Using a projection specific to northern WA which covers both study areas:
  ##  Proj4js.defs["EPSG:2855"] = "+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 
  ##  +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  ##  ================================

  #  Load packages
  library(tidyverse)    
  library(sf)
  library(sp)
  library(rgeos)
  
  #  Read in study area shapefiles & reproject to common projection ("+init=epsg:2855")
  OK <- st_read("Shapefiles/StudyAreaPolygons/METHOW_SA.shp") %>%
    st_transform("+init=epsg:2855")
  NE <- st_read("Shapefiles/StudyAreaPolygons/NE_SA.shp") %>%
    st_transform("+init=epsg:2855")
  WA <- st_read("Shapefiles/Washington_State_Boundary/Washington_State_Boundary.shp") %>%
    st_transform("+init=epsg:2855")
  
  #  Combine study area shapefiles to use for extracting spatial data
  WPPP_SA <- st_union(OK, NE)
  WPPP_fullstudyarea <- as(WPPP_SA, "Spatial")
  require(rgdal)
  projection(WPPP_fullstudyarea)
  writeOGR(WPPP_fullstudyarea, dsn = "Shapefiles/StudyAreaPolygons", 
           layer = "WPPP_fullstudyarea", driver = "ESRI Shapefile")
  #  What's the bounding box?
  st_bbox(WPPP_SA)
  
  
  #  Create new bounding boxes for manipulation spatial data
  #  =======================================================
  
  #  Full extent including both study areas and then some:
  #  min and max values based on extent of both study areas and cougar locations
  #  that went outside the study areas plus a 2000 m buffer beyond that extent
  #  to capture all covaraite data available within the study
  full_bbox <- st_bbox(c(xmin = 502659, xmax = 783979.9, ymin = 100808.3, ymax = 222849.8))
  # 5000 m buffer: xmin = 499659, xmax = 786979.9, ymin = 97808.3, ymax = 225849.8
  
  #  Study area-specific bboxes based on summer 2018 cougar data plus 2000 m buffer
  OK_coug_bbox <- st_bbox(c(xmin = 527199.2, xmax = 590176.2, ymin = 119645, ymax = 222849.8))
  NE_coug_bbox <- st_bbox(c(xmin = 679337.3, xmax = 783979.9, ymin = 109497.6, ymax = 195370.5))
  
  
  #  Read in covariate shapefiles & rasters, reproject, crop to appropriate  
  #  spatial extent, and save so I never have to do this again!
  #  ======================================================================
  
  ####  ROADS  ####
  #  Data come from WA DNR (http://data-wadnr.opendata.arcgis.com/datasets/wadnr-washington-active-roads)
  
  #  Read and transform- this will take awhile (big file!)
  roads <- st_read("Shapefiles/WADNR_Washington_Active_Roads/WADNR_Washington_Active_Roads.shp") %>%
    st_transform(crs = "+init=epsg:2855")
  
  #  Crop to appropriate bounding boxes
  roads_OK <- st_crop(roads, OK) 
  roads_NE <- st_crop(roads, NE) 
  roads_SA <- st_crop(roads, full_bbox)
  roads_OKcoug <- st_crop(roads, OK_coug_bbox) 
  roads_NEcoug <- st_crop(roads, NE_coug_bbox)
  
  #  Visualize an example
  ggplot() +
    geom_sf(data = NE) +
    geom_sf(data = roads_NEcoug)
  
  #  Save as individual shapefiles that are more manageable to work with
  # st_write(roads_OK, dsn = "Shapefiles/Roads_OK", layer = "roads_OK", driver = "ESRI Shapefile")
  # st_write(roads_NE, dsn = "Shapefiles/Roads_NE", layer = "roads_NE", driver = "ESRI Shapefile")
  # st_write(roads_SA, dsn = "Shapefiles/Roads", layer = "roads_SA", driver = "ESRI Shapefile")
  st_write(roads_OKcoug, dsn = "Shapefiles/Roads_OKcoug", layer = "roads_OKcoug", driver = "ESRI Shapefile")
  st_write(roads_NEcoug, dsn = "Shapefiles/Roads_NEcoug", layer = "roads_NEcoug", driver = "ESRI Shapefile")
  
  
  ####  WATER  ####
  #  Data come from USDA (https://gdg.sc.egov.usda.gov/GDGOrder.aspx)
  #  Shapefiles are broken up by county (Okanogan, Chelan, Stevens, Pend Oreille,
  #  and Spokane). Each shapefile is manipulated and saved as new shapefile based 
  #  on portion that falls within full_bbox (hense, not the entire county).

  #### NOTE: be sure to use correct bounding box and change the object name if 
  #### switching between bounding boxes with different extents!!!!

  #  Streams only (does not include water bodies e.g., lakes)
  #  Read in, transform, and crop shapefiles
  hydro_OKcoug <- st_read("Shapefiles/National_Hydrography_Dataset/hydrography_NHD24K_wa047_Okanogan/hydrography/nhd24kst_l_wa047.shp") %>%
    st_transform(crs = "+init=epsg:2855") %>%
    st_zm() %>%  # st_zm() drops extra dimensions in the XYZM geometry that prevent sf operations from working
    st_crop(., OK_coug_bbox)  # Okanogan County streams
  hydro_Chcoug <- st_read("Shapefiles/National_Hydrography_Dataset/hydrography_NHD24K_wa007_Chelan/hydrography/nhd24kst_l_wa007.shp") %>%
    st_transform(crs = "+init=epsg:2855") %>%
    st_zm() %>%  
    st_crop(., OK_coug_bbox)  # Chelan County streams
  hydro_Stcoug <- st_read("Shapefiles/National_Hydrography_Dataset/hydrography_NHD24K_wa065_Stevens/hydrography/nhd24kst_l_wa065.shp") %>%
    st_transform(crs = "+init=epsg:2855") %>%
    st_zm() %>%
    st_crop(., NE_coug_bbox)  # Stevens County streams
  hydro_POcoug <- st_read("Shapefiles/National_Hydrography_Dataset/hydrography_NHD24K_wa051_PendOreille/hydrography/nhd24kst_l_wa051.shp") %>%
    st_transform(crs = "+init=epsg:2855") %>%
    st_zm() %>%
    st_crop(., NE_coug_bbox)  # Pend Oreille County streams
  hydro_Spcoug <- st_read("Shapefiles/National_Hydrography_Dataset/hydrography_NHD24K_wa063_Spokane/hydrography/nhd24kst_l_wa063.shp") %>%
    st_transform(crs = "+init=epsg:2855") %>%
    st_zm() %>%
    st_crop(., NE_coug_bbox)  # Spokane County streams


  # #  Combine hydro data from St, PO, & Sp counties into 1 dataset for the NE
  # #  Stop trying to make this work! Computer spazzes out every time
  # hydro_NE <- st_union(hydro_St, hydro_PO)
  # 
  # #  Try different data source for WA hydrography
  # hydro_OK <- st_read("Shapefiles/WA_DeptEcology_HydroWA/NHDWA.gdb", layer = "NHDFlowline") %>%
  #   st_transform(crs = "+init=epsg:2855")
  # tst <- st_read("Shapefiles/WA_DeptEcology_HydroWA/NHDFLowline.shp") %>%
  #   st_transform(crs = "+init=epsg:2855")
  ##  can't seem to get this one to work- will have to use county specific datasets
  
  #  Visualize to make sure it worked
  #  Keep in mind non-rectangular boundries on hydro shapefiles are county lines
  ggplot() +
    geom_sf(data = NE) + 
    geom_sf(data = hydro_POcoug) + 
    geom_sf(data = hydro_Stcoug) +
    geom_sf(data = hydro_Spcoug)
  
  #  Save as individual shapefiles
  # st_write(hydro_OK, dsn = "Shapefiles/Hydro_OK", layer = "hydro_OK", driver = "ESRI Shapefile")
  # st_write(hydro_Ch, dsn = "Shapefiles/Hydro_Ch", layer = "hydro_Ch", driver = "ESRI Shapefile")
  # st_write(hydro_St, dsn = "Shapefiles/Hydro_St", layer = "hydro_St", driver = "ESRI Shapefile")
  # st_write(hydro_PO, dsn = "Shapefiles/Hydro_PO", layer = "hydro_PO", driver = "ESRI Shapefile")
  # st_write(hydro_Sp, dsn = "Shapefiles/Hydro_Sp", layer = "hydro_Sp", driver = "ESRI Shapefile")
  st_write(hydro_OKcoug, dsn = "Shapefiles/Hydro_OKcoug", layer = "hydro_OKcoug", driver = "ESRI Shapefile")
  st_write(hydro_Chcoug, dsn = "Shapefiles/Hydro_Chcoug", layer = "hydro_Chcoug", driver = "ESRI Shapefile")
  st_write(hydro_Stcoug, dsn = "Shapefiles/Hydro_Stcoug", layer = "hydro_Stcoug", driver = "ESRI Shapefile")
  st_write(hydro_POcoug, dsn = "Shapefiles/Hydro_POcoug", layer = "hydro_POcoug", driver = "ESRI Shapefile")
  st_write(hydro_Spcoug, dsn = "Shapefiles/Hydro_Spcoug", layer = "hydro_Spcoug", driver = "ESRI Shapefile")
  #st_write(hydro_NE, dsn = "Shapefiles/Hydro_NE", layer = "hydro_NE", driver = "ESRI Shapefile")

  

  
  
  