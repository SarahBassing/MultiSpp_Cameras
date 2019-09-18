  ##  Generate random locations for multiple individuals
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  =============================================
  ##  Script to break apart collar data by animal ID and estimate individual 
  ##  home ranges, then randomly sample "available" points from within each home 
  ##  range to use for covariate extraction and RSFs.
  
  #  Load packages
  library(tidyverse)
  library(sp)
  library(adehabitatHR)
  
  #  Read in data
  cougars_summer18 <- read.csv("./input/cougars_summer18.csv")
  
  #  Dataframe of unique animal IDs
  unq_id <- as.data.frame(cougars_summer18$Animal_ID) %>%
    distinct()
  colnames(unq_id) <- "Animal_ID"
  
  #  How many unique animals are there?
  n_animals <- nrow(unq_id)

  
  #  Function to create SpatialPoints for each indvidual, estimate KUD, and
  #  randomly sample points within KUD. Output is a list of locations by 
  #  individual to use as "available" points for RSF.
  #  ======================================================================
  
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
  
  
  #  Mega df to hold all points corresponding to unique Animal_ID
  #  ============================================================
  
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
 
  