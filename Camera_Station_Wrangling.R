  ##  Camera station data prep
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ================================
  ##  Organize camera deployment and checking data
  
  #  Load packages
  library(tidyverse)
  
  #  Read in & select relevant data
  cameras <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Deployment_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year", 
                  "Cell_No", "Camera_ID", "Camera_Lat", "Camera_Long", "Make", 
                  "Dist_to_road_m", "Height_from_ground_m", "Feature_monitored", 
                  "Azimuth", "Slope", "Aspect", 
                  "Canopy_Cov_percent", "Land_mgnt", "Habitat_type") %>%
    dplyr::filter(Make == "Reconyx") %>%  # excluding Moultrie data b/c its crap
    dplyr::filter(Cell_No != "OKbonus")   # not an official study camera
    colnames(cameras) <- c("Date", "Study_Area", "Year", "Cell_ID", 
                           "Camera_ID", "Camera_Lat", "Camera_Long", "Make",
                           "Dist_focal_pt", "Height_frm_grnd", "Monitoring",
                           "Azimuth", "Slope", "Aspect", 
                           "Canopy_Cov", "Land_mgnt", "Habitat_type")
    ncams <- nrow(cameras) 
    deployed <- as.data.frame(rep("Deployed", ncams))
    colnames(deployed) <- "Status"
    cams <- cbind(deployed, cameras)
    
    
  checks18 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year", "Cell_No", 
                  "Camera_ID", "Camera_Lat", "Camera_Long", "Make",
                  "Condition", "How_damaged", "Cam_replaced", "No_Images",
                  "Adj_to_placement", "Why1", 
                  "New_location", "New_Cam_Lat", "New_Cam_Long",
                  "Cam_removed", "Why2") %>%
    dplyr::filter(Make == "Reconyx") %>% # most Moultrie pulled by 6/3 but two were pulled 6/30
    dplyr::filter(Cell_No != "OKbonus")
  colnames(checks18) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Camera_Lat", "Camera_Long", "Make",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2", 
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3")
  nchks18 <- nrow(checks18) 
  checked18 <- as.data.frame(rep("Checked", nchks18))
  colnames(checked18) <- "Status"
  chks18 <- cbind(checked18, checks18)
  
  
  checks19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2019.csv")) %>%
    dplyr::select("Date", "Study_Area", "Year", "Cell_ID", 
                  "Cam_ID", "Cam_Lat", "Cam_Long", 
                  "Cam_Condition", "Explain1", "Cam_Replaced", "Num_Images",
                  "Adjustments", "Explain2", 
                  "New_Location", "New_Lat", "New_Long",
                  "Cam_Removed", "Explain3", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect") %>%
    dplyr::filter(Year == "Year1")
  colnames(checks19) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Camera_Lat", "Camera_Long",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2",
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3",
                          "Azimuth", "Slope", "Aspect")
  nchks19 <- nrow(checks19) 
  checked19 <- as.data.frame(rep("Removed", nchks19))
  colnames(checked19) <- "Status"
  chks19 <- cbind(checked19, checks19)
  
  
  #  Slim this down for now
  cameras_slim <- dplyr::select(cams, c("Status", "Date", "Study_Area", "Cell_ID", 
                                   "Camera_ID", "Camera_Lat", "Camera_Long",
                                   "Dist_focal_pt", "Height_frm_grnd", "Monitoring",
                                   "Canopy_Cov", "Land_mgnt", "Habitat_type"))
  checks18_slim <- dplyr::select(chks18, c("Status", "Date", "Study_Area", "Cell_ID", 
                                      "Camera_ID", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images", 
                                      "Adjustments", "Explain2", "Cam_Removed"))
  checks19_slim <- dplyr::select(chks19, c("Status", "Date", "Study_Area", "Cell_ID",  
                                      "Camera_ID", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images",
                                      "Adjustments", "Explain2","Cam_Removed"))
  
  # Bind deploy and check dataframes together into single file
  camera_master_2018 <- bind_rows(cameras_slim, checks18_slim, checks19_slim)

  #write.csv(camera_master_2018, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018.csv")
  
  
    
  