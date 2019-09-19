  ##  Camera Trap Detection Histories
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ================================
  ##  Combines species classification data from camera traps with deployment
  ##  data from each camera trap station. Uses camtrapR package to generate 
  ##  species-specific encounter histories that can be used for occupancy models.
  
  ##  Acknowledgements: Script based on code originally provided by Mitch Pearson
  ##  and Michael Haverda.
  ##  ================================
  
  #  Load packages
  library(camtrapR)
  #library(anytime)
  library(tidyverse)
  
  #  Read in data
  #  Camera trap stations: deploy and removal dates
  cam_stations <- read.csv("./Data/OK_Camera_Stations_18-19.csv")
  #  Classified images from camera traps- this takes a few seconds
  megadata <- read.csv("./Data/MegaData_Cameras_summer18_091919.csv")
  
  str(megadata)
  
  
  #  Filter and clean data
  #  ==================================
  #  Camera trap data
  images <- megadata %>%
    #  Remove images of cameras being serviced
    filter(Service == "FALSE") %>%
    #  Remove empty images
    filter(Empty == "FALSE") %>%
    filter(Species != "") %>%
    filter(Species != "--") %>%
    #  Thin down the number of columns
    select("File", "Date", "Time", "CameraLocation", "Service", "Empty", "Animal", 
           "Human", "Vehicle", "Species", "HumanActivity", "Count", "Comments") %>%
    #  Reformat dates, add extra column for Cell_ID, & repeat CameraLocation
    mutate(
      Date = as.Date(Date, format = "%d-%b-%y"), # note the date formatting here!
      Cell_ID = as.character(str_sub(CameraLocation, 1, 6)),
      Camera_ID = as.character(CameraLocation)
    )
  #  Remove cell_ID from second CameraLocation, leaving column with Camera_ID
  str_sub(images$Camera_ID, 1, 7) <- ""
  #  Number rows and add to dataframe
  ID <- as.data.frame(1:nrow(images))
  colnames(ID) <- "ID"
  images <- cbind(images, ID)
  #  Create unique name for each individual image file
  images$Image <- str_c(images$Cell_ID, images$Camera_ID, images$File, images$ID,  sep = "-")
  
  #  Filter dates to specific range (06/01/2018 - 08/31/2018)
  images_summer18 <- images %>%
    filter(Date > "2018-05-31") %>%
    filter(Date < "2018-09-01") %>%
    select("Image", "File", "Cell_ID", "Camera_ID", "Date", "Time", "Species")
  
  
  #  Camera station data
  str(cam_stations)
  #  Remove cameras that never collected data or retrieved
  #  Reformat dates to be consistent with camera trap data above
  stations <-  cam_stations %>%
    filter(Notes != "burned") %>%
    filter(Retrieval_date != "NA") %>%
    transmute(
      Cell_ID = Cell_ID,
      Camera_ID = Camera_ID, 
      UTM_X = Longitude,
      UTM_Y = Latitude,
      Set_date = as.Date(Setup_date, format = "%m/%d/%Y"),  # Note the different date formatting here!
      Pull_date = as.Date(Retrieval_date, format = "%m/%d/%Y"),
      Problem1_from = as.Date(Problem1_from, format = "%m/%d/%Y"),
      Problem1_to = as.Date(Problem1_to, format = "%m/%d/%Y"),
      Problem2_from = as.Date(Problem2_from, format = "%m/%d/%Y"),
      Problem2_to = as.Date(Problem2_to, format = "%m/%d/%Y")
    )

  #  Were any stations established after time window of interest (08/31/2018)
  late_deploy <- stations[stations$Set_date > "2018-08-31",]
  print(late_deploy)
  
  #  Remove stations that were established after time window of interest (08/31/2018)
  stations <- stations[stations$Set_date < "2018-08-31",]
  
  head(stations)
  str(stations)
  is.na(stations$Pull_date)
  
  #write.csv(stations, "./Data/OK_cam_stations.csv")

  
  #  camtrapR time!
  #  =========================================
  #  Calucate number of trap nights per camera
  trapnights <- as.numeric(stations$Pull_date - stations$Set_date)
  
  #  Create camera operation table- needed for creating deteciton histories
  #  Creates a matrix with each camera and date it was deployed
  #  1 = operating, 0 = not operating but deployed, NA = not deployed

  #  NOTE: Consider double cameras in NE- is each camera a unique station or are
  #  there 2 cameras at the same station? Decision will affect how you set up 
  #  data and these functions.
  cam_probs <- cameraOperation(CTtable = stations,
                                stationCol = "Cell_ID",
                                cameraCol = "Camera_ID",
                                setupCol = "Set_date",
                                retrievalCol = "Pull_date",
                                writecsv = F, 
                                hasProblems = T,
                                byCamera = F,  
                                # F = operability matrix computed by station, not camera
                                allCamsOn = F,            
                                # F = at least 1 camera is active to consider station operational
                                camerasIndependent = F, 
                                # F = doesn't matter how many cameras are present as long as 1 is operational
                                # T = detections are considered independent between cameras at 1 station
                                dateFormat = "%Y-%m-%d")  # note the date format
  
  probs <- as.data.frame(cam_probs)
  head(probs)
  
  
  #  Double check Cell_ID match up between the two data sets
  #  This will cause you pain if there are mismatches!!!
  #  "Error in detectionHistory(recordTable = images_summer18, camOp = cam_probs,: 
  #  Not all values of stationCol in recordTable are matched by rownames of camOp"
  #  list out unique Cell_ID names from each data stream
  cells1 <- unique(images_summer18$Cell_ID)
  cells2 <- unique(rownames(probs))
  #  Count the number of names from each
  n1 <- 1:length(cells1)
  n2 <- 1:length(cells2)
  #  Create dataframes for each data stream
  img_dat <- as.data.frame(cbind(n1, cells1))
  colnames(img_dat) <-c("img_station_n", "Cell_ID")
  cam_dat <- as.data.frame(cbind(n2, cells2, cells2))
  colnames(cam_dat) <- c("station_n", "Cell_ID", "Station_cells")
  #  Join dataframes by Cell_ID- should line up perfectly
  #  NA's indicate mismatch
  matching <- cam_dat %>%
    full_join(img_dat, by = "Cell_ID")
  
  
  #  Create detection history for a single species
  #  Combine date:time into a column to create detection histories
  #  For now going with 14 day sampling occasions
  images_summer18$DateTimeOriginal <- with(images_summer18, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
  
  head(images_summer18)
  
  #  Cougar DH
  DetHist_coug <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID", 
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Cougar",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_coug)
  
  #  Mule deer DH
  DetHist_mule <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID", 
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Mule Deer",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_mule)

  
  #  Questions:  How do I truncate the encounter histories so they end in Aug?
  
  
  
  
  
  #  Wolf DH
  DetHist_wolf <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID", 
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Wolf",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_wolf)
  
  #  White-tailed deer DH
  DetHist_wtd <- detectionHistory(recordTable = images_summer18,
                                  camOp = cam_probs,
                                  stationCol = "Cell_ID", 
                                  speciesCol = "Species",
                                  recordDateTimeCol = "DateTimeOriginal",
                                  species = "White-tailed Deer",
                                  occasionLength = 14, # number of days
                                  day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                  includeEffort = F,   # fills in NA when station was malfunctioning
                                  timeZone = "US/Pacific",
                                  writecsv = T,
                                  outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_wtd)
  
  #  Elk DH
  DetHist_elk <- detectionHistory(recordTable = images_summer18,
                                  camOp = cam_probs,
                                  stationCol = "Cell_ID", 
                                  speciesCol = "Species",
                                  recordDateTimeCol = "DateTimeOriginal",
                                  species = "Elk",
                                  occasionLength = 14, # number of days
                                  day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                  includeEffort = F,   # fills in NA when station was malfunctioning
                                  timeZone = "US/Pacific",
                                  writecsv = T,
                                  outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_elk)
  
  #  Black bear DH
  DetHist_blkbear <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID",
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Black Bear",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_blkbear)
  
  #  Moose DH
  DetHist_moose <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID",
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Moose",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_moose)
  
  #  Coyote DH
  DetHist_coy <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID",
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Coyote",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_coy)
  
  #  Bobcat DH
  DetHist_bobcat <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID",
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Bobcat",
                                   occasionLength = 14, # number of days
                                   day1 = "2018-06-13", # start detecion history when 1st camera deployed
                                   includeEffort = F,   # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = T,
                                   outDir = "G:/My Drive/1_Repositories/MultiSpp_Cameras/Output")
  head(DetHist_bobcat)
  