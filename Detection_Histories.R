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
  cam_stations <- read.csv("./Input/OK_Camera_Stations_18-19.csv")
  #  Classified images from camera traps- this takes a few seconds
  megadata <- read.csv("./Input/MegaData_Cameras_summer18-MISSING_Beth&Robbie.csv")
  
  str(megadata)
  
  
  #  Filter and clean data
  #  ==================================
  #  Camera trap data
  images <- megadata %>%
    #  Remove empty images
    filter(Empty == "FALSE") %>%
    #  Remove images of cameras being serviced
    filter(Service == "FALSE") %>%
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
      UTM_X = Latitude,
      UTM_Y = Longitude,
      Set_date = as.Date(Setup_date, format = "%m/%d/%Y"),  # Note the different date formatting here!
      Pull_date = as.Date(Retrieval_date, format = "%m/%d/%Y"),
      Problem1_from = as.Date(Problem1_from, format = "%m/%d/%Y"),
      Problem1_to = as.Date(Problem1_to, format = "%m/%d/%Y"),
      Problem2_from = as.Date(Problem2_from, format = "%m/%d/%Y"),
      Problem2_to = as.Date(Problem2_to, format = "%m/%d/%Y")
    )

  head(stations)
  str(stations)
  is.na(stations$Pull_date)
  
  #  camtrapR time!
  #  =========================================
  #  Calucate number of trap nights per camera
  trapnights <- as.numeric(stations$Pull_date - stations$Set_date)
  
  #  Create camera operation table- needed for creating deteciton histories
  #  Creates a matrix with each camera and date it was deployed
  #  1 = operating, NA = not deployed
  cam_probs <- cameraOperation(CTtable = stations,
                                stationCol = "Cell_ID",
                                cameraCol = "Camera_ID",
                                setupCol = "Set_date",
                                retrievalCol = "Pull_date",
                                writecsv = F, 
                                hasProblems = T,
                                byCamera = T,
                                allCamsOn = F,
                                dateFormat = "%Y-%m-%d")  # note the date format
  
  head(cam_probs)
  probs <- as.data.frame(cam_probs)
  
  #  Combine date:time into a column to create detection histories
  images_summer18$DateTimeOriginal <- with(images_summer18, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
  
  head(images_summer18)
  
  #  Create detection history for a single species
  DetHist_coug <- detectionHistory(recordTable = images_summer18,
                                   camOp = cam_probs,
                                   stationCol = "Cell_ID", 
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Cougar",
                                   occasionLength = 7, # number of days
                                   day1 = "station",   # start on deployment date
                                   includeEffort = F,  # fills in NA when station was malfunctioning
                                   timeZone = "US/Pacific",
                                   writecsv = F,
                                   outDir = "./Input")


  #Error in detectionHistory(recordTable = images_summer18, camOp = cam_probs,  : 
  #Not all values of stationCol in recordTable are matched by rownames of camOp
  