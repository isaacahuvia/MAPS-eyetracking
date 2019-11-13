###########################
##  Eyetracking Data #1  ##
##       Read Data       ##
###########################

rm(list = ls())
library(yaml)
library(readr)
library(zoo)
library(dplyr)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
#A list of all files for which we have data for a specific participant, to load
file_list <- list.files(path = lookup$pc$raw_data, full.names = T)
#Remove files that shouldn't be downloaded yet
file_list <- file_list[-96]

# #Limit to only QA files
# QA <- c("3251", "3332", "3589", "4682", "4957")
# file_list <- file_list[grepl(paste(QA, collapse = "|"), file_list)]


#Read data
for(file in file_list) {
  
  print(paste0(which(file_list == file), " of ", length(file_list), ": ", file))
  
  id <- substr(file, 109, 112)
  
  temp <- readr::read_tsv(file) %>%
    dplyr::mutate(
      participantID = id,
      qText = dplyr::if_else(!is.na(zoo::na.locf(StudioEventData, na.rm = F)), zoo::na.locf(StudioEventData, na.rm = F), "[First Question]"), #Fills down `StudioEventData` into NA rows; initial NA rows are manually marked "[First Question]"
      trackLoss = F
      # trackLoss = ValidityLeft == 4 | ValidityRight == 4 #A column to indicate if either the left or right eye data is invalid - let's try running this without any filtering on trackloss
    ) %>% 
    #These next two lines are to create a new variable, `timestamp`, that is a running total of the eye tracker's timestamp within each question (trial)
    dplyr::group_by(participantID, qText) %>% 
    dplyr::mutate(timestamp = EyeTrackerTimestamp - min(EyeTrackerTimestamp, na.rm = T)) %>% #in microseconds - see https://www.tobiipro.com/siteassets/tobii-pro/user-manuals/tobii-pro-studio-user-manual.pdf
    dplyr::ungroup() %>%
    #Select only wanted variables
    dplyr::select(
      participantID,
      timestamp,
      test = StudioTestName,
      qText,
      event = StudioEvent,
      gazeType = GazeEventType,
      gazeDuration = GazeEventDuration,
      gazePointIndex = GazePointIndex,
      gazeAvgX = "GazePointX (ADCSpx)",
      gazeAvgY = "GazePointY (ADCSpx)",
      trackLoss
    ) %>%
    #Filter to only rows with a non-NA timestamp, as this would mess with processing
    dplyr::filter(!is.na(timestamp),
                  qText != "[First Question]") #Check with Tobii about this, but it doesn't seem like this is *actually* the first question - it is the first couple
  #of seconds of the overall participant record, but the following qText fields are actually the first question texts of instruments
  
  if(file == file_list[1]) {
    df <- temp
  } else {
    df <- rbind(df, temp)
  }
  
}



####  Output Data  ####
save(df, file = lookup$pc$clean_data$merged)