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
file_list <- list.files(path = lookup$raw_data, full.names = T)

#Remove 3248, which does not parse correctly
file_list <- file_list[!grepl("3248", file_list)]
#Remove non-raw data directories/files
file_list <- file_list[!grepl("Files to repair", file_list)]
file_list <- file_list[!grepl("Gaze Percentages", file_list)]

#Read data
for(file in file_list) {
  
  print(paste0(which(file_list == file), " of ", length(file_list), ": ", file))
  
  id <- substr(file, 109, 112)
  
  temp <- readr::read_tsv(file)
  names(temp) <- gsub("\\[", "", names(temp))
  names(temp) <- gsub("\\]Value", "", names(temp))
  names(temp) <- gsub(" ", "_", names(temp))
  
  namesToKeep <- c(
    "participantID",
    "c6mapdba",
    paste0("c6mapdb", 1:113),
    paste0("PROMIS_A_Q", 1:5),
    paste0("PROMIS_F_Q", 1:10),
    paste0("PROMIS_PA_Q", 1:8),
    paste0("PROMIS_PFR_Q", 1:8),
    paste0("PROMIS_PSE_Q", 1:8),
    paste0("PROMIS_GH_Q", 1:9)
  )
  
  clean <- temp %>%
    dplyr::mutate(participantID = id) %>%
    dplyr::select(namesToKeep)
  
  #By default, Tobii exports question responses for all questions in a project - even the ones that a participant doesn't answer in a given recording. That's why, without this step,
  #we end up with a ton of "Never"s. To fix this, we replace the response fields in `clean` with the response fields from `temp` in the appropriate test
  lowestResponses <- c("Never", "Excellent", "No days", "Yes")
  responses.participant <- sapply(clean, unique)
  clean <- clean[1,]
  
  for(x in namesToKeep) {
    
    responses.x <- responses.participant[[x]]
    
    if(length(responses.x) == 1) {
      
      clean[[x]] <- responses.x
      
      if(length(clean[[x]]) > 1) stop("Length > 1")
      
    } else {
      
      responses.x <- setdiff(responses.x, lowestResponses)
      clean[[x]] <- responses.x
      
      if(length(clean[[x]]) > 1) stop("Length > 1")
      
    }
    
  }
  
  if(file == file_list[1]) {
    responses <- clean
  } else {
    responses <- rbind(responses, clean)
  }
  
}



####  Output Data  ####
save(responses, file = lookup$clean_data$responses)