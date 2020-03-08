#############################
##   Eyetracking Data #4   ##
##  Make Eyetracking Data  ##
#############################


rm(list = ls())
library(yaml)
library(eyetrackingR)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
load(file = lookup$clean_data$with_AOIs)



####  Prepare Sample-Level Data  ####
#Create a time column in seconds (current timestamp is in microseconds)
df$timestamp.sec <- df$timestamp / 1000000
df$time.sec <- df$timestamp.sec - lag(df$timestamp.sec)
df$time.sec[df$time.sec < 0 | df$time.sec > .01] <- NA

#Not sure what this is about, but for now just filter it out. It's just in 3301
df <- df[!is.na(df$qID),]

#Remove cases where the timestamp is > 90 sec (generally means the participant left to go to the bathroom or something similar)
df <- df[!df$timestamp.sec > 90,]

#Set aside the sample-level data (no summary by item or participant)
tobii.sample <- df
# save(tobii.sample, file = lookup$clean_data$analysis_ready$sample_level)



####  Prepare Item-Level Data  ####
df <- df %>%
  dplyr::filter(trackLoss == F) %>%
  dplyr::group_by(participantID, qID) %>%
  dplyr::summarise(dur.tot = sum(time.sec, na.rm = T),
                   dur.qText = sum(time.sec[qText == T], na.rm = T),
                   start = min(timestamp.overall, na.rm = T)) %>%
  dplyr::arrange(participantID, start) %>%
  dplyr::select(-start) %>%
  dplyr::group_by(participantID) %>%
  dplyr::mutate(qIndex = row_number()) %>%
  dplyr::mutate(measure = dplyr::case_when(grepl("^PROMIS A Q", qID)   ~ "PROMIS A",
                                           grepl("^PROMIS F Q", qID)   ~ "PROMIS F",
                                           grepl("^PROMIS GH Q", qID)  ~ "PROMIS GH",
                                           grepl("^PROMIS PA Q", qID)  ~ "PROMIS PA",
                                           grepl("^PROMIS PFR Q", qID) ~ "PROMIS PFR",
                                           grepl("^PROMIS PSE Q", qID) ~ "PROMIS PSE",
                                           T                           ~ "MAP-DB"))

#Add on extreme response indicator
load(file = lookup$clean_data$responses)

df$qID <- gsub(" ", "_", df$qID)
df$response <- NA
df$minimumFlag <- NA
df$extremeFlag <- NA

for(x in unique(responses$participantID)) {
  
  temp <- responses[responses$participantID == x,]
  
  for(q in names(temp)[2:length(names(temp))]) {
    
    response <- temp[[q]]
    
    df$response[df$participantID == x & df$qID == q] <- response
    
  }
  
}

#Flag extreme responses
extremeResponses <- c("Never", "Many times each day", "No days", "Almost Always", "Always", "Excellent", "Poor", "All the time")
df$extremeFlag <- dplyr::if_else(df$response %in% extremeResponses, T, F)
  
#Flag lowest responses
lowestResponses <- c("Never", "Excellent", "No days")
df$minimumFlag <- dplyr::if_else(df$response %in% lowestResponses, T, F)



df$qID <- gsub("_", " ", df$qID)

#Set aside the item-level data (no summary by participant)
tobii.item <- df
save(tobii.item, file = lookup$clean_data$analysis_ready$item_level)



####  Prepare Participant-Level Data  ####
df <- df %>%
  dplyr::group_by(participantID) %>%
  dplyr::summarise(dur.tot = mean(dur.tot),
                   dur.qText = mean(dur.qText))

#Add on percent of valid gaze samples
validityByParticipant <- read.csv("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Data\\Eyetracking\\Raw Data\\Gaze Percentages.csv")
validityByParticipant <- dplyr::rename(validityByParticipant, participantID = ID, pctValidGazeSamples = Avg)
df <- merge(df, validityByParticipant, by = "participantID")

#Save the participant-level data
tobii.participant <- df
save(tobii.participant, file = lookup$clean_data$analysis_ready$participant_level)

#Add on pctValidGazeSamples to item-level data and re-save
tobii.item <- merge(tobii.item, dplyr::select(tobii.participant, participantID, pctValidGazeSamples))
save(tobii.item, file = lookup$clean_data$analysis_ready$item_level)