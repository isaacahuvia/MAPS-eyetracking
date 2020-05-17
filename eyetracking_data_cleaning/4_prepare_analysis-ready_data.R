#############################
##   Eyetracking Data #4   ##
##  Make Eyetracking Data  ##
#############################


####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("yaml", "openxlsx", "zoo", "conflicted", "magrittr", "eyetrackingR", "tidyverse")
walk(c("lag", "filter", "select", "group_by", "summarise"), 
     ~ conflict_prefer(., "dplyr"))

lookup <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")



####  Read Data  ####
dfSample <- readRDS(lookup$tobii$working_data$with_AOIs)



####  Prepare Sample-Level Data  ####
dfSample %<>%
  #Create a time column in seconds (current timestamp is in microseconds)
  mutate(timestamp.sec = timestamp / 1000000,
         time.sec = (timestamp.sec - lag(timestamp.sec)) %>%
           ifelse(. < 0 | . > .01, NA, .)) %>%
  filter(  
    
    #Not sure what this is about, but for now just filter it out. It's just in 3301
    #NOTE - also includes reversed items? Look into this
    !is.na(qID),
    
    #Remove cases where the timestamp is > 90 sec (generally means the participant left to go to the bathroom or something similar)
    timestamp.sec <= 90
    
    )

#Set aside the sample-level data (no summary by item or participant)
saveRDS(dfSample, file = lookup$tobii$clean_data$sample_level)

#Quickly summarize to person-level, calculating percent valid gaze samples
pctValid <- dfSample %>%
  group_by(participantID) %>%
  summarize(pctValidGazeSamples = mean(!is.na(gazeAvgX)))
saveRDS(pctValid, lookup$tobii$working_data$pct_valid)



####  Prepare Item-Level Data  ####
dfItem <- dfSample %>%
  filter(trackLoss == F) %>%
  group_by(participantID, qID) %>%
  summarise(dur.tot = sum(time.sec, na.rm = T),
            dur.qText = sum(time.sec[qText == T], na.rm = T),
            fixations = sum(rle(gazeType)$values == "Fixation"),
            start = min(timestamp.overall, na.rm = T)) %>%
  arrange(participantID, start) %>%
  select(-start) %>%
  group_by(participantID) %>%
  mutate(qIndex = row_number()) %>%
  mutate(measure = dplyr::case_when(grepl("^PROMIS A Q", qID)   ~ "PROMIS A",
                                    grepl("^PROMIS F Q", qID)   ~ "PROMIS F",
                                    grepl("^PROMIS GH Q", qID)  ~ "PROMIS GH",
                                    grepl("^PROMIS PA Q", qID)  ~ "PROMIS PA",
                                    grepl("^PROMIS PFR Q", qID) ~ "PROMIS PFR",
                                    grepl("^PROMIS PSE Q", qID) ~ "PROMIS PSE",
                                    T                           ~ "MAP-DB")) %>%
  ungroup() %>%
  
  #Calculate z-score variables
  group_by(qID) %>%
  mutate(dur.tot_mean = mean(dur.tot, na.rm = T),
         dur.tot_sd = sd(dur.tot, na.rm = T),
         dur.qText_mean = mean(dur.qText, na.rm = T),
         dur.qText_sd = sd(dur.qText, na.rm = T)) %>%
  ungroup() %>%
  mutate(dur.tot_z = (dur.tot - dur.tot_mean) / dur.tot_sd,
         dur.qText_z = (dur.qText - dur.qText_mean) / dur.qText_sd) 


#Add on extreme response indicator
responses <- readRDS(lookup$tobii$working_data$responses)

#Reshape for merge
responses %<>%
  pivot_longer(-participantID,
               names_to = "qID",
               values_to = "response") %>%
  mutate(qID = gsub("_", " ", qID))

extremeResponses <- c("Never", "Many times each day", "No days", "Almost Always", "Always", "Excellent", "Poor", "All the time")
lowestResponses <- c("Never", "Excellent", "No days")

dfItem %<>%
  left_join(responses, by = c("participantID", "qID")) %>%
  mutate(extremeFlag = response %in% extremeResponses,
         minimumFlag = response %in% lowestResponses)

#Merge on pctValidGazeSamples and save
dfItem %<>%
  left_join(pctValid, by = "participantID")
saveRDS(dfItem, file = lookup$tobii$clean_data$item_level)



####  Prepare Participant-Level Data  ####
dfParticipant <- dfItem %>%
  group_by(participantID, pctValidGazeSamples) %>%
  summarise(dur.tot = mean(dur.tot),
            dur.qText = mean(dur.qText),
            meanFixations = mean(fixations),
            tobii.avgQTextFixationTime_sd = sd(dur.qText, na.rm = T),
            tobii.avgQuestionDuration_sd = sd(dur.tot, na.rm = T),
            tobii.pctUnder4Sec = mean(dur.tot < 4, na.rm = T),
            tobii.avgQuestionDuration_z = mean(dur.tot_z, na.rm = T),
            tobii.avgQTextFixationTime_z = mean(dur.qText_z, na.rm = T)) %>%
  ungroup()

#Save the participant-level data
saveRDS(dfParticipant, file = lookup$tobii$clean_data$participant_level)
