#This ended up in the old .rmd, and relies on some files we don't have right now.
#It looks like the variables this produces are already in the saved data, but we'll
#want to re-incorporate this into the workflow asap.


rm(list = ls())
library(yaml)
library(eyetrackingR)
library(ggplot2)
library(dplyr)
library(RCurl)
library(RItools)
lookup <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")



####  Read Data  ####
## Tobii data with qIndex
load(file = lookup$clean_data$analysis_ready$item_level)



## REDCap data, then join with tracker to generate qIndex
token.FUS <- read.csv("P:\\Personal REDCap API Token - FUS.txt", header = F, stringsAsFactors = F)[[1]][1]
REDCap.FUS <- read.csv(text = postForm(uri="https://redcap.nubic.northwestern.edu/redcap/api/",
                                       token=token.FUS,
                                       content="record",
                                       format="csv",
                                       type="flat",
                                       "forms[0]"="names_demographics",
                                       "forms[1]"="tween_echo_visit",
                                       rawOrLabel="raw",
                                       rawOrLabelHeaders="raw",
                                       exportCheckboxLabel="false",
                                       exportSurveyFields="false",
                                       exportDataAccessGroups="false",
                                       returnFormat="json",
                                       filterLogic="[tev_vstatus] = 2"),
                       stringsAsFactors = F)
REDCap.FUS <- dplyr::rename(REDCap.FUS, participantID = studyid)

token.ECHO <- read.csv("P:\\Personal REDCap API Token - ECHO.txt", header = F, stringsAsFactors = F)[[1]][1]
REDCap.ECHO <- read.csv(text = postForm(uri='https://redcap.nubic.northwestern.edu/redcap/api/',
                                        token=token.ECHO,
                                        content='record',
                                        format='csv',
                                        type='flat',
                                        rawOrLabel='raw',
                                        rawOrLabelHeaders='raw',
                                        exportCheckboxLabel='false',
                                        exportSurveyFields='false',
                                        exportDataAccessGroups='false',
                                        returnFormat='json'),
                        stringsAsFactors = F)
REDCap.ECHO <- dplyr::rename(REDCap.ECHO, participantID = studyid)
REDCap.ECHO <- REDCap.ECHO[REDCap.ECHO$participantID %in% REDCap.FUS$participantID,]

tracker <- openxlsx::read.xlsx("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\MAP-DB & PROMIS Manual Counterbalancing.xlsx") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(measureOrder = paste0(c(Set.1, Set.2, Set.3), collapse = ", ")) %>% 
  dplyr::select(participantID = ID,
                measureOrder) %>%
  dplyr::ungroup()

lookup <- openxlsx::read.xlsx("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Data\\REDCap Question Index Lookup.xlsx") 

REDCap <- REDCap.ECHO %>%
  dplyr::select(participantID, lookup$varName.raw) %>%
  dplyr::filter(!participantID %in% tobii.item$participantID) %>%
  tidyr::gather(key = "qID", value = "value", c6mapdba:c6proang07)

REDCap <- merge(REDCap, tracker, by = "participantID")

for(qID in REDCap$qID) {
  
  REDCap$qID[REDCap$qID == qID] <- lookup$varName.fixed[lookup$varName.raw == qID]
  
}

REDCap$qIndex <- NA

for(id in unique(REDCap$participantID)) {
  
  measureOrder <- strsplit(tracker$measureOrder[tracker$participantID == id], split = ", ")[[1]]
  
  questionOrder <- c()
  
  for(measure in measureOrder) {
    
    if(measure == measureOrder[1]) {
      
      questionOrder <- lookup$varName.fixed[lookup$groupName.fixed == measure]
      
    } else {
      
      questionOrder <- c(questionOrder, lookup$varName.fixed[lookup$groupName.fixed == measure])
      
    }
    
  }
  
  for(question in questionOrder) {
    
    REDCap$qIndex[REDCap$participantID == id & REDCap$qID == question] <- which(questionOrder == question)
    
  }
  
}

REDCap <- dplyr::arrange(REDCap, participantID, qIndex)


