########################
##  Load REDCap Data  ##
########################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse", "RCurl")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")



####  Data Preparation  ####
## Load clean data
# These data include (a) summary-level Tobii data, (b) data from REDCap measures, and (c) archival self-report quality metrics, already calculated
# Isaac Ahuvia prepared this dataset in November 2019 while at the Developmental Mechanisms Lab, for use in this research
df <- read.csv(filenames$redcap$exported,
               stringsAsFactors = F)


## Add new variables via API call to REDCap. This is for variables we forgot to include the first time!
new <- read.csv(stringsAsFactors = F,
                text = postForm(
  uri='https://redcap.nubic.northwestern.edu/redcap/api/',
  token='3846A5246E237CC464EA1C4B5CAE1D21',
  content='record',
  format='csv',
  type='flat',
  'fields[0]'='adi_timestamp',
  'fields[1]'='studyid',
  'forms[0]'='wj_entry_tween_wave',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
))

new %<>%
  filter(wj_entry_tween_wave_complete == 2) %>%
  mutate(participantID = as.numeric(studyid)) %>%
  select(participantID,
         wj.letterWordID.rawScore = t6wjlwraw,
         wj.passageCompletion.rawScore = t6wjpcraw,
         wj.readingFluency.rawScore = t6wjrfraw)


## Join the two datasets together to add on the variables we missed the first time
df <- left_join(df, new, by = "participantID")



####  Save Data  ####
saveRDS(df, file = filenames$redcap$with_new_vars)
