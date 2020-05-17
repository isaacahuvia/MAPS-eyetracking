########################
##  Data Preparation  ##
########################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")

df <- readRDS(filenames$redcap$with_indirect)



####  Create New Variables  ####
df %<>%
  mutate(birthdate = as.Date(birthdate, format = "%Y-%m-%d"),
         ECHOVisitDate = as.Date(ECHOVisitDate, format = "%Y-%m-%d"),
         age = as.numeric((ECHOVisitDate - birthdate) / 365.25),
         tobii.avgQTextFixationTime_quant = cut(df$tobii.avgQTextFixationTime, breaks = quantile(df$tobii.avgQTextFixationTime, na.rm = T)),
         tobii.avgQuestionDuration_quant = cut(df$tobii.avgQuestionDuration, breaks = quantile(df$tobii.avgQuestionDuration, na.rm = T)),
         #Calculate NEPSY total scores
         nepsy.duration = nepsy.1.duration + nepsy.2.duration + nepsy.3.duration + nepsy.4.duration,
         nepsy.selfCorrectedErrors = nepsy.1.selfCorrectedErrors + nepsy.2.selfCorrectedErrors + nepsy.3.selfCorrectedErrors + nepsy.4.selfCorrectedErrors,
         nepsy.uncorrectedErrors = nepsy.1.uncorrectedErrors + nepsy.2.uncorrectedErrors + nepsy.3.uncorrectedErrors + nepsy.4.uncorrectedErrors,
         nepsy.totalErrors = nepsy.1.totalErrors + nepsy.2.totalErrors + nepsy.3.totalErrors + nepsy.4.totalErrors,
         #Calculate rough infrequent symptom scale from APSS-CAPE
         infreqSymptoms = ((apss_cape.q1_frequency > 2) +
                           (apss_cape.q2_frequency > 2) +
                           (apss_cape.q3_frequency > 2) +
                           (apss_cape.q4_frequency > 2) +
                           (apss_cape.q5_frequency > 2) +
                           (apss_cape.q6_frequency > 2) +
                           (apss_cape.q7_frequency > 2))) %>%
  rowwise() %>%
  mutate(readingLevelMean = mean(wj.letterWordID.rawScore, wj.passageCompletion.rawScore, wj.readingFluency.rawScore, na.rm = T)) %>%
  ungroup()



####  Merge in Tobii Data  ####
tobii <- readRDS(filenames$tobii$clean_data$participant_level) %>%
  mutate(participantID = as.numeric(participantID))

df %<>%
  left_join(tobii, by = "participantID") 



####  Save Data  ####
saveRDS(df, file = filenames$analysis_ready)
