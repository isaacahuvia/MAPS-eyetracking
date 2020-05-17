##########################
##  Add Direct Indices  ##
##########################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse", "RCurl")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")

df <- readRDS(filenames$redcap$with_new_vars)



####  Add Direct Indices  ####
df %<>%
  mutate(
    #Instructed items
    instructed = (instructedItem.sometimes == F) + 
      (instructedItem.nearlyAlways == F),
    #Recall items
    recall = (recall.lowerGrade == F) + 
      (recall.raceDiscrimination == F),
    #Rum raisin scale
    rumRaisin.raw = (rumRaisin.q1 >= 4) +
      (rumRaisin.q2 == 5) +
      (rumRaisin.q3 >= 7) +
      (rumRaisin.q4 >= 8) +
      (rumRaisin.q5 == 3),
    rumRaisin = if_else(rumRaisin.raw > 2, as.integer(2), rumRaisin.raw),
    #Effort items
    effort = (effort.q1 <= 2) +
      (effort.q2 <= 2),
    #Overall direct item indicator (0-8)
    direct = instructed + rumRaisin + recall + effort
  )



####  Save Data  ####
saveRDS(df, file = filenames$redcap$with_direct)
