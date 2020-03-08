########################
##  Data Preparation  ##
########################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")



####  Data Preparation  ####
## Load clean data
# These data include (a) summary-level Tobii data, (b) data from REDCap measures, and (c) archival self-report quality metrics, already calculated
# Isaac Ahuvia prepared this dataset in November 2019 while at the Developmental Mechanisms Lab, for use in this research
df <- read.csv(filenames$clean_data$summary,
               stringsAsFactors = F)


## Calculate direct self-report quality metrics
df %<>%
  mutate(
    #Instructed items - a binary variable indicating the participant got at least one instructed item wrong
    instructed = instructedItem.sometimes == F | 
                 instructedItem.nearlyAlways == F,
    #Recall items - a count of how many recall items the participant got wrong
    #Note: This was a high bar to clear, so we may not use it in the summary indicator
    recall = (recall.lowerGrade == F) + 
             (recall.raceDiscrimination == F),
    #Rum raisin scale - a count of how many items on the rum raisin flagged as invalid
    rumRaisin = (rumRaisin.q1 >= 4) +
                (rumRaisin.q2 == 5) +
                (rumRaisin.q3 >= 7) +
                (rumRaisin.q4 >= 8) +
                (rumRaisin.q5 == 3),
    #Effort items - recode both individually to be binary variables indicating the participant reported low effort
    effort.q1 = effort.q1 <= 2,
    effort.q2 = effort.q2 <= 2,
    #Overall direct item indicator. Not including recall items - see note above
    direct = instructed + rumRaisin + effort.q1 + effort.q2
  )



####  Save Data  ####
save(df, file = filenames$clean_data$with_direct_indicators)
