#############################
##   Eyetracking Data #4   ##
##  Make Eyetracking Data  ##
#############################


rm(list = ls())
library(yaml)
library(eyetrackingR)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
load(file = lookup$pc$clean_data$with_AOIs)



####  Transform into Eyetracking Data  ####
#Create a time column in seconds (current timestamp is in microseconds)
df$timestamp.sec <- df$timestamp / 1000000
df$time.sec <- df$timestamp.sec - lag(df$timestamp.sec)
df$time.sec[df$time.sec < 0 | df$time.sec > .01] <- NA

#See http://www.eyetracking-r.com/vignettes/preparing_your_data
df <- eyetrackingR::make_eyetrackingr_data(df,
                                           participant_column = "participantID",
                                           trial_column = "qID",
                                           time_column = "timestamp.sec",
                                           trackloss_column = "trackLoss",
                                           aoi_columns = c("qText", "c1", "c2", "c3", "c4", "c5", "submit"),
                                           treat_non_aoi_looks_as_missing = F)



####  Output Data  ####
save(df, file = lookup$pc$clean_data$analysis_ready)