########################
##  Data Preparation  ##
########################

# For now this file is a placeholder. In the final workflow, this file will combine clean REDCap and clean Tobii data.
# For now, we haven't needed to do any new Tobii analysis to create new variables. So, all of the variables we need
# are already in the REDCap dataset. So, this just loads and saves the data. In the future, we'll use this to merge in
# new variables, too.

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")

load(file = filenames$redcap$with_indirect)


####  Merge Data  ####
# TBD



####  Save Data  ####
save(df, file = filenames$analysis_ready)
