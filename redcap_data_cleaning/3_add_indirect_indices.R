############################
##  Add Indirect Indices  ##
############################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse", "RCurl")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")

load(file = filenames$redcap$with_direct)



####  Add Indirect Indices  ####
#TBD



####  Save Data  ####
save(df, file = filenames$redcap$with_indirect)
