###########################
##  Eyetracking Data #3  ##
##       Add AOIs        ##
###########################

#Note: Gaze point (ADCSpx) is pixels from upper left towards lower right corner of screen (1920x1080). See https://connect.tobiipro.com/s/case/5001B00001H7qWX/adcspx-gaze-point-coordinates 

rm(list = ls())
library(yaml)
library(openxlsx)
library(eyetrackingR)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
AOIs <- openxlsx::read.xlsx(lookup$pc$aoi_list)
load(file = lookup$pc$clean_data$with_qIDs)



####  Update AOIs  ####
#Enlargen AOIs by 100 pixels on each side, and 100 pixels upwards for qText, to account for skew in raw data
AOIs$xMin <- AOIs$xMin - 100
AOIs$xMax <- AOIs$xMax + 100
AOIs$yMin[AOIs$AOI == "qText"] <- AOIs$yMin[AOIs$AOI == "qText"] - 100


####  Add AOIs  ####
for(AOI in unique(AOIs$AOI)) {
  
  print(AOI)
  
  temp <- AOIs[AOIs$AOI == AOI,]
  
  df <- eyetrackingR::add_aoi(data = df,
                              aoi_dataframe = temp,
                              aoi_name = AOI,
                              x_col = "gazeAvgX", y_col = "gazeAvgY",
                              x_min_col = "xMin", x_max_col = "xMax", y_min_col = "yMin", y_max_col = "yMax")
}



####  Output Data  ####
save(df, file = lookup$pc$clean_data$with_AOIs)