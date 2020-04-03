###########################
##  Eyetracking Data #2  ##
##        Add qID        ##
###########################

rm(list = ls())
library(yaml)
library(openxlsx)
library(eyetrackingR)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
load(file = lookup$pc$clean_data$merged)
AOIs <- openxlsx::read.xlsx(lookup$pc$aoi_list)
qID_lookup <- openxlsx::read.xlsx(lookup$pc$qID_lookup)



####  Create qID Column  ####
df$qID <- NA

#Exclude these rows from qID_lookup as they will be assigned manually
qID_lookup <- qID_lookup[!grepl("In the past 7 days, I got tired easily", qID_lookup$qText),]
qID_lookup <- qID_lookup[!grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", qID_lookup$qText),]

for(i in 1:nrow(qID_lookup)) {
  
  df$qID[df$qText == qID_lookup$qText[i]] <- qID_lookup$qID[i]
    
}

df$qID[grepl("In the past 7 days, I got tired easily", df$qText) & df$test == "Test 1of3"] <- "PROMIS GH Q8"
df$qID[grepl("In the past 7 days, I got tired easily", df$qText) & df$test == "Test 3of3"] <- "PROMIS F Q4"

df$qID[grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 1of3"] <- "c6mapdbintro1"
df$qID[grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 2of3"] <- "c6mapdbintro2"
df$qID[grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 3of3"] <- "c6mapdbintro3"



####  Output Data  ####
save(df, file = lookup$pc$clean_data$with_qIDs)