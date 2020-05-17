###########################
##  Eyetracking Data #2  ##
##        Add qID        ##
###########################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("yaml", "openxlsx", "zoo", "conflicted", "tidyverse")

lookup <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")



####  Read Data  ####
df <- readRDS(lookup$tobii$working_data$merged)
AOIs <- openxlsx::read.xlsx(lookup$tobii$aoi_list)
qID_lookup <- openxlsx::read.xlsx(lookup$tobii$qID_lookup)



####  Create qID Column  ####
#Exclude these rows from qID_lookup as they will be assigned manually
qID_lookup <- qID_lookup[!grepl("In the past 7 days, I got tired easily", qID_lookup$qText),]
qID_lookup <- qID_lookup[!grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", qID_lookup$qText),]

df %<>%
  #Join with lookup to add question ID
  left_join(qID_lookup %>%
              select(qID, qText),
            by = "qText") %>%
  #Manually edit question ID in cases where the question text is duplicated across qIDs
  mutate(qID = case_when(
    
    grepl("In the past 7 days, I got tired easily", df$qText) & df$test == "Test 1of3" ~ "PROMIS GH Q8",
    grepl("In the past 7 days, I got tired easily", df$qText) & df$test == "Test 3of3" ~ "PROMIS F Q4",
    grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 1of3" ~ "c6mapdbintro1",
    grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 2of3" ~ "c6mapdbintro2",
    grepl("Now we are going to ask about things that you may sometimes feel, think, or do.", df$qText) & df$test == "Test 3of3" ~ "c6mapdbintro3",
    T ~ qID
    
  ))



####  Output Data  ####
saveRDS(df, file = lookup$tobii$working_data$with_qIDs)
