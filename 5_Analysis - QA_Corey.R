###########################
##  Eyetracking Data #5  ##
##     Analysis - QA     ##
###########################

rm(list = ls())
library(readr)
library(yaml)
library(eyetrackingR)
library(dplyr)
library(tidyr)
library(stringr)
library(utils)
lookup <- yaml::read_yaml("/Volumes/fsmresfiles/MSS/Research/Projects/MAPS-FUS/Tween Wave/Survey Visit (ECHO)/Code/Eyetracking/Eyetracking Lookup.yaml")

####  Read Data  ####
load(file = lookup$mac$clean_data$analysis_ready)

####  QA  ####
#Calculate total amount of time spent looking at an AOI, for each AOI, question, and participant
df.r <- df %>%
  dplyr::mutate(AOI = case_when(qText == T ~ "qText",
                                c1 == T ~ "c1",
                                c2 == T ~ "c2",
                                c3 == T ~ "c3",
                                c4 == T ~ "c4",
                                c5 == T ~ "c5",
                                c6 == T ~ "c6",
                                submit == T ~ "submit",
                                T ~ NA_character_)) %>%
  dplyr::filter(!is.na(AOI)) %>%
  dplyr::group_by(participantID, qID, AOI) %>%
  dplyr::summarise(gazeTime = sum(time.sec)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(qID_AOI = paste0(as.character(qID), "_", AOI)) %>%
  dplyr::select(-qID, -AOI) %>%
  tidyr::spread(qID_AOI, gazeTime)

#Delete unneccesary columns
df.r<-df.r[colSums(!is.na(df.r)) > 0]

###########################
###Tobii output cleaning###
###########################

##load excel data##
df.b1<- readr::read_tsv("/Volumes/fsmresfiles/MSS/Research/Projects/MAPS-FUS/Tween Wave/Survey Visit (ECHO)/Data/Eyetracking/Tobii Outputs/Table 1.txt")
df.b2<- readr::read_tsv("/Volumes/fsmresfiles/MSS/Research/Projects/MAPS-FUS/Tween Wave/Survey Visit (ECHO)/Data/Eyetracking/Tobii Outputs/Table 2.txt")
df.b3<- readr::read_tsv("/Volumes/fsmresfiles/MSS/Research/Projects/MAPS-FUS/Tween Wave/Survey Visit (ECHO)/Data/Eyetracking/Tobii Outputs/Table 3.txt")


####################
##clean excel data##
####################

#removing empty columns
df.b1<- df.b1[!sapply(df.b1, function(x) all(x == "-"))]
df.b2<- df.b2[!sapply(df.b2, function(x) all(x == "-"))]
df.b3<- df.b3[!sapply(df.b3, function(x) all(x == "-"))]

#Delete unneccesary columns#
#df.m<- df.m[!sapply(df.m, function(x) all(x == "-"))]

#merge data frames together#
df.m<- merge(df.b1, df.b2, by = 'X1')
df.m<- merge(df.m, df.b3, by = 'X1')

#Remove unncessary pre-merged data frames
rm(df.b1, df.b2, df.b3)

#Remove uneccesary columns
df.m <- df.m[, -grep("_N", colnames(df.m))]
df.m <- df.m[, -grep("_Sum", colnames(df.m))]

#Rename mislabled AOI's
names(df.m) <- gsub("c6mapdb13_c 3", "c6mapdb13_c3", names(df.m))
names(df.m) <- gsub("c6mapdb24_c 3", "c6mapdb24_c3", names(df.m))
names(df.m) <- gsub("PROMIS PSE Q1_qText 2", "PROMIS PSE Q1_qText", names(df.m))
names(df.m) <- gsub("qtext", "qText", names(df.m))
names(df.m)[which(names(df.m) == "Participant ID")] <- "participantID"
names(df.m)[which(names(df.m) == "c6mapdbintro3_instructions")] <- "c6mapdbintro3_qText"

#Rename Participant ID column
names(df.m) <- gsub("X1", "participantID", names(df.m))

#Remove unneeded text from columns 
names(df.m) <- gsub("Total", "", names(df.m))
names(df.m) <- gsub("Fixation Duration_", "", names(df.m))
names(df.m) <- gsub("_Mean", "", names(df.m))
names(df.m) <- trimws(names(df.m))

#Delete unneccesary rows (entitled 3248, All Recordings)
df.m <- df.m[-c(1,7), ] 

#make "-" into NA values
df.m <- df.m %>% dplyr::na_if("-")

#Reorder columns alphabetically
df.m<- df.m[,order(colnames(df.m))]
df.r<- df.r[,order(colnames(df.r))]

##compare datasets
RnotM <- dplyr::setdiff(names(df.r), names(df.m))
MnotR <- dplyr::setdiff(names(df.m), names(df.r))

RnotM
MnotR

#Drop the columns that are not the same#
#make a vector of the columns shared between each
cols_to_keep <- intersect(colnames(df.m),colnames(df.r))

#Drop the unmatching columns in each data frame 
df.r <- df.r[,cols_to_keep, drop=FALSE]
df.m <- df.m[,cols_to_keep, drop=FALSE]

#make numeric df.m
df.m<- data.matrix(df.m)
df.m<-as.data.frame(df.m)

#make numeric df.r
df.r<- data.matrix(df.r)
df.r<-as.data.frame(df.r)


####################
##  Analyze data  ##
####################

#Participant means 
pm.m<- rowMeans(df.m, na.rm=TRUE)
pm.r<- rowMeans(df.r, na.rm=TRUE)

#pm.m<-pm.m/100 #looks like these are about 100 times larger?
cor(pm.r, pm.m, use="complete.obs")
Analysis.frame<- data.frame(pm.r,pm.m)

#AOI means 
aoim.m<- colMeans(df.m, na.rm=TRUE)
aoim.r<- colMeans(df.r, na.rm=TRUE)

#Cehck to see what it looks like
head(aoim.m)
head(aoim.r)

#Make an AOI dataframe
aoi.df<-as.data.frame(aoim.r, aoim.m)
cor(c(aoim.r), c(aoim.m), use="complete.obs")

##summary stats
