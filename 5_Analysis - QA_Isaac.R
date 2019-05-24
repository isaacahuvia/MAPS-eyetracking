###########################
##  Eyetracking Data #5  ##
##     Analysis - QA     ##
###########################


rm(list = ls())
library(yaml)
library(eyetrackingR)
library(dplyr)
library(tidyr)
library(readr)

lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
## R
load(file = lookup$pc$clean_data$analysis_ready)

## Tobii
df.T1 <- readr::read_tsv("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Data\\Eyetracking\\Tobii Outputs\\Table 1.txt")
df.T2 <- readr::read_tsv("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Data\\Eyetracking\\Tobii Outputs\\Table 2.txt")
df.T3 <- readr::read_tsv("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Data\\Eyetracking\\Tobii Outputs\\Table 3.txt")



####  Clean Data  ####
## R
df.R <- df %>%
  dplyr::mutate(AOI = case_when(qText == T ~ "qText",
                                c1 == T ~ "c1",
                                c2 == T ~ "c2",
                                c3 == T ~ "c3",
                                c4 == T ~ "c4",
                                c5 == T ~ "c5",
                                c6 == T ~ "c6",
                                submit == T ~ "submit",
                                T ~ NA_character_)) %>%
  dplyr::filter(!is.na(AOI),
                trackLoss == F) %>%
  dplyr::group_by(participantID, qID, AOI) %>%
  dplyr::summarise(gazeTime = sum(time.sec)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(qID_AOI = paste0(as.character(qID), "_", AOI)) %>%
  dplyr::select(-qID, -AOI) %>%
  tidyr::spread(qID_AOI, gazeTime)

#Clean names
names(df.R) <- gsub(" ", "_", names(df.R))


## Tobii
#Remove empty columns from each recording (they each output all questions, but only data on 1/3 of questions)
df.T1 <- df.T1[,apply(df.T1,2,function(x) !all(x=="-"))] 
df.T2 <- df.T2[,apply(df.T2,2,function(x) !all(x=="-"))] 
df.T3 <- df.T3[,apply(df.T3,2,function(x) !all(x=="-"))] 

#Confirm rows are arranged in the same way, then cbind
if(all(df.T1$X1 == df.T2$X1)) df.T <- cbind(df.T1, df.T2[,-1])
if(all(df.T$X1 == df.T3$X1)) df.T <- cbind(df.T, df.T3[,-1])

#Remove cases not in R data
df.T <- df.T[df.T$X1 %in% as.character(df.R$participantID),]

#Make variables numeric
df.T[,2:length(df.T)] <- sapply(df.T[,2:length(df.T)], as.numeric)

#Clean names
df.T <- df.T[,names(df.T) %in% c("X1", names(df.T[grepl("Sum$", names(df.T))]))]
names(df.T)[1] <- "participantID"
names(df.T)[2:length(df.T)] <- gsub("^.*Duration_", "", names(df.T)[2:length(df.T)])
names(df.T)[2:length(df.T)] <- gsub("_Sum", "", names(df.T)[2:length(df.T)])
names(df.T) <- gsub(" ", "_", names(df.T))


## Align variable names
names(df.R)[!names(df.R) %in% names(df.T)]
names(df.T)[!names(df.T) %in% names(df.R)]

#Fix misnamed AOIs in Tobii output
names(df.T) <- gsub("qtext", "qText", names(df.T))
names(df.T) <- gsub("instructions", "qText", names(df.T))
names(df.T)[names(df.T) == "c6mapdb13_c 3"] <- "c6mapdb13_c3"
names(df.T)[names(df.T) == "c6mapdb24_c 3"] <- "c6mapdb24_c3"
names(df.T)[names(df.T) == "PROMIS PSE Q1_qText 2"] <- "PROMIS PSE Q1_qText"

#There are a couple of variables that are in one dataset but don't show up in the other - for now, remove these
df.R <- df.R[,names(df.R) %in% names(df.T)]
df.T <- df.T[,names(df.T) %in% names(df.R)]

#Order columns alphabetically
df.R <- df.R %>%
  select(-participantID) %>%
  select(sort(names(.)))
df.T <- df.T %>%
  select(-participantID) %>%
  select(sort(names(.)))



####  QA  ####
for(i in 1:nrow(df.T)) {
  print(paste0("Row: ", i))
  t <- as.numeric(df.T[i,])
  r <- as.numeric(df.R[i,])
  t[is.na(t)] <- 0
  r[is.na(r)] <- 0
  
  #Test
  r[r<.05] <- NA
  
  cor <- cor(t, r, use = "pairwise.complete.obs")
  print(paste0("Correlation: ", round(cor, 2)))
  print(paste0("Mean for Tobii data: ", round(mean(t, na.rm = T), 2)))
  print(paste0("Proportion of Tobii data greater than R data: ", round(mean(t > r, na.rm = T), 2)))
  print(paste0("Mean of Tobii data minus R data: ", round(mean(t - r, na.rm = T), 2)))
  print(paste0("Mean difference between Tobii data and R data: ", round(mean(abs(t - r), na.rm = T), 2)))
  print(paste0("Proportion of Tobii data vs R data: ", round(sum(t, na.rm = T) / sum(r, na.rm = T), 2)))
  print("---------------------------------------------------")
}


## Takeaways
#Tobii generally captures more gazetime
#Tobii and R measures are very highly correlated
#R captures many more gazetimes of <.05 seconds, whereas Tobii captures longer gazetimes


####  Output  ####
write.csv(df.R, file = lookup$pc$outputs$R, row.names = F)
write.csv(df.T, file = lookup$pc$outputs$Tobii, row.names = F)