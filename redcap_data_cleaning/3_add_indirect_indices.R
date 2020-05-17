############################
##  Add Indirect Indices  ##
############################

####  Startup  ####
library(easypackages)
libraries("yaml", "magrittr", "tidyverse", "RCurl", "conflicted")
conflict_prefer("select", "dplyr")

rm(list = ls())

filenames <- yaml::read_yaml("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\filenames.yaml")

df <- readRDS(filenames$redcap$with_direct)

#Load analysis functions
source("C:\\Users\\isaac\\Box\\MAPS - ECHO Tobii Analysis\\MAPS-eyetracking\\redcap_data_cleaning\\archival_indicators_functions.R")


####  Add Indirect Indices  ####
#Establish scale lookup
scaleLookup <- data.frame(
  variable = names(df)[grepl("PROMIS", names(df))]
) %>%
  mutate(scale = gsub("^(.*?)_|_(.*?)$", "", variable)) %>%
  select(scale, variable)

#Establish reversed question lookup
reversedItems <- names(df)[grepl("reverse", names(df))]

#Establish columns
cols <- which(grepl("PROMIS", names(df)))
colsNoReverse <- setdiff(cols, which(names(df) %in% reversedItems))

#Use functions
df$evenOdd <- evenOdd(df, scaleLookup = scaleLookup, columns = cols)
df$interItemSD <- interItemSD(df, scaleLookup = scaleLookup, columns = cols)
df$longstring <- longstring(df, columns = cols)
for(i in sort(unique(as.vector(as.matrix(df[,cols]))))) {
  df[[paste0("longstring_", i)]] <- longstring(df, value = i, columns = cols)
}
df$mahalanobisDist <- mahalanobisDist(df, columns = colsNoReverse)
df$omittedItems <- omittedItems(df, columns = cols)
df$personTotalCor <- personTotalCor(df, columns = cols)
df$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = 5, columns = cols, scaleLookup = scaleLookup)
df$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = 5, norm = T, columns = colsNoReverse)
df$psychSyn <- psychSyn(df, critval = .5, columns = cols)
df$resampledConsistency <- resampledConsistency(df, scaleLookup = scaleLookup, columns = cols, iterations = 10)
df$reversedItemDifference <- reversedItemDifference(df, scaleLookup = scaleLookup, reversedItems = reversedItems, columns = cols)
df$u3 <- u3(df, nCategories = 5, columns = colsNoReverse)
df$zScore <- zScore(df, columns = cols)

#See how many missing values we have here - should be 4 or 5 for most
df %>%
  select(evenOdd:zScore) %>%
  pivot_longer(evenOdd:zScore) %>%
  group_by(name) %>%
  summarize(missing = sum(is.na(value)))



####  Save Data  ####
saveRDS(df, file = filenames$redcap$with_indirect)
