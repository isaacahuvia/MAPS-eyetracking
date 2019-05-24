###########################
##  Eyetracking Data #5  ##
##   Analysis - Ad Hoc   ##
###########################


rm(list = ls())
library(yaml)
library(eyetrackingR)
library(ggplot2)
library(dplyr)
lookup <- yaml::read_yaml("R:\\MSS\\Research\\Projects\\MAPS-FUS\\Tween Wave\\Survey Visit (ECHO)\\Code\\Eyetracking\\Eyetracking Lookup.yaml")



####  Read Data  ####
load(file = lookup$pc$clean_data$analysis_ready)



####  Analysis  ####
## Question duration density plot
df %>%
  dplyr::group_by(participantID, qID) %>%
  dplyr::filter(time.sec == max(time.sec, na.rm = T)) %>%
  ggplot() +
    geom_density(aes(time.sec), adjust = 1) +
    xlim(c(0,30)) + xlab("Question Duration (Seconds)") +
    ylim(c(0, .2)) + ylab("Density") +
    theme_classic()


## Timecourse for qText only - see http://www.eyetracking-r.com/vignettes/growth_curve_analysis
dfResponseTime <- make_time_sequence_data(df,
                                          time_bin_size = .1,
                                          aois = c("qText"))

plot(dfResponseTime) + 
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 5)) +
  ylab("Proportion of Gazes: Question Text") +
  theme_classic()


## Response and fixation times by respondent
dur <- df %>%
  dplyr::filter(trackLoss == F) %>%
  dplyr::group_by(participantID) %>%
  dplyr::summarise(dur.qText = (sum(qText == T, na.rm = T) * 0.008333) / length(unique(qID)), #duration *in seconds* *per question.* .008 figure based on mean(df$time.sec - lag(df$time.sec, 1))
                   dur.c1 = (sum(c1 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.c2 = (sum(c2 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.c3 = (sum(c3 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.c4 = (sum(c4 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.c5 = (sum(c5 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.c6 = (sum(c6 == T, na.rm = T) * .008333) / length(unique(qID)),
                   dur.submit = (sum(submit == T, na.rm = T) * .008333) / length(unique(qID)))

ggplot(dur) +
  geom_density(aes(dur.qText)) +
  geom_point(aes(x = dur.qText, y = 0), size = 4) +
  xlim(0, 6) + ylim(0, 1) +
  xlab("Fixation Duration - Question Text") + ylab("Density") +
  theme_classic()

ggplot(dur) +
  geom_density(aes(dur.qText)) +
  geom_point(aes(x = dur.qText, y = 0), size = 4) +
  geom_text(aes(x = dur.qText, y = 0, label = ifelse(dur.qText == min(dur$dur.qText), as.character(participantID), "")), hjust = .5, vjust = -1, size = 6) +
  geom_text(aes(x = dur.qText, y = 0, label = ifelse(dur.qText > .9 & dur.qText < 1, as.character(participantID), "")), hjust = .5, vjust = 1.5, size = 6) +
  xlim(0, 6) + ylim(0, 1) +
  xlab("Fixation Duration - Question Text") + ylab("Density") +
  theme_classic()