setwd("/Users/yyc/D盘/日本留学/慶應義塾大学/Academics/2021 Fall/41248修士研究会/PerceptualData/PerceptualData20220605")

install.packages(c("tidyr", "ggplot2"))
install.packages("ggpubr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(tidyr)
library(ggplot2)
library(ggpubr)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$melody, paired = TRUE, alternative="less") #updated to make one-tailed in accordance with our a priori predictions
t.test(accuracy_byParti$melody, accuracy_byParti$lyrics, paired = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$lyrics, paired = TRUE)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
subset_musician1 <- subset(accuracy_byParti, MusicExperience1==1)
subset_nonmusician1 <- subset(accuracy_byParti, MusicExperience1==0)
subset_musician2 <- subset(accuracy_byParti, MusicExperience2==1)
subset_nonmusician2 <- subset(accuracy_byParti, MusicExperience2==0)
t.test(subset_musician1$full, subset_nonmusician1$full, paired = FALSE, alternative="greater")
t.test(subset_musician2$melody, subset_nonmusician2$melody, paired = FALSE, alternative="greater")
t.test(subset_musician2$lyrics, subset_nonmusician2$lyrics, paired = FALSE, alternative="greater")



## The following code of function summarySE() is extracted from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



#########################################################################
# Violin plot for accuracy by participant
# Basic violin plot
accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
data_long <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)

# combine
data_mix <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)
data_mix$Condition <- relevel(relevel(relevel(data_mix$Condition,"lyrics"),"melody"),"full")
subset_nonrandom <- subset(data_mix, Condition=="full" | (Condition=="melody" & (MusicExperience2==1 | MusicExperience2==0)) | (Condition=="lyrics" & (MusicExperience2==1 | MusicExperience2==0)))
subset_nonrandom$Condition <- relevel(relevel(relevel(subset_nonrandom$Condition,"lyrics"),"melody"),"full")
summarySE_nonrandom <- summarySE(subset_nonrandom, measurevar = "Accuracy", groupvars = "Condition")
subset_fa <- subset(subset_nonrandom, Condition=="full")
subset_molo <- subset(subset_nonrandom, Condition=="melody" | Condition=="lyrics")
subset_molo$Condition <- relevel(relevel(subset_molo$Condition,"lyrics"),"melody")

mix <- ggplot(data = subset_nonrandom, mapping = aes(x = Condition, y = Accuracy)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_point(color = "white", size = 0.1) + 
  geom_violin(data = subset_nonrandom, 
              mapping = aes(x = Condition, y = Accuracy, color = Condition), 
              fill = "white") + 
  geom_dotplot(data = subset_fa, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(MusicExperience1)), 
               position = position_dodge(width=1.1), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 0.9) + 
  geom_dotplot(data = subset_molo, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(MusicExperience2)), 
               position = position_dodge(width=1.1), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 0.9) + 
  geom_errorbar(data = summarySE_nonrandom, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(shape = "mean"), 
               geom = "point", color = "yellow", fill = "purple", alpha = 0.8, size = 4) + 
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "melody" = "Melody-only", 
                              "lyrics" = "Lyrics-only")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  scale_fill_manual(name = "Music Experience", 
                    values = c("0" = "green", "1" = "blue"), 
                    labels = c("0" = "Non-musician", "1" = "Musician")) + 
  scale_shape_manual(name = "", values = c("mean" = 23), 
                     labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), 
        axis.text.x = element_text(angle = 25, size = 15, hjust = 0.8), 
        axis.text.y = element_text(size = 15), 
        legend.title = element_text(size = 13), 
        legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, reverse = TRUE), 
         shape = guide_legend(order = 3)) + 
  labs(x = "Condition", y = "Matching Degree by Participant")



# Violin plot for accuracy by case
# Basic violin plot
accuracy_byCase <- read.csv("accuracy_by_case.csv", header = TRUE)
data_long <- gather(accuracy_byCase, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)

# Remove onlyLyricalSimi for melody & remove nonLyricalSimi for lyrics
subset_removeIrrelevantCases <- subset(data_long, !((Condition=="melody" & Case==6) | (Condition=="melody" & Case==11) | (Condition=="melody" & Case==13)
                                                    | (Condition=="lyrics" & Case==0) | (Condition=="lyrics" & Case==2)
                                                    | (Condition=="lyrics" & Case==8) | (Condition=="lyrics" & Case==14)
                                                    | (Condition=="lyrics" & Case==20) | (Condition=="lyrics" & Case==22)
                                                    | (Condition=="lyrics" & Case==24) | (Condition=="lyrics" & Case==25)
                                                    | (Condition=="lyrics" & Case==27) | (Condition=="lyrics" & Case==28)
                                                    | (Condition=="lyrics" & Case==29) | (Condition=="lyrics" & Case==30)
                                                    | (Condition=="lyrics" & Case==31) | (Condition=="lyrics" & Case==32)
                                                    | (Condition=="lyrics" & Case==33) | (Condition=="lyrics" & Case==35)
                                                    | (Condition=="lyrics" & Case==37) | (Condition=="lyrics" & Case==38)
                                                    | (Condition=="lyrics" & Case==39)))
# subset_removeInstrumentalForLyrics <- subset(data_long, !((Condition=="lyrics" & Case==8) | (Condition=="lyrics" & Case==14) | (Condition=="lyrics" & Case==25) | (Condition=="lyrics" & Case==29) | (Condition=="lyrics" & Case==31) | (Condition=="lyrics" & Case==33)))

subset_normal <- subset(subset_removeIrrelevantCases, Case!=37 & Case!=38 & Case!=39 & Case!=12 & Case!=15 & Case!=16)

subset_JP <- subset(data_long, Case>=37 & Condition!="lyrics")
subset_CN <- subset(data_long, Case==12 | Case==15 | Case==16)

subset_onlyLyricalSimi <- subset(data_long, Case==6 | Case==11 | Case==13)
subset_nonLyricalSimi <- subset(data_long, Case==0 | Case==2 | Case==8 | Case==14 | Case==20 | Case==22
                                | Case==24 | Case==25 | Case==27 | Case==28 | Case==29 | Case==30 | Case==31
                                | Case==32 | Case==33 | Case==35 | Case==37 | Case==38 | Case==39)
# subset_instrumental <- subset(data_long, Case==8 | Case==14 | Case==25 | Case==29 | Case==31 | Case==33)

summarySE_byCase <- summarySE(subset_removeIrrelevantCases, measurevar = "Accuracy", groupvars = "Condition")

p_byCase <- ggplot(data = subset_removeIrrelevantCases, mapping = aes(x = Condition, y = Accuracy, color = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_violin(fill = "white", bw = 12) + 
  geom_dotplot(data = subset_normal, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(CourtDecision)), 
               position = position_dodge(width=0.3), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 1) + 
  #  geom_dotplot(binwidth = 3, binaxis = "y", binpositions = "all", stackdir = "center", dotsize = 1, stackgroups = TRUE)
  geom_point(data = subset_JP, 
             mapping = aes(x = Condition, y = Accuracy, 
                           fill = factor(CourtDecision), group = factor(CourtDecision), 
                           shape = "rectangle"), #JP cases shown by rectangles
             position = position_jitterdodge(jitter.width = 0.55, dodge.width = 0.6, seed = 2), 
             #position = position_dodge(width=1), 
             color = "black", alpha = 0.5, size = 4, stroke = 1.5) + 
  geom_point(data = subset_CN, 
             mapping = aes(x = Condition, y = Accuracy, 
                           fill = factor(CourtDecision), group = factor(CourtDecision), 
                           shape = "triangle"), #CN cases shown by triangles
             #position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.3, seed = 2), 
             position = position_dodge(width=0.8), 
             color = "black", alpha = 0.5, size = 4, stroke = 1.5) + 
  geom_errorbar(data = summarySE_byCase, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(size = "mean"), 
               geom = "point", color = "yellow", fill = "purple", 
               shape = 23, alpha = 0.8) + #Mean values shown by diamond dots
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "melody" = "Melody-only", 
                              "lyrics" = "Lyrics-only")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  scale_fill_manual(name = "Court Decision", 
                    values = c("0" = "green", "1" = "red"), 
                    labels = c("0" = "No infringement", "1" = "Infringement")) + 
  scale_shape_manual(name = "Special Case", values = c("rectangle" = 22, "triangle" = 24), 
                     labels = c("rectangle" = "Case with \nJapanese language", 
                                "triangle" = "Case with \nChinese language")) + 
  scale_size_manual(name = "", values = c("mean" = 5), 
                    labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, override.aes = list(shape = NA)), 
         shape = guide_legend(order = 3), 
         size = guide_legend(order = 4)) + 
  labs(x = "Condition", y = "Matching Degree by Case")



#########################################################################
# Violin plot for accuracy by participant (Vocals-only & Accompaniment-only)
# Basic violin plot
accuracy_byParti <- read.csv("accuracy_by_participant_voao.csv", header = TRUE)
data_mix <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", vocals:accompaniment, factor_key = TRUE)
data_mix$Condition <- relevel(relevel(data_mix$Condition,"accompaniment"),"vocals")
summarySE_nonrandom <- summarySE(data_mix, measurevar = "Accuracy", groupvars = "Condition")

mix <- ggplot(data = data_mix, mapping = aes(x = Condition, y = Accuracy)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_point(color = "white", size = 0.1) + 
  geom_violin(data = data_mix, 
              mapping = aes(x = Condition, y = Accuracy, color = Condition), 
              fill = "white") + 
  geom_dotplot(data = data_mix, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(MusicExperience)), 
               position = position_dodge(width=1.1), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 0.9) + 
  geom_errorbar(data = summarySE_nonrandom, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(shape = "mean"), 
               geom = "point", color = "yellow", fill = "purple", alpha = 0.8, size = 4) + 
  scale_x_discrete(labels = c("vocals" = "Vocals-only", 
                              "accompaniment" = "Accompaniment-only")) + 
  scale_color_discrete(name = "Condition", labels = c("vocals" = "Vocals-only", 
                                                      "accompaniment" = "Accompaniment\n-only")) + 
  scale_fill_manual(name = "Music Experience", 
                    values = c("0" = "green", "1" = "blue"), 
                    labels = c("0" = "Non-musician", "1" = "Musician")) + 
  scale_shape_manual(name = "", values = c("mean" = 23), 
                     labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), 
        axis.text.x = element_text(angle = 15, size = 15, hjust = 1), 
        axis.text.y = element_text(size = 15), 
        legend.title = element_text(size = 13), 
        legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, reverse = TRUE), 
         shape = guide_legend(order = 3)) + 
  labs(x = "Condition", y = "Matching Degree by Participant")



# Violin plot for accuracy by case (Vocals-only & Accompaniment-only)
# Basic violin plot
accuracy_byCase <- read.csv("accuracy_by_case_voao.csv", header = TRUE)
data_long <- gather(accuracy_byCase, key = "Condition", value = "Accuracy", vocals:accompaniment, factor_key = TRUE)
subset_removeIrrelevantCases <- na.omit(data_long)
subset_normal <- subset(subset_removeIrrelevantCases, Case!=8 & Case!=14 & Case!=29)
subset_vocalsVsAccompaniment <- subset(subset_removeIrrelevantCases, Case==8 | Case==14 | Case==29)
summarySE_byCase <- summarySE(subset_removeIrrelevantCases, measurevar = "Accuracy", groupvars = "Condition")

p_byCase <- ggplot(data = subset_removeIrrelevantCases, mapping = aes(x = Condition, y = Accuracy, color = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_violin(fill = "white", bw = 12) + 
  geom_dotplot(data = subset_normal, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(CourtDecision)), 
               position = position_dodge(width=0.3), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 1) + 
  #  geom_dotplot(binwidth = 3, binaxis = "y", binpositions = "all", stackdir = "center", dotsize = 1, stackgroups = TRUE)
  geom_point(data = subset_vocalsVsAccompaniment, 
             mapping = aes(x = Condition, y = Accuracy, 
                           fill = factor(CourtDecision), group = factor(CourtDecision), 
                           shape = "triangle"), #Vocals vs Accompaniment cases shown by triangles
             #position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.3, seed = 2), 
             position = position_dodge(width=0.8), 
             color = "black", alpha = 0.5, size = 4, stroke = 1.5) + 
  geom_errorbar(data = summarySE_byCase, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(size = "mean"), 
               geom = "point", color = "yellow", fill = "purple", 
               shape = 23, alpha = 0.8) + #Mean values shown by diamond dots
  scale_x_discrete(labels = c("vocals" = "Vocals-only", 
                              "accompaniment" = "Accompaniment-only")) + 
  scale_color_discrete(name = "Condition", labels = c("vocals" = "Vocals-only", 
                                                      "accompaniment" = "Accompaniment\n-only")) + 
  scale_fill_manual(name = "Court Decision", 
                    values = c("0" = "green", "1" = "red"), 
                    labels = c("0" = "No infringement", "1" = "Infringement")) + 
  scale_shape_manual(name = "Special Case", values = c("triangle" = 24), 
                     labels = c("triangle" = "Cases with 1\n instrumental\n work in each\n (i.e. vocals vs.\n accompaniment\n comparisons)")) + 
  scale_size_manual(name = "", values = c("mean" = 5), 
                    labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, override.aes = list(shape = NA)), 
         shape = guide_legend(order = 3), 
         size = guide_legend(order = 4)) + 
  labs(x = "Condition", y = "Matching Degree by Case")



#########################################################################
# Perceptual similarity vs PMI
similarity <- read.csv("perceptual_simi_vs_pmi_long.csv", header = TRUE)
similarity$GROUP <- relevel(relevel(relevel(factor(similarity$GROUP),"LYRICS_SIMI"),"MELODY_SIMI"),"FULL_SIMI")
GROUP.labs <- c("Full-audio", "Melody-only", "Lyrics-only")
names(GROUP.labs) <- c("FULL_SIMI", "MELODY_SIMI", "LYRICS_SIMI")

pmi_p <- ggplot(data = similarity, mapping = aes(x = SIMILARITY, y = PMI)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5), limits = c(1,5)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(mapping = aes(color = GROUP)) + 
  geom_smooth(method = "lm") + 
  facet_grid(GROUP~., labeller = labeller(GROUP = GROUP.labs)) + 
  scale_color_discrete(name = "Condition", labels = c("FULL_SIMI" = "Full-audio", 
                                                      "MELODY_SIMI" = "Melody-only", 
                                                      "LYRICS_SIMI" = "Lyrics-only")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13), 
        strip.text.y = element_text(size = 13)) + 
  labs(x = "Mean Perceptual Similarity", y = "Automatically Calculated Melodic Similarity (PMI)") + 
  #  stat_regline_equation(label.x = 4, label.y = 15) + 
  stat_cor(label.x = 3, label.y = 5, size = 5)



# Perceptual similarity vs Musly similarity
perceptual_musly <- read.csv("perceptual_simi_vs_musly_simi_long.csv", header = TRUE)
perceptual_musly$GROUP <- relevel(relevel(relevel(factor(perceptual_musly$GROUP),"LYRICS_SIMI"),"MELODY_SIMI"),"FULL_SIMI")
GROUP.labs <- c("Full-audio", "Melody-only", "Lyrics-only")
names(GROUP.labs) <- c("FULL_SIMI", "MELODY_SIMI", "LYRICS_SIMI")

musly_p <- ggplot(data = perceptual_musly, mapping = aes(x = SIMILARITY, y = Musly_simi)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5), limits = c(1,5)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(mapping = aes(color = GROUP)) + 
  geom_smooth(method = "lm") + 
  facet_grid(GROUP~., labeller = labeller(GROUP = GROUP.labs)) + 
  scale_color_discrete(name = "Condition", labels = c("FULL_SIMI" = "Full-audio", 
                                                      "MELODY_SIMI" = "Melody-only", 
                                                      "LYRICS_SIMI" = "Lyrics-only")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13), 
        strip.text.y = element_text(size = 13)) + 
  labs(x = "Mean Perceptual Similarity", y = "Automatically Calculated Audio Similarity (Musly)") + 
  #  stat_regline_equation(label.x = 0, label.y = 98) + 
  stat_cor(label.x = 3.2, label.y = 90, size = 5)
