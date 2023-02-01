
#load packages
library(ggplot2)
library(ROCR)
library(verification)

#load data
d<-read.csv("Yuan et al (2022) raw data.csv")



#create percent infringement
d$InfringementAudio<-ifelse(d$Court.Decision.1==1,d$Perceptual.Accuracy...Full.audio,1-d$Perceptual.Accuracy...Full.audio)
d$InfringementMelody<-ifelse(d$Court.Decision.1==1,d$Perceptual.Accuracy...Melody.only,1-d$Perceptual.Accuracy...Melody.only)
d$InfringementLyrics<-ifelse(d$Court.Decision.1==1,d$Perceptual.Accuracy...Lyrics.only,1-d$Perceptual.Accuracy...Lyrics.only)
#omit NAs for lyrics dataset
d.lyric.na<-na.omit(d)

#plot infringement: 
#vs PMI:
ggplot(d,aes(x=InfringementAudio,y=PMI,group=Court.Decision))+
       geom_point(aes(color=Court.Decision))
ggplot(d,aes(x=InfringementMelody,y=PMI,group=Court.Decision))+
       geom_point(aes(color=Court.Decision))
ggplot(d,aes(x=InfringementLyrics,y=PMI,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))

#vs Musly:
ggplot(d,aes(x=InfringementAudio,y=Musly,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))
ggplot(d,aes(x=InfringementMelody,y=Musly,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))
ggplot(d,aes(x=InfringementLyrics,y=Musly,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))

#perceptual full audio vs. automated PMI (best human/automated AUCs) - using infringement judgments
ggplot(d,aes(x=InfringementAudio,y=PMI,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))+
  geom_vline(xintercept = .43)+
  geom_hline(yintercept = 46)

#perceptual full audio vs. automated PMI (best human/automated AUCs) - using perceived similarity
ggplot(d,aes(x=AudioSimilarity,y=PMI,group=Court.Decision))+
  geom_point(aes(color=Court.Decision))+
  geom_vline(xintercept = 2.60,linetype='dotted')+
  geom_hline(yintercept = 46,linetype='dotted')


#signal detection analyses:
#PMI
pred <- prediction(d$PMI, d$Court.Decision.1)
#Repeat for:
pred <- prediction(d$Musly, d$Court.Decision.1)
pred <- prediction(d$InfringementAudio, d$Court.Decision.1)
pred <- prediction(d$AudioSimilarity, d$Court.Decision.1)
pred <- prediction(d$InfringementMelody, d$Court.Decision.1)
pred <- prediction(d$MelodySimilarity, d$Court.Decision.1)
pred <- prediction(d.lyric.na$InfringementLyrics, d.lyric.na$Court.Decision.1)
pred <- prediction(d.lyric.na$LyricSimilarity, d.lyric.na$Court.Decision.1)


##
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a = 0, b = 1)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a = 0, b = 1)

opt.cut = function(perf, pred) {
  cut.ind = mapply(FUN = function(x, y, p) {
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

#Significance of ROCs:
roc.area(d$Court.Decision.1,d$PMI) #AUC=0.7247475, p=0.007462554
roc.area(d$Court.Decision.1,d$Musly) #AUC=0.6590909, p=0.0445481
roc.area(d$Court.Decision.1,d$LyricSimilarity) #AUC=0.4392361, p=0.7336617
roc.area(d$Court.Decision.1,d$InfringementLyrics) #AUC=0.4010417, p=0.8429579
roc.area(d$Court.Decision.1,d$MelodySimilarity) #AUC=0.7487374, p=0.003816406
roc.area(d$Court.Decision.1,d$InfringementMelody) #AUC=0.7058081, p=0.01356389
roc.area(d$Court.Decision.1,d$AudioSimilarity) #AUC=0.7777778, p=0.001436054
roc.area(d$Court.Decision.1,d$InfringementAudio) #AUC=0.7070707, p=0.01316813





#Logistic regression:
#Full model (Excluding unhelpful lyric data with missing data)
glm.fit <- glm(Court.Decision.1~AudioSimilarity+MelodySimilarity+InfringementAudio+InfringementMelody+PMI+Musly,data=d,family = binomial)
summary(glm.fit)

#To re-run analyses using just the subset of previously analyzed songs from Yuan et al. (2020), use following code:
d<- subset(d,PreliminaryStudy=="Yes")