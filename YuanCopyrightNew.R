
#load packages
library(ggplot2)
library(ROCR)

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

#signal detection analyses:
#PMI
pred <- prediction(d$PMI, d$Court.Decision.1)
#Repeat for:
pred <- prediction(d$Musly, d$Court.Decision.1)
pred <- prediction(d$InfringementAudio, d$Court.Decision.1)
pred <- prediction(d$InfringementMelody, d$Court.Decision.1)
pred <- prediction(d.lyric.na$InfringementLyrics, d.lyric.na$Court.Decision.1)

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



