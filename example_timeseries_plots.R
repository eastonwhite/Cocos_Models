#This script creates a rough example of what the time series plot should look like in the final picture. The correct data and models have not been included in this script
#Created by Easton R. White
#Last edited 11-Apr-2014

#the code has not been commented on for clarity or understanding yet

pdf(file='exampletimeseries.pdf',height=5,width=8)

par(mfrow=c(3,4))

op <- par(mfrow = c(3,4),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,1.5,1,1) + 0.1)

aaa=aggregate(cocos$Hammerhead,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

require(biOps)
hammerhead=readJpeg('hammerhead.jpg')
require(TeachingDemos)
subplot(plot(hammerhead),x=2006,y=100,size=c(1,1))

aaa=aggregate(cocos$Whitetips,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

aaa=aggregate(cocos$MarbleRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

aaa=aggregate(cocos$EagleRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

aaa=aggregate(cocos$MantaRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

aaa=aggregate(cocos$MobulaRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')


aaa=aggregate(cocos$Hammerhead,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')
hammerhead=readJpeg('hammerhead.jpg')
require(TeachingDemos)
subplot(plot(hammerhead),x=2006,y=100,size=c(1,1))

aaa=aggregate(cocos$Whitetips,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2,xaxt='n')

aaa=aggregate(cocos$MarbleRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2)

aaa=aggregate(cocos$EagleRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2)

aaa=aggregate(cocos$MantaRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2)

aaa=aggregate(cocos$MobulaRays,by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013
plot(aaa,'b',ylab='',xlab='',las=2)

mtext('Number of individuals per dive',2,line=2,outer=TRUE,cex=1.2)
mtext('Time (years)',1,line=3.5,outer=TRUE,cex=1.2)

dev.off()
