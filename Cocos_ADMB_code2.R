#Created by: Easton R. White
#Last edited: 6-Jan-2013 

#this script contains codes for basic analyses using ADMB in R for Cocos data set

library(glmmADMB)
library(ggplot2)

#call in data 
main=read.csv('FINAL_COMPLETE_main_species_data_EW.csv',header=T,sep=',')
cocos=subset(main,main$IslandCode==1)
cocos=cocos[,1:38]

#remove nightime data- if I remove the night dives, it will remove the first 5 years of data because all the records for TimeCode in the first 5 years are NA
#cocos=subset(cocos,cocos$TimeCode<4)
#remove miscellaneous dive sites
cocos=subset(cocos,cocos$SiteCode<119)
#subset part of years
#cocos=subset(cocos,cocos$StudyYear<10)
#remove misc and unknown divemasters from data set
cocos=subset(cocos,cocos$DiverCode<40)

cocos$SeaCondCode[cocos$SeaCondCode>3]=3
cocos$CurrentCode[cocos$CurrentCode>3]=3


#set factor variables to factors
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$TimeCode <- as.factor(cocos$TimeCode)
cocos$WeatherCode <- as.factor(cocos$WeatherCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$SeaCondCode <- as.factor(cocos$SeaCondCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$JackCode <- as.factor(cocos$JackCode)

cocos$StudyYear<-as.numeric(cocos$StudyYear)
cocos$Month<-as.numeric(cocos$Month)
cocos$StudyMonth<-as.numeric(cocos$StudyMonth)



cocos=na.omit(cocos[,c(3,7,8,17,19,20,23,12,22,25:32)])


#############################################################################
# # 

# #ZIP
# GLM1 <- glmmadmb(formula=Hammerhead~Month+StudyYear+(1|SiteCode)+ElNinoIndex,data=na.omit(cocos),family="poisson",zeroInflation=TRUE)
# summary(GLM1)

# plot(fitted(GLM1),residuals(GLM1),col=rgb(1,0,0,alpha=0.4))

# library(coefplot)
# coefplot(GLM1)


# #ZINB 1
# GLM2 <- glmmadmb(formula=Hammerhead~Month+StudyYear+(1|SiteCode)+ElNinoIndex,data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
# summary(GLM2)

# plot(fitted(GLM2),residuals(GLM2),col=rgb(1,0,0,alpha=0.4))

# library(coefplot)
# coefplot(GLM2)

# #ZINB 2
# GLM3 <- glmmadmb(formula=Hammerhead~Month+StudyYear+(1|SiteCode)+ElNinoIndex,data=na.omit(cocos),family="nbinom1",zeroInflation=TRUE)
# summary(GLM3)

# plot(fitted(GLM3),residuals(GLM3),col=rgb(1,0,0,alpha=0.4))

# library(coefplot)
# coefplot(GLM3)

# #compare models
# library(bbmle)
# AICtab(GLM1,GLM2,GLM3)

# multiplot(list(GLM1,GLM2,GLM3))




#########################################################################
####Use model explantory variables####

#ZIP
ZIP <- glmmadmb(formula=Hammerhead~Month+StudyYear+TimeCode+WeatherCode+CurrentCode+SeaCondCode+SeaTempF+VisibilityFt+ElNinoIndex+(1|SiteCode)+(1|DiverCode),data=na.omit(cocos),family="poisson",zeroInflation=TRUE)
summary(ZIP)

plot(fitted(ZIP),residuals(ZIP),col=rgb(1,0,0,alpha=0.4))
coefplot(ZIP)


#ZIP-All variables
ZIP <- glmmadmb(formula=Hammerhead~Month+StudyYear+TimeCode+WeatherCode+CurrentCode+SeaCondCode+SeaTempF+VisibilityFt+ElNinoIndex+Turtles+MobulaRays+EagleRays+Whitetips+MarbleRays+MantaRays+JackCode+(1|SiteCode)+(1|DiverCode),data=na.omit(cocos),family="poisson",zeroInflation=TRUE)
summary(ZIP)

plot(fitted(ZIP),residuals(ZIP),col=rgb(1,0,0,alpha=0.4))
coefplot(ZIP)

####ZINB-most variables except SeaCondCode- see notes
start=Sys.time()
ZINB <- glmmadmb(formula=Hammerhead~Month+StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|SiteCode)+(1|DiverCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
summary(ZINB)
Time=Sys.time()-start
plot(fitted(ZINB),residuals(ZINB),col=rgb(1,0,0,alpha=0.4))
coefplot(ZINB)


####ZINB-add sin and cos terms
Sys.time()
start=Sys.time()
ZINB2 <- glmmadmb(formula=MantaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+sin(2*pi*StudyJulianDate/365.25)+cos(2*pi*StudyJulianDate/365.25)+(1|SiteCode)+(1|DiverCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
summary(ZINB2)
Time=Sys.time()-start
plot(fitted(ZINB),residuals(ZINB),col=rgb(1,0,0,alpha=0.4))
coefplot(ZINB)







