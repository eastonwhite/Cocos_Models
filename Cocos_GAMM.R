#Created by: Easton R. White
#Last edited: 13-March-2014 

#this script contains codes for basic GAMM


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

cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

############################################################
require(mgcv)
Hammerhead_GAMM_full_model <- gamm(Hammerhead~s(StudyYear)+CurrentCode+s(SeaTempF)+s(VisibilityFt)+s(ElNinoIndex)+SIN_TIME+COS_TIME,random=list(SiteCode = ~1, DiverCode=~1),data=cocos,family=poisson)
summary(Hammerhead_GAMM_full_model$gam)
anova(Hammerhead_GAMM_full_model$gam)
plot(Hammerhead_GAMM_full_model$gam)


##from pg 259 of red book
Hammerhead_GAMM_full_model <- gamm(Hammerhead~s(StudyYear)+CurrentCode+s(SeaTempF)+s(VisibilityFt)+s(ElNinoIndex)+SIN_TIME+COS_TIME,random=list(SiteCode = ~1, DiverCode=~1),data=cocos,family=negative.binomial(1),control=gam.control(maxit=100),gamma=1.4)


#run GAMM from pg 76 of zuur inflated models book
Hammerhead_GAMM_full_model <- gamm(Hammerhead~s(StudyYear)+CurrentCode+s(SeaTempF)+s(VisibilityFt)+s(ElNinoIndex)+SIN_TIME+COS_TIME,random=list(SiteCode = ~1, DiverCode=~1),data=cocos,family=poisson)

#run a GAMM zero-inflated hurdle model
cocos$Hammerhead_Presence <- cocos$Hammerhead
cocos$Hammerhead_Presence[cocos$Hammerhead>0] <- 1
Hammerhead_GAMM_hurdle_part1 <- gamm(Hammerhead_Presence~s(StudyYear)+CurrentCode+SeaTempF+VisibilityFt+s(ElNinoIndex)+SIN_TIME+COS_TIME,random=list(SiteCode = ~1, DiverCode=~1),data=cocos,family=binomial)


cocos$Hammerhead_Count <- cocos$Hammerhead
cocos$Hammerhead_Count[cocos$Hammerhead==0] <- NA
Hammerhead_GAMM_hurdle_part2 <- gamm(Hammerhead_Count~s(StudyYear)+CurrentCode+SeaTempF+VisibilityFt+s(ElNinoIndex)+SIN_TIME+COS_TIME,random=list(SiteCode = ~1, DiverCode=~1),data=cocos,family=Gamma(link='log'))
