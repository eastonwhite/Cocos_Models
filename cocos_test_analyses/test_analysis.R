#Created by: Easton R. White
#Last edited: 21-May-2014 

#this script contains codes for basic analyses using ADMB in R for Cocos data set

#require pacakges
require(glmmADMB)
require(ggplot2)
require(MuMIn)

#call in data and run a number of filters
main=read.csv('cocos_test.csv',header=T,sep=',')

##################
#remove letters from explanatory variables
main$SeaTempF= str_replace_all(main$SeaTempF[1:nrow(main)], "[c,Fft]", "")
main$SeaTempF=as.numeric(main$SeaTempF)

main$VisibilityFt= str_replace_all(main$VisibilityFt[1:nrow(main)], "[c,FftTd]", "")
main$VisibilityFt=as.numeric(main$VisibilityFt)

##################

cocos=subset(main,main$IslandCode==1)
malpelo=subset(main,main$IslandCode==2)

#$remove 1992 and 2014
cocos=subset(cocos,cocos$Year>1992)
cocos=subset(cocos,cocos$Year<2014)

nrow(cocos)

#remove divermasters who made less than 100 dives, and misc, unknown divemasters
cocos=subset(cocos,cocos$DiverCode<38)
cocos=subset(cocos,cocos$DiverCode!=21)

nrow(cocos)

#remove miscellaneous dive sites
cocos=subset(cocos,cocos$SiteCode<119)
cocos=subset(cocos,cocos$SiteCode!=104)

nrow(cocos)

#remove unneccesary rows (not including marble rays)
marblerays=cocos[,c(1:12,17,19:23,27)]
cocos=cocos[,c(1:12,17,19:23,25,26,28:30,33:38)]


cocos=na.omit(cocos)
nrow(cocos)

#############################################


#########
cocos$CurrentCode[cocos$CurrentCode>3]=3


#set factor variables to factors
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)


#define seasonality terms
cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

###################################################
#Convert "other species to presence-absence




#######################################################################
#######################################################################