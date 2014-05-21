#!/bin/env Rscript


#Created by: Easton R. White
#Last edited: 21-May-2014 

#this script contains codes for basic analyses using ADMB in R for Cocos data set

#require pacakges
require(glmmADMB)
require(MuMIn)

# # #call in data and run a number of filters
# # main=read.csv('cocos_test.csv',header=T,sep=',')

# # ##################
# # #remove letters from explanatory variables
# # require(stringr)
# # main$SeaTempF= str_replace_all(main$SeaTempF[1:nrow(main)], "[c,Fft]", "")
# # main$SeaTempF=as.numeric(main$SeaTempF)

# # main$VisibilityFt= str_replace_all(main$VisibilityFt[1:nrow(main)], "[c,FftTd]", "")
# # main$VisibilityFt=as.numeric(main$VisibilityFt)

# # ##################

# # cocos=subset(main,main$IslandCode==1)
# # malpelo=subset(main,main$IslandCode==2)

# # #$remove 1992 and 2014
# # cocos=subset(cocos,cocos$Year>1992)
# # cocos=subset(cocos,cocos$Year<2014)

# # nrow(cocos)

# # #remove divermasters who made less than 100 dives, and misc, unknown divemasters
# # cocos=subset(cocos,cocos$DiverCode<38)
# # cocos=subset(cocos,cocos$DiverCode!=21)

# # nrow(cocos)

# # #remove miscellaneous dive sites
# # cocos=subset(cocos,cocos$SiteCode<119)
# # cocos=subset(cocos,cocos$SiteCode!=104)

# # nrow(cocos)

# # #remove unneccesary rows (not including marble rays)
# # marblerays=cocos[,c(1:12,17,19:23,27)]
# # cocos=cocos[,c(1:12,17,19:23,25,26,28:30,33:38)]


# # cocos=na.omit(cocos)
# # nrow(cocos)

# # #############################################


# # #########
# # cocos$CurrentCode[cocos$CurrentCode>3]=3


# # #set factor variables to factors
# # cocos$SiteCode <- as.factor(cocos$SiteCode)
# # cocos$CurrentCode <- as.factor(cocos$CurrentCode)
# # cocos$DiverCode <- as.factor(cocos$DiverCode)


# # #define seasonality terms
# # cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
# # cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

# # ###################################################
# # #Convert "other species to presence-absence
# # cocos$Silky[cocos$Silky!=0]=1
# # cocos$Silvertip[cocos$Silvertip!=0]=1
# # cocos$Blacktip[cocos$Blacktip!=0]=1
# # cocos$Galapagos[cocos$Galapagos!=0]=1
# # cocos$WhaleShark[cocos$WhaleShark!=0]=1
# # cocos$Tiger[cocos$Tiger!=0]=1




#######################################################################
#######################################################################
######Setup for Westgird#####

CocosData=read.csv('/home/erwhite1/data/cocos_test_clean.csv',header=T,sep=',')

CocosData$SiteCode <- as.factor(CocosData$SiteCode)
CocosData$CurrentCode <- as.factor(CocosData$CurrentCode)
CocosData$DiverCode <- as.factor(CocosData$DiverCode)



#Run basic models


## Hammerheads (GLMM with negative binomial error function)
Hammerhead_ZINB_GLMM_full_model <- glmmadmb(formula=Hammerhead~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=FALSE)
Hammerhead_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
#subset(Hammerhead_ZINB_GLMM_mumin,delta<5)
#model.avg(Hammerhead_ZINB_GLMM_mumin, delta<2)
#summary(get.models(Hammerhead_ZINB_GLMM_mumin,1)[[1]])

save(Hammerhead_ZINB_GLMM_mumin,file='/home/erwhite1/data/Hammerhead_ZINB_GLMM_mumin.Rdata')





# # #########################################################################################################
# # ## Whitetips (GLMM with negative binomial error function)
# # Whitetips_ZINB_GLMM_full_model <- glmmadmb(formula=Whitetips~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=FALSE)
# # Whitetips_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
# # #subset(Whitetips_ZINB_GLMM_mumin,delta<5)
# # #model.avg(Whitetips_ZINB_GLMM_mumin, delta<2)
# # #summary(get.models(Whitetips_ZINB_GLMM_mumin,1)[[1]])

# # save(Whitetips_ZINB_GLMM_mumin,file='Whitetips_ZINB_GLMM_mumin.Rdata')

# # #########################################################################################################

# # #########################################################################################################
# # ## EagleRays (Zero-inflated GLMM with negative binomial error function)
# # EagleRays_ZINB_GLMM_full_model <- glmmadmb(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
# # EagleRays_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
# # #subset(EagleRays_ZINB_GLMM_mumin,delta<5)
# # #model.avg(EagleRays_ZINB_GLMM_mumin, delta<2)
# # #summary(get.models(EagleRays_ZINB_GLMM_mumin,1)[[1]])

# # save(EagleRays_ZINB_GLMM_mumin,file='EagleRays_ZINB_GLMM_mumin.Rdata')


# # #########################################################################################################
# # ## MantaRays (Zero-inflated GLMM with negative binomial error function)
# # MantaRays_ZINB_GLMM_full_model <- glmmadmb(formula=MantaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
# # MantaRays_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
# # #subset(MantaRays_ZINB_GLMM_mumin,delta<5)
# # #model.avg(MantaRays_ZINB_GLMM_mumin, delta<2)
# # #summary(get.models(MantaRays_ZINB_GLMM_mumin,1)[[1]])

# # save(MantaRays_ZINB_GLMM_mumin,file='MantaRays_ZINB_GLMM_mumin.Rdata')



# # #########################################################################################################
# # ## MobulaRays  (Zero-inflated GLMM with negative binomial error function)
# # MobulaRays_ZINB_GLMM_full_model <- glmmadmb(formula=MobulaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
# # MobulaRays_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
# # #subset(MobulaRays_ZINB_GLMM_mumin,delta<5)
# # #model.avg(MobulaRays_ZINB_GLMM_mumin, delta<2)
# # #summary(get.models(MobulaRays_ZINB_GLMM_mumin,1)[[1]])

# # save(MobulaRays_ZINB_GLMM_mumin,file='MobulaRays_ZINB_GLMM_mumin.Rdata')





