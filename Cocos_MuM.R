#Created by: Easton R. White
#Last edited: 6-Jan-2013 

#this script contains codes for basic analyses using ADMB in R for Cocos data set

#require pacakges
require(glmmADMB)
require(ggplot2)
require(MuMIn)

#call in data and run a number of filters
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

#define seasonality terms
cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

#########################################################################################################
#run basic NB GLM with MuMIn
require(MASS)
EagleRays_NB_GLM_theta=glm.nb(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+DiverCode+SiteCode,data=na.omit(cocos))

EagleRays_NB_GLM_full_model=glm(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+DiverCode+SiteCode,family=negative.binomial(EagleRays_NB_GLM_theta$theta),data=na.omit(cocos))

EagleRays_NB_GLM_mumin <- dredge(EagleRays_NB_GLM_full_model,rank='AIC',subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME),extra=c('deviance','R^2'))
EagleRays_NB_GLM_mumin$dev_exp=100*(EagleRays_NB_GLM_mumin$deviance[nrow(EagleRays_NB_GLM_mumin)]-EagleRays_NB_GLM_mumin$deviance)/EagleRays_NB_GLM_mumin$deviance[nrow(EagleRays_NB_GLM_mumin)]
subset(EagleRays_NB_GLM_mumin,delta<5)

EagleRays_NB_GLM_bestmodel <- glm(formula=EagleRays~StudyYear+SeaTempF+VisibilityFt+ElNinoIndex+DiverCode+SiteCode,family=negative.binomial(EagleRays_NB_GLM_theta$theta),data=na.omit(cocos))

save(EagleRays_NB_GLM_mumin,file='EagleRays_NB_GLM_mumin.Rdata')

#########################################################################################################
#run GLMM negative binomial
EagleRays_NB_GLMER_theta=glmer.nb(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos))

#0.1749 from glmer.nb of Manta Rays vs all explanatory variables and data
#0.3995 from glmer.nb of Eagle Rays vs all explanatory variables and data
EagleRays_NB_GLMER_full_model=glmer(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),family=negative.binomial(0.3995),data=na.omit(cocos))

EagleRays_NB_GLMER_mumin <- dredge(EagleRays_NB_GLMER_full_model,rank='AIC',subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME),extra=c('deviance','R^2'))
EagleRays_NB_GLMER_mumin$dev_exp=100*(EagleRays_NB_GLMER_mumin$deviance[nrow(EagleRays_NB_GLMER_mumin)]-EagleRays_NB_GLMER_mumin$deviance)/EagleRays_NB_GLMER_mumin$deviance[nrow(EagleRays_NB_GLMER_mumin)]

EagleRays_NB_GLMER_bestmodel <- glmer(formula=EagleRays~StudyYear+SeaTempF+VisibilityFt+ElNinoIndex+(1|DiverCode)+(1|SiteCode),family=negative.binomial(0.3995),data=na.omit(cocos))

save(EagleRays_NB_GLMER_mumin,file='EagleRays_NB_GLMER_mumin.Rdata')

#########################################################################################################
#GLMM with nbinom in glmmadmb

EagleRays_ZINB_GLMM_full_model <- glmmadmb(formula=EagleRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(cocos),family="nbinom",zeroInflation=TRUE)
EagleRays_ZINB_GLMM_mumin <- dredge(ZINB,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
#subset(EagleRays_ZINB_GLMM_mumin,delta<5)
#model.avg(EagleRays_ZINB_GLMM_mumin, delta<2)
#summary(get.models(EagleRays_ZINB_GLMM_mumin,1)[[1]])

save(EagleRays_ZINB_GLMM_mumin,file='EagleRays_ZINB_GLMM_mumin.Rdata')







