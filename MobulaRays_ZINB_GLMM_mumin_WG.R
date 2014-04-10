#!/bin/env Rscript

CocosData=read.table('/home/erwhite1/data/CocosData_19305_dpts.txt',header=T)

CocosData$SiteCode <- as.factor(CocosData$SiteCode)
CocosData$CurrentCode <- as.factor(CocosData$CurrentCode)
CocosData$DiverCode <- as.factor(CocosData$DiverCode)
CocosData$StudyYear<-as.numeric(CocosData$StudyYear)


require(glmmADMB)
require(MuMIn)
MobulaRays_ZINB_GLMM_full_model <- glmmadmb(formula=MobulaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(CocosData),family="binomial",zeroInflation=FALSE)
MobulaRays_ZINB_GLMM_mumin <- dredge(MobulaRays_ZINB_GLMM_full_model,rank="AIC",subset=(COS_TIME | !SIN_TIME) && (!COS_TIME | SIN_TIME))
MobulaRays_ZINB_GLMM_mumin

save(MobulaRays_ZINB_GLMM_mumin,file='/home/erwhite1/data/MobulaRays_ZINB_GLMM_mumin.Rdata')




