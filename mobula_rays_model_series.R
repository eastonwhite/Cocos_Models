#!/bin/env Rscript

## this script imports Cocos data and a series of models for Mobula rays
## Created by Easton R. WHite
## Last edited: 9-Apr-2014

## call in data
CocosData=read.table('CocosData_19305_dpts.txt',header=T)

## load packages
require(fields)
require(numDeriv)

## run hurdle model R script
source("ExtraZeroCodeForJulia_edited.R")

## run hurdle model form Cantoni et al (2014)
Xspecies <- model.matrix(~StudyYear + ElNinoIndex + CurrentCode +SeaTempF + VisibilityFt + SIN_TIME + COS_TIME, data=CocosData)
Zspecies <- model.matrix(~StudyYear + ElNinoIndex + CurrentCode +SeaTempF + VisibilityFt + SIN_TIME + COS_TIME, data=CocosData)
yspecies <- CocosData$MobulaRays
CLUSTERspecies <- as.factor(CocosData$SiteCode)

speciesHM <- hurdlemixed(X=Xspecies,Z=Zspecies,y=yspecies,K=100,ident=CLUSTERspecies,number.init.val=5,lowersigma=0.01,uppersigma=2,lowergamma=-2,uppergamma=2)
species.hat <- c(MobulaRaysHM$alphahat, MobulaRaysHM$betahat, MobulaRaysHM$gammahat, MobulaRaysHM$sigmauhat, MobulaRaysHM$sigmavhat)


clusternumberspecies<-length(unique(CLUSTERspecies))
clustersizespecies<-lapply(split(CLUSTERspecies,CLUSTERspecies),'length')


speciesHM$hess <- hessian(func=approxloglik,x=c(speciesHM$alphahat,speciesHM$betahat,speciesHM$gammahat,speciesHM$sigmauhat,speciesHM$sigmavhat),
	XX=Xspecies,ZZ=Zspecies,yy=yspscies$species,clusternb=clusternumberspecies,clustersz=clustersizespecies,cluster=CLUSTERspecies,uu=speciesHM$u.ll,vv=speciesHM$v.ll)
species.SE <- sqrt(diag(solve(speciesHM$hess)))


###############################################################################
###############################################################################


require(glmmADMB)

## Run zero-inflated mixture model with glmmadmb()
## call in data
CocosData=read.table('CocosData_19305_dpts.txt',header=T)

CocosData$SiteCode <- as.factor(CocosData$SiteCode)
CocosData$CurrentCode <- as.factor(CocosData$CurrentCode)
CocosData$DiverCode <- as.factor(CocosData$DiverCode)
CocosData$StudyYear<-as.numeric(CocosData$StudyYear)


require(glmmADMB)
require(MuMIn)
MobulaRays_ZI_GLMM_full_model <- glmmadmb(formula=MobulaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+
							ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(CocosData),
							family="nbinom",zeroInflation=TRUE)


###############################################################################
###############################################################################


#run logistic mixed model (presence-absence) for MobulaRays
MobulaRays_Binomial_GLMM_full_model <- glmmadmb(formula=MobulaRays~StudyYear+CurrentCode+SeaTempF+VisibilityFt+
							ElNinoIndex+SIN_TIME+COS_TIME+(1|DiverCode)+(1|SiteCode),data=na.omit(CocosData),
							family="binomial",zeroInflation=FALSE)



