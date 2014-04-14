#!/bin/env Rscript

## this script imports Cocos data and runs a hurdle models according to Cantoni 2014
CocosData=read.table('/home/erwhite1/data/CocosData_19305_dpts.txt',header=T)

require(fields)
source("/home/erwhite1/data/hurdlemodels/ExtraZeroCodeForJulia_edited.R")

Xspecies <- model.matrix(~StudyYear + ElNinoIndex + CurrentCode +SeaTempF + VisibilityFt + SIN_TIME + COS_TIME, data=CocosData)
Zspecies <- model.matrix(~StudyYear + ElNinoIndex + CurrentCode +SeaTempF + VisibilityFt + SIN_TIME + COS_TIME, data=CocosData)
yspecies <- CocosData$MobulaRays
CLUSTERspecies <- as.factor(CocosData$SiteCode)




MobulaRaysHM <- hurdlemixed(X=Xspecies,Z=Zspecies,y=yspecies,K=100,ident=CLUSTERspecies,number.init.val=5,lowersigma=0.01,uppersigma=2,lowergamma=-2,uppergamma=2)

save(MobulaRaysHM,file='/home/erwhite1/data/MobulaRaysHM.Rdata')


MobulaRays.hat <- c(MobulaRaysHM$alphahat, MobulaRaysHM$betahat, MobulaRaysHM$gammahat, MobulaRaysHM$sigmauhat, MobulaRaysHM$sigmavhat)
MobulaRays.hat

require(numDeriv)


speciesHM = MobulaRaysHM
clusternumberspecies<-length(unique(CLUSTERspecies))
clustersizespecies<-lapply(split(CLUSTERspecies,CLUSTERspecies),'length')



speciesHM$hess <- hessian(func=approxloglik,x=c(speciesHM$alphahat,speciesHM$betahat,speciesHM$gammahat,speciesHM$sigmauhat,speciesHM$sigmavhat),XX=Xspecies,ZZ=Zspecies,yy=yspscies$species,clusternb=clusternumberspecies,clustersz=clustersizespecies,cluster=CLUSTERspecies,uu=speciesHM$u.ll,vv=speciesHM$v.ll)

speciesHM$hess


species.SE <- sqrt(diag(solve(speciesHM$hess)))
species.SE

