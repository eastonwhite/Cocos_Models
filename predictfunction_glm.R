

f4=(Whitetips~ElNinoIndex)
model=glm.nb(f4,data=cocos)

MyData<- data.frame(ElNinoIndex=seq(-3,3,by=1))

Pred <- predict(model,newdata=MyData,type='response',se.fit=T)
dude=aggregate(cocos$Whitetips,by=list(cocos$ElNinoIndex),FUN=mean)
dude1=aggregate(cocos$Whitetips,by=list(cocos$ElNinoIndex),FUN=sd)
plot(x=dude$Group.1,y=dude$x,col='red',xlim=c(-3,3),ylim=c(0,100))
lines(MyData$ElNinoIndex,Pred$fit)
lines(MyData$ElNinoIndex,Pred$fit + 1.96*Pred$se.fit,lty=2)
lines(MyData$ElNinoIndex,Pred$fit - 1.96*Pred$se.fit,lty=2)


for (j in 1:nrow(dude)){
	arrows(dude$Group.1[j],dude$x[j],dude$Group.1[j],dude$x[j]+dude1$x[j],angle=90,length=0.1)
	arrows(dude$Group.1[j],dude$x[j],dude$Group.1[j],dude$x[j]-dude1$x[j],angle=90,length=0.1)
}