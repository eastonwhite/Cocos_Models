aaa=aggregate(predict(MantaRays_best_model_full_data,type='response'),by=list(cocos$StudyYear),FUN=mean)
aaa$Group.1=1992:2013

bbb=aggregate(predict(MantaRays_best_model_full_data,type='response'),by=list(cocos$StudyYear),FUN=sd)
bbb$Group.1=1992:2013

 plot(aaa,ylim=c(0,),type='l', ylab='Number observed per dive',xlab='Time (years)',las=1,main='MantaRays')
#points(bbb$Group.1,aaa$x+bbb$x,type='l',col='red',lty=2)
#points(bbb$Group.1,aaa$x-bbb$x,type='l',col='red',lty=2)




y.low <- aaa$x-bbb$x
y.high <- aaa$x+bbb$x

#plot(age,y.high,type = 'n', ylim = c(100, 400),
     #ylab = 'Y Range', xlab = 'Age (years)')
lines(bbb$Group.1, y.low, col = rgb(1,0,0,alpha=0.1))
lines(bbb$Group.1, y.high, col = rgb(1,0,0,alpha=0.1))

polygon(c(bbb$Group.1, rev(bbb$Group.1)), c(y.high, rev(y.low)),
     col = rgb(1,0,0,alpha=0.1), border = NA)

actualcounts=aggregate(cocos$MantaRays,by=list(cocos$StudyYear),FUN=mean)
actualcounts$Group.1=1992:2013
points(actualcounts,pch='o',ylim=c(0,3),col='blue')

legend('topright',legend=c('Actual Count','Model Prediction'),pch=c('o',''),col=c('blue','black'),lty=c(0,1))

#points(aggregate(cocos$MantaRays,by=list(cocos$StudyYear),FUN=sd),pch='o',ylim=c(0,200),col='green')

