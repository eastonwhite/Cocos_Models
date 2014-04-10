#!/bin/env Rscript

hurdlemixed <- function(X, Z, y, ident, K, number.init.val=20,lowersigma=0,uppersigma=2,lowergamma=-2,uppergamma=2)
  {    
    cc <- length(unique(ident))
    cs <- lapply(split(ident,ident),"length")

    alpha.init <- glm((y>0)~-1+X,family=binomial)$coeff
    beta.init <- glm(y~-1+Z,family=truncpoisson,subset=(y>0))$coeff
  
    dima=length(alpha.init)
    dimb=length(beta.init)

    uu.loglik <- rnorm(K)
    vv.loglik <- rnorm(K)
  
    require(fields)
    candidates.3 <- make.surface.grid(list( seq(lowergamma, uppergamma,,number.init.val),  seq(lowersigma, uppersigma,,number.init.val),  seq(lowersigma, uppersigma,,number.init.val)))
    initvalues3dim <- cover.design(R=candidates.3, nd=number.init.val, scale.type="range")
 
    i <- 1
    par.init=c(alpha.init,beta.init,initvalues3dim[1,])
    hmfit <- optim(par=par.init,fn=approxloglik,XX=X,ZZ=Z,yy=y,clusternb=cc,clustersz=cs,cluster=ident,uu=uu.loglik,vv=vv.loglik,method="BFGS",control=list(trace=1,REPORT=1))
#method="L-BFGS-B",lower=c(rep(-Inf,ncol(X)+ncol(Z)+1),0,0),upper=Inf,control=list(trace=TRUE))
    if(hmfit$convergence!=0)
      warning("optim did not converge")
    cat("minusloglik=",hmfit$value,"\n",hmfit$par) #c(hmfit$par[dima+dimb+1],hmfit$par[dima+dimb+2],hmfit$par[dima+dimb+3]),"\n")
    bestvalue <- hmfit 
    allvalues <- vector("list",number.init.val)
    allvalues[[1]] <- hmfit

    for(i in 2:number.init.val)
      {
       cat("Initial value",i,"\n")
       par.init=c(alpha.init,beta.init,initvalues3dim[i,])
       hmfit <- optim(par=par.init,fn=approxloglik,XX=X,ZZ=Z,yy=y,clusternb=cc,clustersz=cs,cluster=ident,uu=uu.loglik,vv=vv.loglik,method="BFGS",control=list(trace=1,REPORT=1))
#method="L-BFGS-B",lower=c(rep(-Inf,ncol(X)+ncol(Z)+1),0,0),upper=Inf,control=list(trace=TRUE))
       if(hmfit$convergence!=0)
         warning("optim did not converge")
       cat("minusloglik=",hmfit$value,"\n",hmfit$par,"\n") #c(hmfit$par[dima+dimb+1],hmfit$par[dima+dimb+2],hmfit$par[dima+dimb+3]),"\n")
       if(bestvalue$value > hmfit$value)  bestvalue <- hmfit
       allvalues[[i]] <- hmfit     
      }
    
    return(list(alphahat=bestvalue$par[1:dima],betahat=bestvalue$par[(dima+1):(dima+dimb)],gammahat=bestvalue$par[dima+dimb+1],sigmauhat=bestvalue$par[dima+dimb+2],sigmavhat=bestvalue$par[dima+dimb+3], minusloglikvalue=bestvalue$value,hess=hmfit$hessian,allvalues=allvalues,initvalues=initvalues3dim,u.ll=uu.loglik,v.ll=vv.loglik))
  }

approxloglik <- function(theta,XX,ZZ,yy,clusternb,clustersz,cluster,K=1000,uu=rnorm(K),vv=rnorm(K))
  {
    ncolX <- ncol(XX)
    ncolZ <- ncol(ZZ)
    alpha <- theta[1:ncolX]
    beta <- theta[(ncolX+1):(ncolX+ncolZ)]
    gamma <- theta[ncolX+ncolZ+1]
    sigmau <- theta[ncolX+ncolZ+2]
    sigmav <- theta[ncolX+ncolZ+3]

    Xalpha <- XX%*%alpha
    Xalphasplit <- split(Xalpha,cluster)
    Zbeta <- ZZ%*%beta
    Zbetasplit <- split(Zbeta,cluster)
    ysplit <- split(yy,cluster)

    fct.aux1 <- function(t,y)
      {
        ypos <- (y>0)
        -1*log(1+exp(t))+ypos*t
      }

    fct.aux2 <- function(t,y)
      {
        ypos <- (y>0)
        -1*ypos*exp(t)+ypos*y*t-ypos*lfactorial(y)-ypos*log(1-exp(-exp(t)))
      }
       
    integ <- numeric(clusternb)
 
    for(i in 1:clusternb)
      {
        Xalphamat <- outer(Xalphasplit[[i]],sigmau*uu,FUN="+")
        Zbetamat <- outer(Zbetasplit[[i]],gamma*sigmau*uu+sigmav*vv,FUN="+")
        integ[i] <- mean(exp(colSums(fct.aux1(Xalphamat,y=ysplit[[i]])+fct.aux2(Zbetamat,y=ysplit[[i]]))))
      }
    -1*sum(log(integ))
  }


truncpoisson <- function()
  {
    
   h.function <- function(x) {
     res <- x
     index <- x < 5
     a <- 0.58178587
     b <- 0.04364283
     res[index] <- (1 + a * x + b * x^2)[index]
     res
   }

   hinv.function <- function(x) {
     res <- x
     index <- x < 5
     a <- 0.58178587
     b <- 0.04364283
     res[index] <- (( - a + sqrt(a^2 + 4 * b * (x - 1)))/(2 *b))[index]
     res
   }

   hderiv.function <- function(x)
     {
       (1-exp(-x)-x*exp(-x))/(1 - exp(- x))^2  
     }

   hderiv2.function <- function(x)
     {
       (exp( - x) * ((x - 2) * (1 - exp( - x)) + 2 * x * exp( - x)))/(1 - exp(- x))^3 
     }

    linkfun <- function(mu) {log(hinv.function(mu))}
    linkinv <- function(eta) {h.function(exp(eta))}
    variance <- function(mu) {mu*(1+hinv.function(mu)-mu)}
    validmu <- function(mu) {all(mu>0)}

   # This are squared residuals = deviance components.
   # Note that in Barry & Welsh (2002), - deviance is given.
    dev.resids <- function(y,mu,wt){-2*wt* ( y * log(hinv.function(mu)) - hinv.function(mu) - log(1 -exp( - hinv.function(mu))) + ifelse(y==1,0, - y * log(hinv.function(y)) + hinv.function(y) + log(1 - exp( - hinv.function(y)))))}

    # It is in fact -2 loglik  
    aic <- function(y,n,mu,wt,dev){-2*sum(log(dpois(y,mu)/(1-exp(-mu)))*wt)}
    
    mu.eta <- function(eta) {hderiv.function(exp(eta))*exp(eta)}

    dvar <- function(mu)
       {
         (1+hinv.function(mu)-mu) + mu*(1/hderiv.function(hinv.function(mu))-1)
       }
   
    d2link <- function(mu)
      {
        -1/(hinv.function(mu))^2/(hderiv.function(hinv.function(mu)))^2 - 1/hinv.function(mu)/(hderiv.function(hinv.function(mu)))^3*hderiv2.function(hinv.function(mu))
      }

    initialize <- expression({
      mustart <- y
      mustart[mustart==1] <- 1.1})    

    structure(list(family="truncpoisson",link="trunclog",linkfun=linkfun,linkinv=linkinv,variance=variance,dev.resids=dev.resids,aic=aic,mu.eta=mu.eta,dvar=dvar,d2link=d2link,initialize=initialize,validmu=validmu),class="family")
  }


