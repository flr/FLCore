# Plot - plots of residuals for diagnostic checking
# FLCore/R/PlotResid.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: laurie Kell, ICCAT
# $Id: PlotResid.R

# Reference:
# Notes:


##### Generic Functions ########################################################
# panel functions
srpanel <- function(x, y, ...) {
	panel.xyplot(x, y, col='gray40', cex=0.8)
	panel.loess(x,y, col='red')
	panel.abline(a=0, b=0, lty=2, col='blue')
	}

respanel <- function(x, y, ...) {
	panel.xyplot(x, y, col='gray40', cex=0.8)
  panel.lmline(x, y, ..., col='red')
  panel.abline(a=0, b=0, lty=2, col='blue')
	}

# get dimensions to condition on
condNms<-function(flq){
		res <-names(flq)[c(1,3:5)][dim(flq)[c(1,3:5)]!=1]
		res <-paste(res, collapse="+")
		if(res != "") res<-paste("|", res)

		return(res)
		}

# compare fits

# plots realtionship
plotFunc<-function(obs,prd,indVar,indVar.,xttl="X",yttl="Y",mttl="",splt=c(1,1,1,1),more=FALSE)
  	{
    cond<-condNms(prd)

    dat=model.frame(FLQuants(indVar.=indVar.,prd=prd,obs=obs,indVar=indVar))

    print(xyplot(formula(paste("obs~indVar", cond)), data=dat,
            panel=function(x, y, subscripts){
            panel.xyplot(x, y,col="black")
    	      panel.loess(x, y, lty=4,col="blue")
    	      panel.lines(sort(dat$indVar.[subscripts]),dat$prd[subscripts][order(dat$indVar.[subscripts])], col="red")
              }, subscripts=T, scale="free", main=mttl, xlab=xttl, ylab=yttl),
           split=splt, more=more)

		invisible()
    }

plotResidYr<-function(resid,xttl="Year",yttl='Residuals',mttl="",splt=c(1,1,1,1),more=FALSE)
  	{
		# 2. Residuals plotted against year

    cond<-condNms(resid)
		print(xyplot(formula(paste("resid~year", cond)), ylab='Residuals', xlab=xttl,
			data=model.frame(FLQuants(resid=resid)),
			panel=srpanel, main='Residuals by year',scale="free"), split=splt, more=more)

		invisible()
    }

plotResidX<-function(resid,Var,xttl="X",yttl='Residuals',mttl="",splt=c(1,1,1,1),more=FALSE)
  	{
    cond<-condNms(resid)
		print(xyplot(formula(paste("resid~Var", cond)), ylab='Residuals', xlab=xttl,
			model.frame(FLQuants(resid=resid, Var=Var)),
			panel=srpanel, main=mttl,scale="free"), split=splt, more=more)

		invisible()
    }

plotResidAR1<-function(resid,xttl='Residuals at t',yttl='Residuals at t+1',mttl='AR(1) Residuals',splt=c(1,1,1,1),more=FALSE)
  	{
		# Residuals at time t vs. residuals at time t+1

    cond<-condNms(resid)
		print(xyplot(formula(paste("resid1~resid", cond)), ylab=yttl,
      xlab=xttl, model.frame(FLQuants(resid=resid,
      resid1=FLQuant(resid,
      dimnames=list(year=as.numeric(dimnames(resid)$year)+1)))),
		  panel=respanel, main=mttl,scale="free"), split=splt, more=more)

		invisible()
    }

plotResidQQ<-function(resid,xttl='Sample Quantiles',yttl='Residuals',mttl='Normal Q-Q Plot',splt=c(1,1,1,1),more=FALSE)
  	{
		# qqplot of residuals

    cond<-condNms(resid)
		print(qqmath(formula(paste("~resid", cond)), ylab=yttl,
    xlab=xttl, model.frame(FLQuants(resid=resid)),
      panel = function(x, ...) {
          panel.qqmath(x, ..., , col='gray40', cex=0.8)
          panel.qqmathline(x, ..., col='red')
       }, main='Normal Q-Q Plot',scale="free"), split=splt, more=more)


    invisible()
    }

plotResidAll<-function(resid,obs,prd,hat,indVar,indVar.,Xttl="X",Yttl="Y")
  	{
    # initial device settings
    trellis.par.set(list(layout.heights = list(bottom.padding = -0.5,
      axis.xlab.padding = 0.5, xlab = -0.5), layout.widths = list(left.padding = -0.5,
      right.padding = -0.5, ylab.axis.padding = -0.5)))

    plotFunc(          obs,prd,indVar,indVar.,xttl=Xttl,              yttl=Yttl,              mttl="Functional Form",      splt=c(1,1,2,3),more=TRUE)
    plotResidYr( resid,                       xttl="Year",            yttl='Residuals',       mttl="",                     splt=c(2,1,2,3),more=TRUE)
    plotResidAR1(resid,                       xttl='Residuals at t',  yttl='Residuals at t+1',mttl='AR(1) Residuals',      splt=c(1,2,2,3),more=TRUE)
    plotResidX(  resid,indVar,                xttl=Xttl,              yttl='Residuals',       mttl="Residuals by Ind var", splt=c(2,2,2,3),more=TRUE)
    plotResidX(  resid,hat,                   xttl=paste(Yttl, "Hat"),yttl='Residuals',       mttl="Residuals by Hat",     splt=c(1,3,2,3),more=TRUE)
    plotResidQQ( resid,                       xttl='Sample Quantiles',yttl='Residuals',       mttl="",                     splt=c(2,3,2,3),more=FALSE)

		trellis.unfocus()

    invisible()
    }

setGeneric('plotA', function(x,y, ...)
		standardGeneric('plotA'))
setMethod("plotA", signature(x="FLSR", y="missing"),

function(x)
	{
  resid <-residuals(x)
  obs   <-rec(x)
  indVar<-ssb(x)
  hat   <-fitted(x)

  # get values over 0 to max
  indVar.<-expand(apply(indVar,c(1,3:6),max),year=1:length(obs))
  mult<-seq(0,1,length.out=length(obs))
  for (i in length(obs):1)
     indVar.[,i]<-indVar.[,1]*mult[i]
  prd <-predict(x,ssb=indVar.)

  plotResidAll(resid,obs,prd,hat,indVar,indVar.,Yttl="Recruits",Xttl="SSB")
  })
  
  
