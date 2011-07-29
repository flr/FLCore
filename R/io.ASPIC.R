#### Exported functions in FLCore NAMESPACE

##### New code #####################################################################################
setGeneric("readASPIC",    function(x,type,scen,...) standardGeneric("readASPIC"))

setMethod("readASPIC",     signature(x="character", type="missing",  scen="missing"),    function(x,type,scen,...)    .readASPIC(  x,type,scale="msy",...))
setMethod("readASPIC",     signature(x="character", type="character",scen="missing"),    function(x,type,scen,...)    .readASPIC(  x,type,scale="msy",...))
setMethod("readASPIC",     signature(x="character", type="character",scen="data.frame"), function(x,type,scen,...)    .readASPIC(  x,type,scen,scale="msy",...))
setMethod("readASPIC",     signature(x="character", type="character",scen="character"),  function(x,type,scen,...)    .readASPIC(  x,type,scen,scale="msy",...))

setMethod("readSS3",       signature(x="character"),                   function(x,...)                                .readSS3(    x,yrs,scen,...))


#### ASPIC #####################################################################################
#### Historic series
aspicTS<-function(file,scale="msy"){
   t.  <-scan(file,skip=4)
   nits<-scan(file,skip=1,nmax=1)
   yrs <-scan(file,skip=2,nmax=2)
   nyrs<-diff(yrs)
   nval<-nyrs*2+3

   yrs <-yrs[1]:yrs[2]

   b.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1) x:(x+y-1)))],dimnames=list(year=yrs,               iter=1:nits))
   f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)   x:(x+y-1)))],dimnames=list(year=yrs[-length(yrs)], iter=1:nits))
   
   bmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
   fmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
   
   return(FLQuants(biomass=sweep(b.,6,bmsy,"/"),harvest=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))}

## Projections
aspicProj<-function(file,scale="msy"){
        ## Stuff
        nits<-scan(file,skip=1,nmax=1)
        yrs <-scan(file,skip=2,nmax=2)
        t.  <-scan(file,skip=4)
        ncol<-yrs[2]-yrs[1]+2

        ## biomass
        first<-rep((1:nits-1)*ncol*2,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        b.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))
        first<-((1:nits-1)*ncol*2)+1
        bmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        b.   <-sweep(b.,6,bmsy,"/")

        ## F
        first<-rep((1:nits-1)*ncol*2+ncol,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        f.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))[,ac(yrs[1]:(yrs[2]-1))]
        first<-((1:nits-1)*ncol*2)+ncol+1
        fmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        f.   <-sweep(f.,6,fmsy,"/")

        return(FLQuants(harvest=f.,biomass=b.))}

readASPICAssess<-function(dir,scen,scale="msy"){
    res   <-mdply(scen, function(x,dir) as.data.frame(readASPIC(paste(dir,"/",scen,".bio",sep=""))), dir=dir)
    ts    <-subset(res, qname %in% c("biomass","harvest"))
    refpts<-subset(res, qname %in% c("bmsy",   "fmsy"))

    ## Quantiles in data.frame
    qtls<-transform(melt(ddply(ts,.(X1,year,qname),function(x) quantile(x$data,prob=c(0.25,0.5,0.75))),id.vars=c("X1","qname","year")),
		      Scenario=factor(X1),Quantity=factor(qname), Year=year, Quantile=variable)[,c("scenario","quantity","year","qantile","value")]

    ## Model frame with points
    ts<-cast(subset(res, qname %in% c("biomass","harvest"),select=c("X1","year","iter","data","qname")), 
		  X1+year+iter~qname,value="data")
    names(ts)<-c("Scenario","Year","iter","biomass","harvest")

    return(list(ts=ts,quantiles=qtls,refpts=refpts))}

readASPICProj<-function(dir,scen,TAC,scale="msy"){
    prj       <-subset(mdply(scen, function(scen,TAC,dir) as.data.frame(readASPIC(paste(dir,"/",scen,TAC,".prb",sep=""))), dir=dir), !is.na(data))   
    prj       <-cast(prj,scen+year+TAC+iter~qname,value="data") 
    names(prj)<-c("scenario","year","TAC","iter","biomass","harvest")

    prjP      <-cbind(prj,kobeP(prj$biomass,prj$harvest))

    prjP      <-ddply(prjP,.(scenario,year,TAC), function(x) cbind(f=mean(x$f,na.rm=T),b=mean(x$b,na.rm=T),p=mean(x$p,na.rm=T),collapsed=mean(x$collapsed)))
    return(prjP)}

.readASPIC<-function(x,type,scen="missing",scale="msy"){
  
  if (!missing(scen)){
     if (substr(tolower(type[1]),1,1)=="b") return(readASPICAssess(x,scen))
     if (substr(tolower(type[1]),1,1)=="p") return(readASPICProj(  x,scen))}
  
  if(missing(type)) type=getExt(x)

  res<-switch(substr(tolower(type[1]),1,1),
       "b"=aspicTS(  x),
       "p"=aspicProj(x))

  return(res)}
################################################################################
