#### Pro2Box ############################################################################
setGeneric("readPro2Box",  function(x,type,...)      standardGeneric("readPro2Box"))
setMethod("readPro2Box",   signature(x="character", type="character"), 
          function(x,type=c("ref","sta","out"),     files=c("BIO_f-1",  ## Biomass ? by iteration and year
                                                            "BIO_t-1",  ## Biomass ? by iteration and year
                                                            "Fapex-1",  ## F Apex by iteration and year
                                                            "RECRT-1",  ## Recuits by iteration and year
                                                            "SSBIO-1",  ## SSB by iteration and year
                                                            "SSNUM-1",  ## ? by iteration and year
                                                            "YIELD-1"), ## Yield by iteration and year
                   minyear=1,data.frame=TRUE,...) .readPro2Box(x,type,files,minyear,data.frame,rel=FALSE,...))

.readPro2Box<-function(x,type=c("ref","sta","out"),files=c("BIO_f-1",  ## Biomass ? by iteration and year
                                                           "BIO_t-1",  ## Biomass ? by iteration and year
                                                           "Fapex-1",  ## F Apex by iteration and year
                                                           "RECRT-1",  ## Recuits by iteration and year
                                                           "SSBIO-1",  ## SSB by iteration and year
                                                           "SSNUM-1",  ## ? by iteration and year
                                                           "YIELD-1"), ## Yield by iteration and year
                      minyear=1,data.frame=TRUE,rel=FALSE){

  res<-switch(type[1],
              "ref"=createRefpts(x,        data.frame),
              "sta"=pro2Sta(     x,files,minyear,data.frame),
              "out"=pro2Out(     x,files,minyear,data.frame,rel))

  return(res)}

createRefpts<-function(x,data.frame=FALSE,file="BENCH-1.OUT"){
      nmsRef <- c("Iter", "fmsy",   "msy",    "yrmsy",    "srmsy",   "sprmsy",  "bmsy",
                          "fmax",             "yrmax",    "srmax",   "sprmax",  "ssbmax",
                          "f0.1",             "yr0.1",    "sr0.1",   "spr0.1",  "ssb0.1",
                          "f20",              "yr20",     "sr20",               "ssb20",
                          "f30",              "yr30",     "sr30",               "ssb30",
                          "f40",              "yr40",     "sr40",               "ssb40",
                          "f90max", "y90max", "yr90max",  "sr90max",            "ssb90max",
                          "f75max", "y75max", "yr75max",  "sr75max",            "ssb75max")

     x         =paste(x,file,sep="/")  
     res       =read.table(x,skip=1)
     names(res)=nmsRef

     rfpts<-refpts(NA,refpt=c("msy","fmax","f0.1","spr.20","spr.30","spr.40","fmax.90","fmax.75"),iter=max(res[,1]+1))

     ## MSY
     rfpts["msy","harvest",res[,1]]<-res[,"fmsy"]
     rfpts["msy","ssb",    res[,1]]<-res[,"bmsy"]
     rfpts["msy","yield",  res[,1]]<-res[, "msy"]
     rfpts["msy","rec",    res[,1]]<-res[,"bmsy"]/res[,"srmsy"]

     ## FMax
     rfpts["fmax","harvest",res[,1]]<-res[,"fmax"]
     rfpts["fmax","ssb",    res[,1]]<-res[,"ssbmax"]
     rfpts["fmax","rec",    res[,1]]<-res[,"ssbmax"]/res[,"srmax"]
     rfpts["fmax","yield",  res[,1]]<-res[,"yrmax"]*(res[,"ssbmax"]/res[,"srmax"])

     ## F0.1
     rfpts["f0.1","harvest",res[,1]]<-res[,"f0.1"]
     rfpts["f0.1","ssb",    res[,1]]<-res[,"ssb0.1"]
     rfpts["f0.1","rec",    res[,1]]<-res[,"ssb0.1"]/res[,"sr0.1"]
     rfpts["f0.1","yield",  res[,1]]<-res[,"yr0.1"]*(res[,"ssb0.1"]/res[,"sr0.1"])

     ## F 20% SPR
     rfpts["spr.20","harvest",res[,1]]<-res[,"f20"]
     rfpts["spr.20","ssb",    res[,1]]<-res[,"ssb20"]
     rfpts["spr.20","rec",    res[,1]]<-res[,"ssb20"]/res[,"sr20"]
     rfpts["spr.20","yield",  res[,1]]<-res[,"yr20"]*(res[,"ssb20"]/res[,"sr20"])

     ## F 30% SPR
     rfpts["spr.30","harvest",res[,1]]<-res[,"f30"]
     rfpts["spr.30","ssb",    res[,1]]<-res[,"ssb30"]
     rfpts["spr.30","rec",    res[,1]]<-res[,"ssb30"]/res[,"sr30"]
     rfpts["spr.30","yield",  res[,1]]<-res[,"yr30"]*(res[,"ssb30"]/res[,"sr30"])

     ## F 40% SPR
     rfpts["spr.40","harvest",res[,1]]<-res[,"f40"]
     rfpts["spr.40","ssb",    res[,1]]<-res[,"ssb40"]
     rfpts["spr.40","rec",    res[,1]]<-res[,"ssb40"]/res[,"sr40"]
     rfpts["spr.40","yield",  res[,1]]<-res[,"yr40"]*(res[,"ssb40"]/res[,"sr40"])

     ## FMax 90%
     rfpts["fmax.90","harvest",res[,1]]<-res[,"f90max"]
     rfpts["fmax.90","ssb",    res[,1]]<-res[,"ssb90max"]
     rfpts["fmax.90","rec",    res[,1]]<-res[,"ssb90max"]/res[,"sr90max"]
     rfpts["fmax.90","yield",  res[,1]]<-res[,"yr90max"]*(res[,"ssb90max"]/res[,"sr90max"])

     ## FMax 75%
     rfpts["fmax.75","harvest",res[,1]]<-res[,"f75max"]
     rfpts["fmax.75","ssb",    res[,1]]<-res[,"ssb75max"]
     rfpts["fmax.75","rec",    res[,1]]<-res[,"ssb75max"]/res[,"sr75max"]
     rfpts["fmax.75","yield",  res[,1]]<-res[,"yr75max"]*(res[,"ssb75max"]/res[,"sr75max"])

     if (data.frame){
       res=cast(as.data.frame(rfpts), refpt+iter~quantity, mean)
       res$iter=factor(as.integer(ac(res$iter))-1)
       return(res)
     }else return(rfpts)}

pro2Sta<-function(x,files,minyear=1,data.frame=TRUE){
       getSta<-function(x,dir){ 
           res<-read.table(paste(dir,x,sep=.Platform$file.sep),skip=1,header=F)
           names(res)<-c("scen","year","lowerCL","Median","upperCL","mean","det","sd")
           return(res)}

       res<-mdply(paste(files,"STA",sep="."),getSta,dir=x)

       X1<-c("biomFish","biomass","fapex","rec","ssb","ssn","yield")
       names(X1)<-1:7
       res$X1<-X1[res$X1]
       names(res)[1]<-"quantity"

       if (!data.frame){

  tmp<-melt(res,id.vars=c("quantity","scen","year"))
         names(tmp)[4:5]<-c("quant","data")

         res2<-array(list(),dim=c(length(unique(res[,"scen"])),7),
                                dimnames=list(scen=unique(res[,"scen"]),
                                              val =c("biomFish","biomass","fapex","rec","ssb","ssn","yield")))

         tmp<-dlply(tmp,.(quantity,scen),function(x) as.FLQuant(x[,3:5]))
         k<-0
         for (i in dimnames(res2)[[1]])
            for (j in dimnames(res2) [[2]]){
               k<-k+1

               res2[[i,j]]<-tmp[[k]]}

          return(res2)}

       return(res)}

pro2Out<-function(x,files,minyear=1,data.frame=TRUE,rel=FALSE){
      files=paste(files,"OUT",sep=".")

       res1<-read.table(paste(x,files[1],sep="/"))
       res2<-read.table(paste(x,files[2],sep="/"))
       res3<-read.table(paste(x,files[3],sep="/"))
       res4<-read.table(paste(x,files[4],sep="/"))
       res5<-read.table(paste(x,files[5],sep="/"))
       res6<-read.table(paste(x,files[6],sep="/"))
       res7<-read.table(paste(x,files[7],sep="/"))

       res<-data.frame(scen     =rep(res1[,1],                        dim(res1)[2]-2),
                       iter     =rep(res1[,2],                        dim(res1)[2]-2),
                       year     =rep((1:(dim(res1)[2]-2))+(minyear-1),each=dim(res1)[1]  ),
                       biomFish =unlist(res1[,3:dim(res1)[2]]),
                       biomass  =unlist(res2[,3:dim(res1)[2]]),
                       fapex    =unlist(res3[,3:dim(res1)[2]]),
                       rec      =unlist(res4[,3:dim(res1)[2]]),
                       ssb      =unlist(res5[,3:dim(res1)[2]]),
                       ssn      =unlist(res6[,3:dim(res1)[2]]),
                       yield    =unlist(res7[,3:dim(res1)[2]]))

       if (rel) {;}
                       oogle.es/oogle.es/
       if (!data.frame){
          res2<-array(list(),dim=c(length(unique(res[,"scen"])),7),
                                dimnames=list(scen=unique(res[,"scen"]),
                                              val =c("biomFish","biomass","fapex","rec","ssb","ssn","yield")))

          for (i in dimnames(res2)[2]){
             res3<-res[,c("scen","year","iter",i)]
             names(res3)[4]<-"data"

          res2[,i]<-dlply(res3, "scen", function(x) as.FLQuant(x[,c("iter","year","data")]))}
          return(res2)}

       return(res)}