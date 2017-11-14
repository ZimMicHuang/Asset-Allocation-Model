require(fastICA)
require(caret)
require(forecast)
fica = fastICA(smoothDat$IT[,factors],n.comp = 5,method="R")
names(fica)
fica$X
fica$S %*% fica$A == fica$X
colnames(fica$A) = factors
colnames(fica$S) = c("IC1","IC2","IC3","IC4","IC5")
s = fica$S
dim(fica$S)  
dim(fica$A)
dim(fica$X)
dim(fica$W)
dim(fica$K)

l1 = lm(PX_LAST~0+s[,"IC1"]+s[,"IC2"]+s[,"IC3"]+s[,"IC4"]+s[,"IC5"],data=smoothDat$IT)
summary(l1)
l1$coefficients
t(fica$A) %*% l1$coefficients



######## Tuning for number of params
form1 = as.formula("PX_LAST~Value+Size+Growth+Yield+Quality+Vol+Momentum+spx")




tg = expand.grid(n.comp=1:8)
trCtrl = trainControl(method="timeslice",initialWindow = 251,horizon=80,fixedWindow = TRUE,verboseIter = TRUE)

xreg = df[,factors]
ret = df$PX_LAST


ica = caret::train(xreg,ret,method="icr",tuneGrid=tg,trControl=trCtrl)
ica$bestTune









####### 5 ica for each
df = smoothDat$TELS
ind = last.5y


getICR = function(df,ind) {
  # regression using independent component analysis. 
  # 5 component is chosen, arbitrarily. Cross validation suggests the full OLS fit.
  fica = fastICA(df[ind,factors],n.comp=5,method="C")
  # XKW = S; X = SA
  src = fica$S
  colnames(src) = paste0("IC",1:5) # each column is one source
  unmix = fica$A
  colnames(unmix) = factors 
  # each column is the unmixing combination for a factor
  # i.e. how to get to f1 from s1,s2,...,sn
  mix = fica$K %*% fica$W
  colnames(mix) = paste0("IC",1:5) 
  # each column is the mixing comb for a source
  # i.e. how to get to s1 from f1,f2,...,fn
  icr = lm(PX_LAST~0+src[,"IC1"]+src[,"IC2"]+src[,"IC3"]+src[,"IC4"]+src[,"IC5"],data=df[ind,] )
  summary(icr)
  coef_ = mix %*%  icr$coefficients
  # rotate back the coefficients. KWb.  
  rownames(coef_) = factors
  res = list(
    ica = fica,
    coef = coef_,
    lm = icr
  )
}

fm.ICR = list(
    IT = getICR(smoothDat$IT,ind=last.5y),
    FIN = getICR(smoothDat$FIN,ind=last.5y),
    ENG = getICR(smoothDat$ENG,ind=last.3y),
    COND = getICR(smoothDat$COND,ind=last.5y),
    CONS = getICR(smoothDat$CONS,ind=last.5y),
    HLTH = getICR(smoothDat$HLTH,ind=last.5y),
    UTIL = getICR(smoothDat$UTIL,ind=last.5y),
    INDU = getICR(smoothDat$INDU,ind=last.5y),
    MATR = getICR(smoothDat$MATR,ind=last.5y),
    TELS = getICR(smoothDat$TELS,ind=last.1y)
)



plotLoading.icr = function(sector,df) {
  # plot time series of factor loadings
  # uses formula 1
  dates = seq.Date(from=as.Date("2002-03-15"),length.out = 551,by="week")
  name = as.name(sector)
  exp = parse(text=paste("fm.ICR$",sector,"$coef",sep=""))
  icr.coef = as.data.frame(eval(exp))
  coef.mat = as.data.frame(t(icr.coef))
  N = nrow(df)-261
  M = 260
  for (i in 1:N) {
    ind_ = i:(i+M)
    rsp.pr = getICR(df,ind=ind_)
    coef.mat = rbind(coef.mat,t(rsp.pr$coef))
    #print(dim(coef.mat))
  }
  ##PLOT
  cols = rainbow(ncol(coef.mat))
  plot(x=dates,
       y=coef.mat[-(1:12),1],type="l",
       ylim=c(-1.5,1.5),
       main=sector,
       lwd=2,
       xlab="5 Year Period Starting",
       ylab="Factor Exposures")
  legend("topleft",legend = colnames(coef.mat),col=cols[1:ncol(coef.mat)],cex=1,pch=15)
  
  for (i in 1:ncol(coef.mat)) {
    lines(dates,coef.mat[-(1:12),i],col=cols[i],lwd=2)
  }
    abline(0,0)
  
  coef.mat
}


coef.IT = plotLoading.icr(sector="IT",df = smoothDat$IT)
coef.FIN = plotLoading.icr(sector="FIN",df = smoothDat$FIN)
coef.ENG = plotLoading.icr(sector="ENG",df = smoothDat$ENG)
coef.HLTH = plotLoading.icr(sector="HLTH",df = smoothDat$HLTH)
coef.CONS = plotLoading.icr(sector="CONS",df = smoothDat$CONS)
coef.COND = plotLoading.icr(sector="COND",df = smoothDat$COND)
coef.UTIL = plotLoading.icr(sector="UTIL",df = smoothDat$UTIL)
coef.INDU = plotLoading.icr(sector="INDU",df = smoothDat$INDU)
coef.MATR = plotLoading.icr(sector="MATR",df = smoothDat$MATR)
coef.TELS = plotLoading.icr(sector="TELS",df = smoothDat$TELS)


r.sq = lapply(fm.ICR,function(df) {summary(df$lm)$r.squared})
write.csv(r.sq,"tmp.csv")










