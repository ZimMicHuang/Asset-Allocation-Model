require(lubridate)

fm.lm = list(
  IT.lm = lm(form1,smoothDat$IT[last.5y,]),
  FIN.lm = lm(form1,smoothDat$FIN[last.5y,]),
  ENG.lm = lm(form1,smoothDat$ENG[last.5y,]),
  COND.lm = lm(form1,smoothDat$COND[last.5y,]),
  CONS.lm = lm(form1,smoothDat$CONS[last.5y,]),
  HLTH.lm = lm(form1,smoothDat$HLTH[last.5y,]),
  UTIL.lm = lm(form1,smoothDat$UTIL[last.5y,]),
  INDU.lm = lm(form1,smoothDat$INDU[last.5y,]),
  MATR.lm = lm(form1,smoothDat$MATR[last.5y,]),
  TELS.lm = lm(form1,smoothDat$TELS[last.5y,])
)


plotLoading.lm = function(sector,df) {
  # uses formula form1
  # plots a time series of factor exposures
  # this is faily slow: takes ~0.7 seconds to fit a single model
  name = as.name(sector)
  exp = parse(text=paste("fm.lm$",sector,".lm$coefficients",sep=""))
  coef = eval(exp)
  bestLam = eval(exp1)
  coef.mat = as.data.frame(t(coef))
  N = nrow(df)-261
  M = 260
  for (i in 1:N) {
    ind_ = i:(i+M)
    rsp = lm(form1,data=df[ind_,])
    coef.mat = rbind(coef.mat,as.vector(rsp$coefficients))
    print(dim(coef.mat))
  }
  ##PLOT
  cols = rainbow(ncol(coef.mat))
  plot(coef.mat[,1],type="l",ylim=c(-8,8),main=sector,lwd=2)
  for (i in 1:ncol(coef.mat)) {
    lines(coef.mat[,i],col=cols[i],lwd=2)
    legend(x=0,y=10-2*i,colnames(coef.mat)[i],cols[i],cex=0.8)
  }
  abline(0,0)
}
plotLoading.lm("IT",smoothDat$IT)
plotLoading.lm("FIN",smoothDat$FIN)
plotLoading.lm(sector="ENG",smoothDat$ENG)
plotLoading.lm(sector="HLTH",smoothDat$HLTH)
plotLoading.lm(sector="CONS",smoothDat$CONS)
plotLoading.lm(sector="COND",smoothDat$COND)
plotLoading.lm(sector="UTIL",smoothDat$UTIL)
plotLoading.lm(sector="INDU",smoothDat$INDU)
plotLoading.lm(sector="MATR",smoothDat$MATR)
plotLoading.lm(sector="TELS",smoothDat$TELS)
