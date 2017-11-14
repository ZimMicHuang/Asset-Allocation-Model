#################################################################
############################# PCR ###############################
# takes away VIX as a factor because it is spurious
form2 = as.formula("PX_LAST~DXY+T5+BCom+GDP+PCE+CPI+UEP+Spread+Housing+EEM+Quality+Value+Growth+Momentum+spx")
factors = c("Value","Size","Growth","Yield","Quality","Vol","Momentum","spx")
getPCR = function(df,ind) {
  # exclude VIX in the modelling because its intepretation is wierd
  # vix.del = which(colnames(df)=="VIX")
  # pca = prcomp(df[ind,-c(1,vix.del)])
  pca = prcomp(df[ind,factors])
  # uses two principle components
  lm2 = lm(PX_LAST~0+pca$x[,"PC1"]+pca$x[,"PC2"]+pca$x[,"PC3"]+pca$x[,"PC4"],data=df[ind,])
  summary(lm2)
  coef.pcr = lm2$coefficients[1:4]
  rotat.prcomp = pca$rotation[,1:4]
  coef.lm = rotat.prcomp %*% coef.pcr
  # Restrict factors to the top 6 most important ones
  nth_largest= function(vect, n){
    vect[rev(order(vect))][n]
  }
  res = list(
    model = lm2,
    coef = coef.lm,
    pc = pca
  )
}

fm2 = list(
  IT.pr = getPCR(smoothDat$IT,ind=last.5y),
  FIN.pr = getPCR(smoothDat$FIN,ind=last.5y),
  ENG.pr = getPCR(smoothDat$ENG,ind=last.3y),
  COND.pr = getPCR(smoothDat$COND,ind=last.5y),
  CONS.pr = getPCR(smoothDat$CONS,ind=last.5y),
  HLTH.pr = getPCR(smoothDat$HLTH,ind=last.5y),
  UTIL.pr = getPCR(smoothDat$UTIL,ind=last.5y),
  INDU.pr = getPCR(smoothDat$INDU,ind=last.5y),
  MATR.pr = getPCR(smoothDat$MATR,ind=last.5y),
  TELS.pr = getPCR(smoothDat$TELS,ind=last.3y)
)


plotLoading.pr = function(sector,df) {
  # plot time series of factor loadings
  # uses formula 1
  name = as.name(sector)
  exp = parse(text=paste("fm2$",sector,".pr$coef",sep=""))
  pr.coef = as.data.frame(eval(exp))
  coef.mat = as.data.frame(t(pr.coef))
  N = nrow(df)-261
  M = 260
  for (i in 1:N) {
    ind_ = i:(i+M)
    rsp.pr = getPCR(df,ind=ind_)
    coef.mat = rbind(coef.mat,t(rsp.pr$coef))
    #print(dim(coef.mat))
  }
  ## take out insignificant loadings, as defined by:
  # can't have an IQR range greater than 40% --- change in magnitude
  # can't have more than 4 times crossing 0 
  # cutOff.sd = 0.13
  # cutOff.flip = 12
  cutOff.0 = 0.05
  # countSignChange = function(s) {
  #   count = 0
  #   curr = s[1]
  #   for (i in 2:length(s)) {
  #     if (curr*s[i] < 0) {
  #       count = count + 1
  #     }
  #     curr = s[i]
  #   }
  #   count
  # }
  # 
  # for (c in 1:ncol(coef.mat)) {
  #   sd = sd(coef.mat[,c])
  #   if (sd > cutOff.sd) {
  #     coef.mat[,c] = rep(0,times=nrow(coef.mat))
  #   }
  #   count = countSignChange(coef.mat[,c])
  #   if (count >= cutOff.flip) {
  #     coef.mat[,c] = rep(0,times=nrow(coef.mat))
  #   }
  # }
  # for (c in 1:ncol(coef.mat)) {
  #   coef.mat[,c][which(abs(coef.mat[,c]) < cutOff.0)] = 0
  # }
  ##PLOT
  cols = rainbow(ncol(coef.mat))
  plot(coef.mat[,1],type="l",ylim=c(-1,1),main=sector,lwd=2)
  for (i in 1:ncol(coef.mat)) {
    lines(coef.mat[,i],col=cols[i],lwd=2)
    legend(x=0,y=1.15-i/4,colnames(coef.mat)[i],cols[i],cex=0.8)
  }
  abline(0,0)
  
  coef.mat
}


coef.IT = plotLoading.pr(sector="IT",df = smoothDat$IT)
coef.FIN = plotLoading.pr(sector="FIN",df = smoothDat$FIN)
coef.ENG = plotLoading.pr(sector="ENG",df = smoothDat$ENG)
coef.HLTH = plotLoading.pr(sector="HLTH",df = smoothDat$HLTH)
coef.CONS = plotLoading.pr(sector="CONS",df = smoothDat$CONS)
coef.COND = plotLoading.pr(sector="COND",df = smoothDat$COND)
coef.UTIL = plotLoading.pr(sector="UTIL",df = smoothDat$UTIL)
coef.INDU = plotLoading.pr(sector="INDU",df = smoothDat$INDU)
coef.MATR = plotLoading.pr(sector="MATR",df = smoothDat$MATR)
coef.TELS = plotLoading.pr(sector="TELS",df = smoothDat$TELS)

r.sq = lapply(fm2,function(df) {summary(df$model)$r.squared})

