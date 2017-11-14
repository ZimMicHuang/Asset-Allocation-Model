


form1 = as.formula("PX_LAST~Value+Size+Growth+Yield+Quality+Vol+Momentum+spx")
df = smoothDat$IT
ind = last.5y
tg = expand.grid(.alpha=1,.lambda=seq(0,0.4,length=100))
trCtrl = trainControl(method="timeslice",initialWindow = 251,horizon=80,fixedWindow = TRUE,verboseIter = TRUE)


xreg = df[,factors]
ret = df$PX_LAST
lasso = caret::train(form1,data=df,method="glmnet",trControl = trCtrl,tuneGrid=tg)
lam = lasso$bestTune$lambda

plot(lasso)

getLas2 = function(df,ind,f) {
  tg = expand.grid(.alpha=1,.lambda=seq(0.01,0.4,length=100))
  trCtrl = trainControl(method="timeslice",initialWindow = 251,horizon=160,fixedWindow = TRUE,verboseIter = TRUE)
  lasso = caret::train(form1,data=df,method="glmnet",trControl = trCtrl,tuneGrid=tg)
  lam = lasso$bestTune$lambda
  
  res = list()
  x = model.matrix(f,data=df)[ind,-1]
  y=df$PX_LAST[ind]
  
  las = glmnet(x,y,alpha=1,lambda = lam)
  coef_ = as.matrix(las$beta)
  res = list(
    model = las,
    coef = coef_,
    bestLam = lam
  )
  res
}
fm4 = list(
  IT.las = getLas2(smoothDat$IT,last.5y,f=form1),
  FIN.las = getLas2(smoothDat$FIN,last.5y,f=form1),
  ENG.las = getLas2(smoothDat$ENG,last.5y,f=form1),
  COND.las = getLas2(smoothDat$COND,last.5y,f=form1),
  CONS.las = getLas2(smoothDat$CONS,last.5y,f=form1),
  HLTH.las = getLas2(smoothDat$HLTH,last.5y,f=form1),
  UTIL.las = getLas2(smoothDat$UTIL,last.5y,f=form1),
  INDU.las = getLas2(smoothDat$INDU,last.5y,f=form1),
  MATR.las = getLas2(smoothDat$MATR,last.5y,f=form1),
  TELS.las = getLas2(smoothDat$TELS,last.5y,f=form1)
)
plotLoading.Lasso = function(sector,df) {
  # uses formula form1
  # plots a time series of factor exposures
  # this is faily slow: takes ~0.7 seconds to fit a single model
  name = as.name(sector)
  exp = parse(text=paste("fm3$",sector,".las$coef",sep=""))
  exp1 = parse(text=paste("fm3$",sector,".las$bestLam",sep=""))
  coef = eval(exp)
  bestLam = eval(exp1)
  coef.mat = as.data.frame(t(coef))
  N = nrow(df)-261
  M = 260
  for (i in 1:N) {
    ind_ = i:(i+M)
    x = model.matrix(f,data=df)[ind_,-1]
    y=df$PX_LAST[ind_]
    rsp.las = glmnet(x,y,alpha=1,lambda=bestLam)
    coef.mat = rbind(coef.mat,as.vector(rsp.las$beta))
    print(dim(coef.mat))
  }
  ##PLOT
  cols = rainbow(ncol(coef.mat))
  plot(coef.mat[,1],type="l",ylim=c(-3,3),main=sector,lwd=2)
  for (i in 1:ncol(coef.mat)) {
    lines(coef.mat[,i],col=cols[i],lwd=2)
    legend(x=0,y=3-i/4,colnames(coef.mat)[i],cols[i],cex=0.8)
  }
  abline(0,0)
}
plotLoading.Lasso("IT",smoothDat$IT)
plotLoading.Lasso("FIN",smoothDat$FIN)
plotLoading.Lasso(sector="ENG",smoothDat$ENG)
plotLoading.Lasso(sector="HLTH",smoothDat$HLTH)
plotLoading.Lasso(sector="CONS",smoothDat$CONS)
plotLoading.Lasso(sector="COND",smoothDat$COND)
plotLoading.Lasso(sector="UTIL",smoothDat$UTIL)
plotLoading.Lasso(sector="INDU",smoothDat$INDU)
plotLoading.Lasso(sector="MATR",smoothDat$MATR)
plotLoading.Lasso(sector="TELS",smoothDat$TELS)



######################## Polynomially Weighted Lasso ################

# 0.5^(-halflife of memory) --- 18 weeks
# form1 = as.formula("PX_LAST~Value+Size+Growth+Yield+Quality+Vol+Momentum+spx")
# form2 = as.formula("PX_LAST~DXY+T5+BCom+GDP+PCE+CPI+UEP+Spread+Housing+Quality+Value+Growth+Momentum+spx")
# getWeightedLasso = function(df,ind,f) {
#   res = list()
#   x = model.matrix(f,data=df)[ind,-1]
#   y=df$PX_LAST[ind]
#   hl = 52
#   delta = 0.5^(1/hl)
#   l = length(ind)
#   weight = rep(NA,l)
#   for (i in 1:l) {
#     weight[i] = delta^(l-i)
#   }
#   grid = seq(0.001,0.1,length=50)
#   #cv.out = cv.glmnet(x,y,alpha=1,weights=weight,lambda=grid)
#   cv.out = cv.glmnet(x,y,alpha=1,lambda=grid)
#   best.lam = cv.out$lambda.min
#   #lasso = glmnet(x,y,alpha=1,lambda = grid,weights=weight)
#   lasso = glmnet(x,y,alpha=1,lambda = grid)
#   #plot(lasso,xvar="lambda")
#   coef_ = predict(lasso,s=best.lam,type="coefficients")[1:9,]
#   res = list(
#     model = lasso,
#     coef = coef_,
#     bestLam = best.lam
#   )
#   res
# }
# 
# fm1 = list(
#   IT.wl = getWeightedLasso(smoothDat$IT,last.3y,f=form1),
#   FIN.wl = getWeightedLasso(smoothDat$FIN,last.3y,f=form1),
#   ENG.wl = getWeightedLasso(smoothDat$ENG,last.3y,f=form1),
#   COND.wl = getWeightedLasso(smoothDat$COND,last.3y,f=form1),
#   CONS.wl = getWeightedLasso(smoothDat$CONS,last.3y,f=form1),
#   HLTH.wl = getWeightedLasso(smoothDat$HLTH,last.3y,f=form1),
#   UTIL.wl = getWeightedLasso(smoothDat$UTIL,last.3y,f=form1),
#   INDU.wl = getWeightedLasso(smoothDat$INDU,last.3y,f=form1),
#   MATR.wl = getWeightedLasso(smoothDat$MATR,last.3y,f=form1),
#   TELS.wl = getWeightedLasso(smoothDat$TELS,last.3y,f=form1)
# )
# 
# 
# ##### Time Series plot
# 
# plotLoading.weightedLasso = function(sector,df) {
#   # uses formula form1
#   # plots a time series of factor exposures
#   # this is faily slow: takes ~0.7 seconds to fit a single model 
#   name = as.name(sector)
#   exp = parse(text=paste("fm1$",sector,".wl$coef",sep=""))
#   coef = eval(exp)
#   coef.mat = as.data.frame(t(coef))
#   N = nrow(df)-261
#   M = 260
#   for (i in 1:N) {
#     ind_ = i:(i+M)
#     rsp.wl = getWeightedLasso(df,ind=ind_,f=form1)
#     coef.mat = rbind(coef.mat,rsp.wl$coef)
#     print(dim(coef.mat))
#   }
#   ##PLOT
#   cols = rainbow(ncol(coef.mat))
#   plot(coef.mat[,1],type="l",ylim=c(-2,2),main=sector,lwd=2)
#   for (i in 1:ncol(coef.mat)) {
#     lines(coef.mat[,i],col=cols[i],lwd=2)
#     legend(x=0,y=2-i/8,colnames(coef.mat)[i],cols[i],cex=0.8)
#   }
#   abline(0,0)
# }
# plotLoading.weightedLasso("IT",smoothDat$IT)
# plotLoading.weightedLasso("FIN",smoothDat$FIN)
# plotLoading.weightedLasso(sector="ENG",smoothDat$ENG)
# plotLoading.weightedLasso(sector="HLTH",smoothDat$HLTH)
# plotLoading.weightedLasso(sector="CONS",smoothDat$CONS)
# plotLoading.weightedLasso(sector="COND",smoothDat$COND)
# plotLoading.weightedLasso(sector="UTIL",smoothDat$UTIL)
# plotLoading.weightedLasso(sector="INDU",smoothDat$INDU)
# plotLoading.weightedLasso(sector="MATR",smoothDat$MATR)
# plotLoading.weightedLasso(sector="TELS",smoothDat$TELS)
# 
# 
