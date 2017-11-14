require(glmnet)
require(pls)

getWeightedLasso = function(df,ind) {
  res = list()
  x = model.matrix(form2,data=df)[ind,-1]
  y=df$PX_LAST[ind]
  hl = 52
  delta = 0.5^(1/hl)
  l = length(ind)
  weight = rep(NA,l)
  for (i in 1:l) {
    weight[i] = delta^(l-i)
  }
  grid = 2.716^seq(-3,1,length=100)
  cv.out = cv.glmnet(x,y,alpha=1,weights=weight)
  best.lam = cv.out$lambda.min
  lasso = glmnet(x,y,alpha=1,lambda = grid,weights=weight)
  #plot(lasso,xvar="lambda")
  coef_ = predict(lasso,s=best.lam,type="coefficients")[1:11,]
  res = list(
    model = lasso,
    coef = coef_
  )
  res
}
