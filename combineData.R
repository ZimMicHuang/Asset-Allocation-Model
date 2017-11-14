combDat = function(df,sector="IT",index=1:nrow(df)) {
  # combines sector price, sector factors, and broad factors
  # output is a convenience dataframe for ease of running regressions and validations
  # the dataframe is scaled
  exp = parse(text=paste("sectorPrice$",sector,".price",sep=""))
  price = eval(exp)
  index = intersect(rownames(df),rownames(price))
  res = cbind(df[index,],
              PX_LAST = price[index,],
              DXY = macro$DXY[index,],
              T5 = macro$T5[index,],
              BCom = macro$BCom[index,],
              VIX = macro$VIX[index,],
              GDP = macro$GDP[index,],
              PCE = macro$PCE[index,],
              CPI = macro$CPI[index,],
              UEP = macro$UEP[index,],
              Spread = macro$Spread[index,],
              Housing = macro$Housing[index,],
              EEM = macro$EEM[index,])
  res=scale(res)
  res = as.data.frame(res)
  res
}

getCombDat = function() {
  res = list(
    IT = combDat(factor_data_list$IT,"IT"),
    FIN = combDat(factor_data_list$FIN,"FIN"),
    ENG = combDat(factor_data_list$ENG,"ENG"),
    HLTH = combDat(factor_data_list$HLTH,"HLTH"),
    CONS = combDat(factor_data_list$CONS,"CONS"),
    COND = combDat(factor_data_list$COND,"COND"),
    UTIL = combDat(factor_data_list$UTIL,"UTIL"),
    MATR = combDat(factor_data_list$MATR,"MATR"),
    INDU = combDat(factor_data_list$INDU,"INDU"),
    TELS = combDat(factor_data_list$TELS,"TELS")
  )  
  res
}
combDat = getCombDat()
head(combDat$FIN)
tail(combDat$FIN)
