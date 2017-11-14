getDiff = function() {
  # difference certain items in the combined dataset to get incremental values
  diffDat = function(df) {
    diffItems = c(
      "PX_LAST",
      "INDX_WEIGHTED_BOOK_VAL",
      "INDX_WEIGHTED_EST_ERN",
      "T12_EPS_AGGTE",
      "RETURN_ON_ASSET",
      "RETURN_COM_EQY",
      "OPER_MARGIN",
      "EEM",
      "Housing",
      "VIX",
      "BCom",
      "DXY"
    )
    for (item in diffItems) {
      exp = parse(text=paste("df$",item,sep=""))
      df[,item] = c(0,diff(eval(exp)))
    }
    df = df[-1,]
    df
  }
  res = list(
    IT = diffDat(combDat$IT),
    FIN = diffDat(combDat$FIN),
    ENG = diffDat(combDat$ENG),
    HLTH = diffDat(combDat$HLTH),
    CONS = diffDat(combDat$CONS),
    COND = diffDat(combDat$COND),
    UTIL = diffDat(combDat$UTIL),
    MATR = diffDat(combDat$MATR),
    INDU = diffDat(combDat$INDU),
    TELS = diffDat(combDat$TELS)
  )
  res
}
diffDat = getDiff()

