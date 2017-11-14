smooth = function(df) {
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
  nonDiff = setdiff(colnames(df),diffItems)
  for (item in diffItems) {
    exp = parse(text=paste("df$",item,sep=""))
    df[,item] = c(rep(NA,times=7),rollapply(eval(exp),FUN=sum,width = 8))
  }
  for (item in nonDiff) {
    exp = parse(text=paste("df$",item,sep=""))
    df[,item] = c(rep(NA,times=7),rollapply(eval(exp),FUN=mean,width = 8))
  }
  df = na.omit(df)
  df
}
head(df)
