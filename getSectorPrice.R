getSectorPrice = function() {
  # get sector log prices from the "SectorData1.csv" file.   
  require(lubridate)
  clean = function(df) {
    colnames(df) = as.character(df[1,])
    rownames(df) = df[,1]
    df = df[-1, ,drop=FALSE]
    df=df[,-1,drop=FALSE]
    rownames(df)[1] <- "1/4/2002"
    dates = mdy(rownames(df))
    rownames(df) = dates
    #print(head(df))
    df
  }
  numerify = function(df) {
    for (c in 1:ncol(df)) {
      df[,c] = as.numeric(df[,c])
    }
    df
  }
  dat = read.csv("SectorData1.csv",colClasses = "character")
  IT = dat[,1:22]
  FIN = dat[,24:45]
  ENG = dat[,47:68]
  HLTH = dat[,70:91]
  CONS = dat[,93:114]
  COND = dat[,116:137]
  INDU = dat[,139:160]
  UTIL = dat[,162:183]
  TELS = dat[,185:206]
  MATR = dat[,208:229]
  IT = clean(IT)
  FIN = clean(FIN)
  ENG = clean(ENG)
  HLTH = clean(HLTH)
  CONS = clean(CONS)
  COND = clean(COND)
  INDU = clean(INDU)
  UTIL = clean(UTIL)
  TELS = clean(TELS)
  MATR = clean(MATR)
  IT = numerify(IT)
  FIN = numerify(FIN)
  ENG = numerify(ENG)
  HLTH = numerify(HLTH)
  CONS = numerify(CONS)
  COND = numerify(COND)
  INDU = numerify(INDU)
  UTIL = numerify(UTIL)
  TELS = numerify(TELS)
  MATR = numerify(MATR)
  res = list(
    IT.price = log(IT[,"PX_LAST",drop=FALSE]),
    FIN.price = log(FIN[,"PX_LAST",drop=FALSE]),
    ENG.price = log(ENG[,"PX_LAST",drop=FALSE]),
    HLTH.price = log(HLTH[,"PX_LAST",drop=FALSE]),
    CONS.price = log(CONS[,"PX_LAST",drop=FALSE]),
    COND.price = log(COND[,"PX_LAST",drop=FALSE]),
    INDU.price = log(INDU[,"PX_LAST",drop=FALSE]),
    UTIL.price = log(UTIL[,"PX_LAST",drop=FALSE]),
    TELS.price = log(TELS[,"PX_LAST",drop=FALSE]),
    MATR.price = log(MATR[,"PX_LAST",drop=FALSE])
  )
}
sectorPrice = getSectorPrice()
