require(plyr)
require(zoo)
require(stats)
require(pls)
require(fBasics)
require(glmnet)
require(caret)
require(ggplot2)
# 
# > dim(smoothDat$IT)
# [1] 816  16
last.1y = 762:815
last.3y = 661:815
last.5y = 565:815
all = 1:816
factors = c("Value","Growth","Momentum","Quality","Size","Vol","Yield","spx")


# Get msci factor return index data
getMSCI = function() {
  require(lubridate)
  # get data from "msci1.csv"; all prices are log transformed
  
  #get data
  msci = read.csv("msci1.csv",colClasses = "character")
  momentum = msci[,1:2]
  value = msci[,4:5]
  quality = msci[,7:8]
  size = msci[,10:11]
  growth = msci[,13:14]
  vol = msci[,16:17]
  yield = msci[,19:20]
  
  
  clean = function(df) {
    df = df[1:826,]
    colnames(df) = as.character(df[1,])
    rownames(df) = df[,1]
    df = df[-1, ,drop=FALSE]
    df=df[,-1,drop=FALSE]
    df = df[1:(nrow(df)-1),,drop=FALSE]
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
  
  res = list(
    quality = log(numerify(clean(quality))),
    growth = log(numerify(clean(growth))),
    value = log(numerify(clean(value))),
    momentum = log(numerify(clean(momentum))),
    yield = log(numerify(clean(yield))),
    size = log(numerify(clean(size))),
    vol = log(numerify(clean(vol)))
  )
}
MSCI = getMSCI()


# get sector return index data
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

# Get macroeconomic data
getBroad = function() {
  # Read and Clean Data in the macroGDP.csv data set
  # Read and Clean Data in the EEM.csv data set
  # Read and Clean Data in the SectorData1.csv data set
  # DXY, VIX, Commodity Index, EEM and Housing Index are logged
  require(lubridate)
  
  macro = read.csv("macroGDP.csv",na.strings=c("","NA"),colClasses = "character")
  EEM = read.csv("EEM.csv",na.strings=c("","NA"),colClasses = "character")
  dat = read.csv("SectorData1.csv",colClasses = "character")
  
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
  
  econ2 = dat[,243:253]
  DXY = econ2[,1:2]
  T5 = econ2[,4:5]
  BCom = econ2[,7:8]
  VIX = econ2[,10:11]
  
  GDP = na.omit(macro[,1:2])
  PCE = na.omit(macro[,3:4])
  CPI = na.omit(macro[,5:6])
  UEP = na.omit(macro[,7:8])
  Spread = na.omit(macro[,9:10])
  Housing = na.omit(macro[,11:12])
  
  
  GDP_ = numerify(clean(GDP))
  PCE_ = numerify(clean(PCE))
  CPI_ = numerify(clean(CPI))
  UEP_ = numerify(clean(UEP))
  Spread_ = numerify(clean(Spread))
  Housing_ = log(numerify(clean(Housing)))
  EEM_ = log(numerify(clean(EEM)))
  
  DXY_ = log(numerify(clean(DXY)))
  T5_ = numerify(clean(T5))
  BCom_ = log(numerify(clean(BCom)))
  VIX_ = log(numerify(clean(VIX)))
  
  res = list(
    GDP = GDP_,
    PCE = PCE_,
    CPI = CPI_,
    UEP = UEP_,
    Spread = Spread_,
    Housing = Housing_,
    EEM = EEM_,
    DXY = DXY_,
    T5 = T5_,
    BCom = BCom_,
    VIX = VIX_
  )
}
macro = getBroad()

# get market return
getSPX = function() {
  require(lubridate)
  # get data in the "SPX.csv" file and log transform the prices
  
  #get data
  spx = read.csv("SPX.csv",colClasses = "character")
  
  
  clean = function(df) {
    colnames(df) = as.character(df[1,])
    rownames(df) = df[,1]
    df = df[-1, ,drop=FALSE]
    df=df[,-1,drop=FALSE]
    #df = df[1:(nrow(df)),,drop=FALSE]
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
  
  res = list(
    spx = log(numerify(clean(spx)))
  )
}
spx = getSPX()

#combine data
combDat = function(sector) {
  # combines the dataframe with it's sector price, and broad factors
  # output is a convenience dataframe for ease of running regressions and validations
  # the dataframe is NOT!!! scaled
  exp = parse(text=paste("sectorPrice$",sector,".price",sep=""))
  price = eval(exp)
  index = intersect(rownames(MSCI$quality),rownames(price))
  res = cbind(PX_LAST = price[index,,drop=FALSE],
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
              EEM = macro$EEM[index,],
              Quality = MSCI$quality[index,],
              Value = MSCI$value[index,],
              Growth = MSCI$growth[index,],
              Momentum = MSCI$momentum[index,],
              Size = MSCI$size[index,],
              Vol = MSCI$vol[index,],
              Yield = MSCI$yield[index,],
              spx = spx$spx[index,])
  res = as.data.frame(res)
  res
}
getCombDat = function() {
  res = list(
    IT = combDat("IT"),
    FIN = combDat("FIN"),
    ENG = combDat("ENG"),
    HLTH = combDat("HLTH"),
    CONS = combDat("CONS"),
    COND = combDat("COND"),
    UTIL = combDat("UTIL"),
    MATR = combDat("MATR"),
    INDU = combDat("INDU"),
    TELS = combDat("TELS")
  )  
  res
}
combDat = getCombDat()

# Diff data
getDiff = function() {
  # difference certain items in the combined dataset to get incremental values
  diffDat = function(df) {
    diffItems = c(
      "PX_LAST",
      "Quality",
      "Value",
      "Momentum",
      "Growth",
      "Size",
      "Vol",
      "Yield",
      "EEM",
      "Housing",
      "VIX",
      "BCom",
      "DXY",
      "spx"
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


# get excess market returns
getDeMarket = function() {
  # get excess factor returns and excess sector returns
  # the data frame is NOT!!! scaled
  deMarket = function(df) {
    diffItems = c(
      # "Quality",
      # "Value",
      # "Momentum",
      # "Growth",
      # "EEM"
    )
    for (item in diffItems) {
      exp1 = parse(text=paste("df$",item,sep=""))
      df[,item] = eval(exp1) - df$spx
    }
    df
  }
  res = list(
    IT = deMarket(diffDat$IT),
    FIN = deMarket(diffDat$FIN),
    ENG = deMarket(diffDat$ENG),
    HLTH = deMarket(diffDat$HLTH),
    CONS = deMarket(diffDat$CONS),
    COND = deMarket(diffDat$COND),
    UTIL = deMarket(diffDat$UTIL),
    MATR = deMarket(diffDat$MATR),
    INDU = deMarket(diffDat$INDU),
    TELS = deMarket(diffDat$TELS)
  )
}
deMarketDat = getDeMarket()


# Smooth data
expSmooth = function(dat) {
  #convenience function for polynomially weighted smoothing, half-life is 4
  hl = 2
  delta = 0.5^(1/hl)
  l = length(dat)
  weight = rep(NA,l)
  for (i in 1:l) {
    weight[i] = delta^(l-i)
  }
  weight = weight/sum(weight)
  smoothed = sum(weight * dat)
  smoothed
}
smoothDat = function(df) {
  # apply bi-montly smoothing to weekly data;
  # the differenced items are now summed because we want cumulative return
  # the undifferenced items are now averaged as they are indicators of regime
  # the resulting dataframe is scaled
  # diffItems = c(
  #   "PX_LAST",
  #   "Quality",
  #   "Value",
  #   "Momentum",
  #   "Growth",
  #   "Size",
  #   "Vol",
  #   "Yield",
  #   # "EEM",
  #   # "Housing",
  #   # "VIX",
  #   # "BCom",
  #   # "DXY",
  #   "spx"
  # )
  #nonDiff = setdiff(colnames(df),diffItems)
  # for (item in diffItems) {
  #   exp = parse(text=paste("df$",item,sep=""))
  #   df[,item] = c(rep(NA,times=7),rollapply(eval(exp),FUN=sum,width = 8))
  # }
  # for (item in nonDiff) {
  #   exp = parse(text=paste("df$",item,sep=""))
  #   df[,item] = c(rep(NA,times=7),rollapply(eval(exp),FUN=expSmooth,width = 8))
  # }
  df = as.data.frame(scale(na.omit(df)))
  
  #df = as.data.frame(na.omit(df))
  
  df
}
getSmooth = function() {
  res = list(
    IT = smoothDat(deMarketDat$IT),
    FIN = smoothDat(deMarketDat$FIN),
    ENG = smoothDat(deMarketDat$ENG),
    HLTH = smoothDat(deMarketDat$HLTH),
    CONS = smoothDat(deMarketDat$CONS),
    COND = smoothDat(deMarketDat$COND),
    UTIL = smoothDat(deMarketDat$UTIL),
    MATR = smoothDat(deMarketDat$MATR),
    INDU = smoothDat(deMarketDat$INDU),
    TELS = smoothDat(deMarketDat$TELS)
  )
}
smoothDat = getSmooth()


# regression ready

form1 = as.formula("PX_LAST~Value+Size+Growth+Yield+Quality+Vol+Momentum+spx")
lm1 = lm(form1,data=smoothDat$FIN[last.3y,])
summary(lm1)





