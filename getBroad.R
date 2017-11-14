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


