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
    spx = log(numerify(clean(spx)))
  )
}
spx = getSPX()