require(nloptr)
require(quantmod)
require(lubridate)

last.1y = 762:815
last.3y = 661:815
last.5y = 565:815
all = 1:816
factors = c("Value","Growth","Momentum","Quality","Size","Vol","Yield","spx")
# > rownames(SIGMA)
# [1] "IT.price"   "FIN.price"  "ENG.price"  "HLTH.price" "CONS.price" "COND.price" "INDU.price"
# [8] "UTIL.price" "TELS.price" "MATR.price"

sectors = c("IT","FIN","ENG","HLTH","CONS","COND","INDU","UTIL","TELS","MATR")


loading = read.csv("betasEditted.csv")
rownames(loading) = loading[,1]
loading = loading[,-1]

betas=loading[,"beta",drop=FALSE]

loading = as.data.frame(loading.icr)[sectors,]


S = quantile(loading$Size)["75%"]
M = quantile(loading$Momentum)["75%"]
G = quantile(loading$Growth)["75%"]
Q = quantile(loading$Quality)["75%"]
V = quantile(loading$Value)["75%"]
L = quantile(loading$Vol)["25%"]
Y = quantile(loading$Yield)["25%"]
B = quantile(loading$spx)["25%"]


secP = getSectorPrice() 


secP.mat = as.data.frame(matrix(0,nrow = nrow(secP$IT.price),ncol=length(secP)))
colnames(secP.mat) = names(secP)
for (c in 1:ncol(secP.mat)) {
  exp = parse(text=paste0("secP$",colnames(secP.mat)[c]))
  price = eval(exp)
  secP.mat[,c] = price
}
secRet = diff(as.matrix(secP.mat))


SIGMA = cov(secRet[last.3y,])

SIGMA
fn_ = function(x) {
  return(
    t(x) %*% SIGMA %*% x
    #sum(x*loading$spx)
  )
}


hin_ = function(x) {
  # x is weights
  h=numeric(12)
  # Size exposure
  h[1] = sum(x * loading$Size) - S
  # Momentum exposure
  h[2] = sum(x * loading$Momentum) - M
  # Low Vol exposure
  h[3] = L - sum(x * loading$Vol)
  # High Yield exposure
  h[4] = Y - sum(x * loading$Yield)
  # Full investment
  h[5] = sum(x) - 1
  h[6] = 1 - sum(x)
  # # Market exposure
  h[7] = sum(x*loading$spx) - B
  # Value, Quality, Growth
  h[8] = sum(x * loading$Value) - V
  h[9] = sum(x * loading$Quality) - Q
  h[10] = sum(x * loading$Growth) - G
  return(h)
}

# > dim(SIGMA)
# [1] 10 10
x0_ = rep(0.05,times=10)
Solution = cobyla(x0 = x0_,fn=fn_,hin=hin_,lower=rep(0.03,times=10),upper=rep(0.35,times=10))

AA = round(Solution$par*100,2)
names(AA) = sectors
sort(AA)

write.csv(AA,"AA.csv")
