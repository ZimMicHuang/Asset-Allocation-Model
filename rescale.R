
################### Rescale 
factDatSmooth = smoothDat.scale$IT[,factors] # the same for every dataframe
head(factDatSmooth)

dim(factDatSmooth)

res = matrix(rep(0,times=2*ncol(factDatSmooth)),nrow=2)
for (c in 1:ncol(factDatSmooth)) {
  
  mu = mean(factDatSmooth[,c])
  sig = sd(factDatSmooth[,c])
  res[1,c] = mu
  res[2,c] = sig
  
}
res = as.data.frame(res)
rownames(res) = c("mean","sd")
colnames(res) = colnames(factDatSmooth)


res.las

alpha = rep(0,times=nrow(res.las))
beta = res.las
for (r in 1:nrow(res.las)) {
  print(r)
  beta[r,] = res.las[r,] * res["sd",]
}
round(beta,2)

for (r in 1:nrow(beta)) {
  print(r)
  alpha[r] = sum((-1) *beta[r,] * res["mean",])
}
alpha


round(beta,3)
tmp4 = round(beta,3)
tmp4[tmp4==0] = " "
tmp4 = as.data.frame(tmp4)
tmp4

write.csv(beta,"betas.csv")
