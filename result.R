require(ggplot2)
require(gplots)
################################### The results
res.wl = rep(0,times=9)
for (lst in fm1) {
  res.wl = rbind(res.wl,lst$coef)
}
rownames(res.wl) = c("0",names(fm1))
res.wl = res.wl[-1,]
tmp1 = round(res.wl,2)
tmp1[tmp1==0] = " "
tmp1 = as.data.frame(tmp1)
tmp1

res.pr = rep(0,times=8)
for (lst in fm2) {
  nm = rownames(lst$coef)
  vec = as.vector(lst$coef)
  names(vec) = nm
  res.pr = rbind(res.pr,vec)
}
rownames(res.pr) = c("0",names(fm2))
res.pr = res.pr[-1,]
tmp2 = round(res.pr,2)

tmp2 = as.data.frame(cbind(rep(0,times=10),tmp2))
tmp2


############ The Result
res.las = rep(0,times=8)
for (lst in fm3) {
  tmp = as.vector(lst$coef)
  names(tmp) = rownames(lst$coef)
  res.las = rbind(res.las,tmp)
}
rownames(res.las) = c("0",names(fm3))
res.las = res.las[-1,]
tmp3 = round(res.las,2)
tmp3[tmp3==0] = " "
tmp3 = as.data.frame(tmp3)
tmp3
tmp1[,-1]
tmp2[,-1]


############### Heat Map Visuals For Coefficients
save.image("G:/WORK/SMIF/factorModelV2.1/fm1.RData")

heatmap(res.las,col=cm.colors(100))
heatmap(res.wl[,-1],col=cm.colors(100))
heatmap(res.pr[,-1],col=cm.colors(100))



############### ICR Results
res.icr = rep(0,times=8)
for (lst in fm.ICR) {
  tmp = as.vector(lst$coef)
  names(tmp) = rownames(lst$coef)
  res.icr = rbind(res.icr,tmp)
}
rownames(res.icr) = c("0",names(fm.ICR))
res.icr = res.icr[-1,]
round(res.icr,3)

## take out insignificant loadings
cutoff = 0.05
loading.icr = res.icr
loading.icr[which(abs(loading.icr)<cutoff)] = 0
round(loading.icr,1)
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
heatmap.2(loading.icr,dendrogram="none",Rowv = NA,Colv=NA,col=cm.colors(100),density.info="none",main="Factor Loading Heatmap")
