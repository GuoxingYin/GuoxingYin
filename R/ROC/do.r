library(pROC)
library(ggplot2)
data = read.table("data.txt",header=T)
roca<-roc(data$DCR,data$msaf)
auc(roca)
plot(roca,print.auc=TRUE,plot=TRUE,
     print.thres=TRUE)
