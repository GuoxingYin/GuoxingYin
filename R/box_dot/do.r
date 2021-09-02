tmp = commandArgs(T)
library(ggplot2)
data <- read.table(tmp[1], head=T)
pdf(tmp[2])
p<-ggplot(data,aes(x=factor(class),y=time))
p+geom_boxplot(col="black",pch=16,cex=1)+geom_point(position="jitter",col=2,pch=16,cex=1)+ theme_bw() + theme(panel.grid=element_blank())
dev.off()

