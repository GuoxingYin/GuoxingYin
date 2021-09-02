tmp = commandArgs(T)
data <- read.table(tmp[1], head=T)
pdf(tmp[2])
boxplot(shannon~class,data=data,boxwex=0.5,col=rainbow(length(unique(data$class))),cex.lab=1.3,cex.main=1.5,cex.axis=1.2)
p1 <- wilcox.test(shannon~class,data)$p.value
p = signif(p1,3)
mtext(paste("p=",p),col=1, font=2, cex= 1.5)
dev.off()
