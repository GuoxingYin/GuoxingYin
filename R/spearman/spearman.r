data=read.table("1.txt",header = T)
x=data$freq_pair
y=data$freq_single
p=cor.test(x,y,method="spearman")$p.value
r=cor.test(x,y,method="spearman")$estimate
pdf("out.pdf")
plot(x,y,pch=16)+abline(x,y)
vp=paste0("spearman\n","rho=",r,"\np=",p)
mtext(paste(vp),col=1, font=2, cex= 1.5)  
dev.off()
