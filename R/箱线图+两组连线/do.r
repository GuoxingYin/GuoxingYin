library(ggplot2)
library(reshape2)
tmp <- commandArgs()
data<-read.table(tmp[6],header=T,sep="\t")
pdf(tmp[7])  
input = melt(data,id=c("d"))
plot1 = ggplot(input, aes(x=variable, y=value, fill=variable)) +
geom_boxplot() +scale_fill_manual(values=c("#2D3580","#E60006"))+
labs( y="mTBI") +
theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'))
plot3 = plot1 + geom_point(color="black") + geom_line(aes(group=pats))
plot3
dev.off()
