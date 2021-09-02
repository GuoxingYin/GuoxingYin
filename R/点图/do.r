#data = read.table("fig1.txt",header = T,sep = "\t")
library(ggplot2)
library(reshape2)
tmp <- commandArgs()
data<-read.table(tmp[6],header=T,sep="\t")
pdf(tmp[7],width=10,height=10,onefile = FALSE)  
data2 <- melt(data)
p = ggplot(data2, aes(x=variable, y=value, color = variable,fill = variable)) +  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=0.2,)
p
#+scale_fill_manual(values=c("#235AB3","#EAB509","#737373","#C03D3B","#6894D3","#102B53","#7B6504"))
# dot plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"))
dev.off()

