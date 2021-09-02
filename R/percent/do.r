tmp = commandArgs(T)
library(ggplot2)
library(ggsci)
pdf(tmp[2])
data1 = read.table(tmp[1],header = T)
p= ggplot(data = data1,aes(x=gene,y=freq,fill=kind),axis.text.x = element_text(angle = 90,hjust = 1))+
geom_bar(stat = "identity", position = "stack")+scale_color_npg()+ scale_fill_npg()
p=p+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))
p
dev.off()
