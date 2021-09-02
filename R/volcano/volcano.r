library(ggplot2)
diff <- read.csv("DF2.txt",header = T,sep="\t")

logFC <-diff$Log2.FC
padj <- diff$FDR

data <- data.frame(logFC=logFC,padj=padj)
data$sig[(data$padj > 0.05|data$padj=="NA")|(data$logFC < 1)& data$logFC > -1] <- "no"
data$sig[data$padj <= 0.05 & data$logFC >= 1] <- "up"
#data$sig[data$padj <= 0.05 & data$logFC >= 0] <- "up"

data$sig[data$padj <= 0.05 & data$logFC <= -1] <- "down"
#data$sig[data$padj <= 0.05 & data$logFC <= -0] <- "down"

# 选最大值作为xlim的上下边界
x_lim <- max(logFC,-logFC)
# 绘制火山图
library(ggplot2)
library(RColorBrewer)
pdf(file = "miRNA_volcano.pdf",width=8,height=8)
theme_set(theme_bw())
p <- ggplot(data,aes(logFC,-1*log10(padj),
                   color = sig))+geom_point()+
   scale_x_continuous(limits=c(-ceiling(x_lim),ceiling(x_lim)),breaks=seq(-ceiling(x_lim),ceiling(x_lim),1)) +  labs(x="log2(FoldChange)",y="-log10(FDR)")
p <- p + scale_color_manual(values =c("#0072B5","grey","#BC3C28"))+
#  p <- p + scale_color_manual(values =c("#0072B5","#BC3C28"))+ 
  geom_hline(yintercept=-log10(0.05),linetype=4)+
  geom_vline(xintercept=c(-1,1),linetype=4)
p <- p +theme(panel.grid =element_blank())+
    theme(axis.line = element_line(size=0))+ylim(0,15)
p <- p  +guides(colour = FALSE)
p <- p +theme(axis.text=element_text(size=20),axis.title=element_text(size=20))
p
dev.off()
print(p)
