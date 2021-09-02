###usage:bar_input:input_ref;gene_input:input_ref
library(ggplot2)
library(ggsci)
#library(reshape2)
#library(gridExtra)

df = read.table('bar_input',header = T,sep ='\t')
df$frequency2 <- ifelse(df$group == "DC", 
                    df$freq* 1,
                    df$freq * -1)

gene_df = read.table('gene_input',header = T)
df$gene <- factor(df$gene,levels = rev(gene_df$gene))


pdf('PD_VS_DC.pdf',height = 30,width = 10)
#as.integer(rownames(df))
ggplot()+
  geom_bar(data = df,aes(x = factor(gene),y = frequency2,fill = kind),
           color = 'black',stat = 'identity',position = 'stack',size = 0.01)+coord_flip()+
  theme_bw()+scale_fill_d3('category20c')+
  scale_y_continuous(limits = c(-100,100),breaks = c(-100,-50,0,50,100),labels = c(100,50,0,50,100)) +
  ylab('PD_VS_DC Frequency (%)')+xlab('Gene')

dev.off()
