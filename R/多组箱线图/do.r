args=commandArgs(T)
data = read.table(args[1],header = T)
require(cowplot)
require(tidyverse)
require(ggsci)
pdf(args[2])
## Loading required package: ggsci
mydata<-data %>% 
  ## 基因表达数据gather,gather的范围应调整
  gather(key="gene",value="Expression",args[3]:args[4]) %>% 
  ##
  dplyr::select(ID,gene,Expression,everything()) 
head(mydata)  
P<-mydata %>% 
   ## 确定x,y
   ggplot(aes(x = gene, y = Expression, fill = group)) +
        geom_boxplot(alpha=0.7) +
        scale_y_continuous(name = "Expression")+
        scale_x_discrete(name = "Gene") +
        #ggtitle("Boxplot of hub gene") +
        theme_bw() +
        theme(plot.title = element_text(size = 14, face =  "bold"),
              text = element_text(size = 12),
              axis.title = element_text(face="bold"),
              axis.text.x=element_text(size = 11)) +
theme(text = element_text(size = 20),
axis.text.x = element_text(angle = 90,hjust = 1))
p2<-P+scale_fill_lancet()
p2
dev.off()
