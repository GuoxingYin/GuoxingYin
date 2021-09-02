rm(list=ls())
anti_tumor <- c('Activated CD4 T cell', 'Activated CD8 T cell', 'Central memory CD4 T cell', 'Central memory CD8 T cell', 'Effector memeory CD4 T cell', 'Effector memeory CD8 T cell', 'Type 1 T helper cell', 'Type 17 T helper cell', 'Activated dendritic cell', 'CD56bright natural killer cell', 'Natural killer cell', 'Natural killer T cell')
pro_tumor <- c('Regulatory T cell', 'Type 2 T helper cell', 'CD56dim natural killer cell', 'Immature dendritic cell', 'Macrophage', 'MDSC', 'Neutrophil', 'Plasmacytoid dendritic cell')
load('score.Rdata')
anti<- as.data.frame(gsva_matrix1[gsub('^ ','',rownames(gsva_matrix1))%in%anti_tumor,])
pro<- as.data.frame(gsva_matrix1[gsub('^ ','',rownames(gsva_matrix1))%in%pro_tumor,])
anti_n<- apply(anti,2,sum)
pro_n<- apply(pro,2,sum)
patient <- pheno$num2[match(colnames(gsva_matrix1),pheno$num1)]
library(ggplot2)
data <- data.frame(anti=anti_n,pro=pro_n,patient=patient)
anti_pro<- cor.test(anti_n,pro_n,method='pearson')
gg<- ggplot(data,aes(x = anti, y = pro),color=patient) + 
  xlim(-20,15)+ylim(-15,10)+
  labs(x="Anti-tumor immunity", y="Pro-tumor suppression") +
  geom_point(aes(color=patient),size=3)+geom_smooth(method='lm')+
  annotate("text", x = -5, y =7.5,label=paste0('R=',round(anti_pro$estimate,4),'\n','p<0.001'))
ggsave(gg,filename = 'cor.pdf', height = 6, width = 8)