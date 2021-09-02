###输入文件格式
### 样本1 样本2 样本3
###gene1  ### ### ###
###gene2  ### ### ###
###gene3  ### ### ###
library("ggplot2")
data.matrix=read.table("data2.txt",header = T)

head(data.matrix)


pca <- prcomp(t(data.matrix), scale=TRUE)  ###prcomp函数的横行必须是样本，所以倒置一下
pca.var <- pca$sdev^2  ## sdev是标准偏差，十个样本，就有十个标准偏差，平方是避免负数的干扰
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)  ##求每个样本的variation
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")  ##用柱状图可视
pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data
pca_data= read.table("data.txt",header = T)
ggplot(data=pca_data,aes(x=X,y=Y,color=Sample,shape=Sample))+
  geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  xlab(paste("PC1(",pca.var.per[1],"%","variance)",sep=""))+
  ylab(paste("PC2(",pca.var.per[2],"%","variance)",sep=""))+
  theme(legend.position = c(0.80,0.9))

