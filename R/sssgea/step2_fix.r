##加载包
{
library(genefilter)
library(GSVA)
library(Biobase)
library(stringr)
}
##载入数据
#load('uni_matrix.Rdata')
uni_matrix<-read.table("dataset04.txt",header = T,row.names = 1)
pheno<-read.table("pheno.txt",header = T)
gene_set<-read.csv("mmc3.csv")[, 1:2]
head(gene_set)
list<- split(as.matrix(gene_set)[,1], gene_set[,2])
gsva_matrix<- gsva(as.matrix(uni_matrix), list,method='ssgsea',kcdf='Gaussian',abs.ranking=TRUE)
library(pheatmap)
gsva_matrix1<- t(scale(t(gsva_matrix)))
gsva_matrix1[gsva_matrix1< -2] <- -2
gsva_matrix1[gsva_matrix1>2] <- 2
anti_tumor <- c('Activated CD4 T cell', 'Activated CD8 T cell', 'Central memory CD4 T cell', 'Central memory CD8 T cell', 'Effector memeory CD4 T cell', 'Effector memeory CD8 T cell', 'Type 1 T helper cell', 'Type 17 T helper cell', 'Activated dendritic cell', 'CD56bright natural killer cell', 'Natural killer cell', 'Natural killer T cell')
pro_tumor <- c('Regulatory T cell', 'Type 2 T helper cell', 'CD56dim natural killer cell', 'Immature dendritic cell', 'Macrophage', 'MDSC', 'Neutrophil', 'Plasmacytoid dendritic cell')
anti<- gsub('^ ','',rownames(gsva_matrix1))%in%anti_tumor
pro<- gsub('^ ','',rownames(gsva_matrix1))%in%pro_tumor
non <- !(anti|pro)
gsva_matrix1<- rbind(gsva_matrix1[anti,],gsva_matrix1[pro,],gsva_matrix1[non,])
normalization<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
nor_gsva_matrix1 <- normalization(gsva_matrix1)
annotation_col = data.frame(patient=pheno$num2)
rownames(annotation_col)<-colnames(uni_matrix)
bk = unique(c(seq(0,1, length=100)))
pheatmap(nor_gsva_matrix1,
         show_colnames = F,
         cluster_rows = F,cluster_cols = F,
         annotation_col = annotation_col,
         breaks=bk,cellwidth=5,cellheight=5,
         fontsize=5,gaps_row = c(12,20),
         filename = 'ssgsea.pdf',width = 8)
save(gsva_matrix,gsva_matrix1,pheno,file = 'score.Rdata')