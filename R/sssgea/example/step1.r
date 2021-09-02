a <- read.table('GSE112996_merged_fpkm_table.txt',
                header = T,
                row.names=1)
raw_data<- a[,-1]
###表型信息提取
pheno <- read.csv(file = 'GSE112996_series_matrix.txt')
pheno <- data.frame(num1 = strsplit(as.character(pheno[42,]),split='\t')[[1]][-1],
                    num2 = gsub('patient: No.','P',strsplit(as.character(pheno[51,]),split='\t')[[1]][-1]))

{
####数据过滤
data<- a[!apply(raw_data,1,sum)==0,]
####去除重复基因名的行，归一化
data$median=apply(data[,-1],1,median)
data=data[order(data$GeneName,data$median,decreasing = T),]
data=data[!duplicated(data$GeneName),]
rownames(data)=data$GeneName
uni_matrix <- data[,grep('\\d+',colnames(data))]
uni_matrix <- log2(uni_matrix+1)
colnames(uni_matrix)<- gsub('X','',gsub('\\.','\\-',colnames(uni_matrix)))
uni_matrix<- uni_matrix[,order(colnames(uni_matrix))]
}
save(uni_matrix,pheno,file = 'uni_matrix.Rdata')