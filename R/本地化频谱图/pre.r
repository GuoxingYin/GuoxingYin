###输入文件和集群一样,注意保留对应突变类型，没有分组要注释掉分组相关信息
tmp = commandArgs(T)
gcut=0
#height = 0
#### prepare input data ####
mutinput<-read.table(file=tmp[1],as.is=TRUE,sep="\t")

sampleNum<-length(unique(mutinput$V1))
cat(paste("Sample Number:",sampleNum,"\n"))

mutinput$type<-'Others'
mutinput$type[mutinput$V4=='missense']<-'Missense'
mutinput$type[mutinput$V4=='nonsense']<-'Nonsense'
mutinput$type[mutinput$V4=='splice-5']<-'Splicing'
mutinput$type[mutinput$V4=='splice-3']<-'Splicing'
mutinput$type[mutinput$V4=='splice']<-'Splicing'
mutinput$type[mutinput$V4=='frameshift']<-'Frame Shift'
mutinput$type[mutinput$V4=='cds-ins']<-'In Frame Indel'
mutinput$type[mutinput$V4=='cds-del']<-'In Frame Indel'
mutinput$type[mutinput$V4=='cds-indel']<-'In Frame Indel'
mutinput$type[mutinput$V4=='CNV']<-'CNV'
mutinput$type[mutinput$V4=='amplification']<-'Amplification'
#mutinput$type[mutinput$V4=='SV']<-'SV'

genepersample<-table(mutinput$V1,mutinput$V2)
SampleGeneCount<-rowSums(genepersample)
samples<-sort(unique(mutinput$V1))
genes<-sort(unique(mutinput$V2))
tsg<-table(mutinput$V1,mutinput$V2)
smutcount<-colSums(tsg>0)
smutcountper<-smutcount*100/length(samples)
smutcountper<-smutcountper[smutcount>=gcut]
gsc<-names(smutcount[smutcount>=gcut])
inputgsc<-subset(mutinput, is.element(V2, gsc))
gene2<-sort(unique(inputgsc$V2))
#if(height == 0){
#  height = width*length(gene2)/length(samples)
#}

mmat<-matrix(" ",length(gene2),length(samples))
for(i in 1:length(gene2)){
   for(j in 1:length(samples)){
     if(dim(subset(inputgsc,V1==samples[j] & V2==gene2[i]))[1]>0){
          jmut<-paste(unique(subset(inputgsc,V1==samples[j] & V2==gene2[i])$type),collapse = ';')
          jmut<-paste(jmut,';',sep="")
          mmat[i,j]<-jmut
     }
  }
}
rownames(mmat)<-gene2
colnames(mmat)<-samples
pdf(tmp[3],height = 50,onefile = FALSE)
#################################################################################
library(ComplexHeatmap)
library(circlize)

mat = t(mmat)
mat[is.na(mat)] = ""
rownames(mat) = mat[, 1]
mat = mat[, -1]
mat=  mat[, -ncol(mat)]
mat = t(as.matrix(mat)) 
alter_fun = function(x, y, w, h, v) {
  n=sum(v)
  h=h*0.9
  grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#CCCCCC", col = NA))
  if(v["Missense"])  grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["In Frame Indel"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["Splicing"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["Frame Shift"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["Nonsense"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
 # if(v["SV"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["CNV"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["Others"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
}
col = c("Missense" = "#1F78B4", "In Frame Indel" = "#33A02C", "Splicing" = "#E31A1C","Frame Shift"="#8B658B","Nonsense"="#CD6839","SV"="#A72838","CNV"="#2D6296","Others"="#090909")
GT<-read.table(file=tmp[2],header=T)
order<-as.character(GT$ID)
group<-as.character(GT$Group)
#TNM<-as.character(GT$TNM)
oncoPrint(mat, top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot(),Group=group),get_type = function(x) strsplit(x, ";")[[1]],alter_fun = alter_fun,col = col,row_order = NULL, column_order = NULL)
#oncoPrint(mat, top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot()),get_type = function(x) strsplit(x, ";")[[1]],alter_fun = alter_fun,col = col,row_order = NULL, column_order = NULL)

dev.off()
