#.libPaths("/mnt/X500/farmers/suyao/NL200/software/R-3.4.0/lib64/R/library")
library(GetoptLong)
mutfile<-'NA'
cut<-0
phenofile<-'NA'
groupfile<-'NA'
samplefile<-'NA'
outputfile<-'NA'
width<-10
height<-0
atype<-'NA'
btype<-'NA'
colname<-'F'

GetoptLong(
	      "mutfile=s", "Mutation data file",
	      "cut=i", "Cutoff of the number of samples per mutated gene, default (0)",
          "phenofile=s", "Phenotype data file",
          "groupfile=s", "Group data file",
          "samplefile=s", "Sample list file for column order",
          "outputfile=s", "Output file, default (mutfile.pdf)",
          "width=f", "Width of figure, default (10)",
          "height=f", "Height of figure",
          "atype=s", "Cosmic tumor type", 
          "btype=s", "IntOGene tumor type",
          "colname=s", "Show column names (T or F), default (F)",
	      "verbose", "Print messages"
)

gcut<-cut

if(outputfile == 'NA'){
  outputfile = paste(mutfile,'pdf',sep=".")
}
if(length(grep("\\.pdf$",outputfile))==0){
  outputfile = paste(outputfile,'pdf',sep=".")
}
#print(outputfile)

ShowColName = FALSE
if(colname == 'T'){
  ShowColName = TRUE
}

#mywd=getwd()
cmd.args <- commandArgs()
m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
this.dir <- dirname(regmatches(cmd.args, m))
###########
cosmicfile<-paste(this.dir,'cosmic_tumor_type',sep="/")
igfile<-paste(this.dir,"intogen_tumor_type",sep="/")
outputpdf<-outputfile

##########
cat(paste("mut_file:",mutfile,"\n"))
cat(paste("pheno_file:",phenofile,"\n"))
cat(paste("group_file:",groupfile,"\n"))
cat(paste("output_file:",outputpdf,"\n"))
cat(paste("show column name:",ShowColName,"\n"))

#### prepare input data ####
mutinput<-read.table(file=mutfile,as.is=TRUE,sep="\t")

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
mutinput$type[mutinput$V4=='SV']<-'SV'

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
if(height == 0){
  height = width*length(gene2)/length(samples)
}

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
write.table(mmat,file ="mmat.txt",sep = "\t")
#######################################

library(ComplexHeatmap)
library(markdown)
options(markdown.HTML.options = c(options('markdown.HTML.options')[[1]], "toc"))

library(knitr)
knitr::opts_chunk$set(
    error = FALSE,
    tidy  = FALSE,
    message = FALSE,
    fig.align = "center",
    fig.width = 5,
    fig.height = 5)
options(markdown.HTML.stylesheet = "custom.css")

options(width = 100)

#### source oncoPrint.R ####
myoncoPrint=paste(this.dir,'oncoPrint.R',sep="/")
source(myoncoPrint)

mutcol = c("Missense" = "#D68A22", "Nonsense" = "#AF0D27", "Frame Shift" = "#349AD8", "Splicing" = "#0E381B", "In Frame Indel" = "#C3E1BB", "CNV" = "#80578E", "Amplification" = "forestgreen", "SV" = "black","Others" = "#005698")

mut_alter_fun = list(
    background = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#E6E6E6FF", col = NA))
	},
	Missense = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#D68A22", col = NA))
	},
	Nonsense = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#AF0D27", col = NA))
	},
    "Frame Shift" = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#349AD8", col = NA))
	},
    Splicing = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#0E381B", col = NA))
	},
    "In Frame Indel" = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#C3E1BB", col = NA))
	},
    CNV = function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#80578E", col = NA))
    },
    Amplification = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "forestgreen", col = NA))
	},
    SV = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "black", col = NA))
	},
    Others = function(x, y, w, h) {
		grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#005698", col = NA))
	}
)

ht<-oncoPrint(mmat, get_type = function(x) strsplit(x, ";")[[1]],
              alter_fun = mut_alter_fun, col = mutcol, 
              heatmap_legend_param = list(title = "Mutation"),
              show_row_barplot = TRUE, show_column_names = FALSE)
htlist<-ht@ht_list
globalcorder<-htlist[[names(htlist[2])]]@column_order
if(samplefile != 'NA'){
  samplelist<-read.table(file=samplefile,colClasses="character")
  globalcorder<-samplelist$V1
}

SampleMutGeneCount<-SampleGeneCount
SampleMutGeneCountAnnoBar = HeatmapAnnotation('Mutation Burden' = anno_barplot(SampleMutGeneCount,border=FALSE,axis=TRUE,gp = gpar(fill = "royalblue4")),
                                              show_annotation_name = TRUE,annotation_name_rot = 90,annotation_name_side = 'left',
                                              annotation_height = unit(3, "cm"),annotation_name_offset = unit(10, "mm"))
SortSmutCountPer<-names(sort(smutcountper,decreasing=TRUE))
MutGenePer = rowAnnotation("Percentage (%)" = row_anno_barplot(smutcountper,border=FALSE,gp = gpar(fill = "royalblue4"),axis=TRUE),
                           annotation_width = unit(3, "cm"),show_annotation_name = TRUE,annotation_name_rot = 0,annotation_name_offset = unit(10, "mm"))


if(phenofile != 'NA'){
    pheno<-read.table(file=phenofile,head=TRUE,sep="\t",as.is=T)
    phenoSample<-length(unique(pheno$ID))
    cat(paste("Pheno Sample Number:", phenoSample,"\n"))
    setdiffPheno<-length(setdiff(unique(pheno$ID),unique(mutinput$V1)))
	cat(paste("Setdiff Pheno:", setdiffPheno,"\n"))
    pheno<-pheno[with(pheno,order(ID)),]
	colname<-colnames(pheno)
    pheno<-as.data.frame(pheno[,-1])
    colnames(pheno)<-colname[-1]
    rbcol<-rainbow(7)
    plist<-list()
	pheno_column = HeatmapAnnotation(df = pheno, show_annotation_name = TRUE)
}

if(groupfile != 'NA'){
    group<-read.table(file=groupfile,sep="\t",colClasses="character",head=TRUE)
    #group<-group[with(group,order(ID)),]
    groupSample<-length(unique(group$ID))
    cat(paste("Group Sample Number:",groupSample,"\n"))
    setdiffGroup<-length(setdiff(unique(group$ID),unique(mutinput$V1)))
    cat(paste("Setdiff Group:", setdiffGroup,"\n"))
    groupname<-colnames(group)[2] 
    gclass<-sort(unique(group[,2]))
    tabgroup<-table(group[,2])
    gcol<-list()
    groupdf<-data.frame(group[,2][match(samples,group[,1])])
    names(groupdf)<-groupname
    selcol<-rainbow(length(gclass))
    names(selcol)<-gclass
    gcol[[groupname]]=selcol
    corder<-c()
    for (gc in gclass){
      gcid<-group$ID[group[,2]==gc]
      gcid<-sort(gcid)
      ginputgsc<-subset(inputgsc, is.element(V1, gcid))
      gsamples<-sort(gcid)
      ggene2<-sort(unique(ginputgsc$V2))
      gmmat<-matrix(" ",length(ggene2),length(gsamples))
      for(i in 1:length(ggene2)){
        for(j in 1:length(gsamples)){
          if(dim(subset(ginputgsc,V1==gsamples[j] & V2==ggene2[i]))[1]>0){
          			jmut<-paste(unique(subset(ginputgsc,V1==gsamples[j] & V2==ggene2[i])$type),collapse = ';')
          			jmut<-paste(jmut,';',sep="")
          			gmmat[i,j]<-jmut
     	  }
  		}
	  }
      rownames(gmmat)<-ggene2
      colnames(gmmat)<-gsamples
      ght<-oncoPrint(gmmat, get_type = function(x) strsplit(x, ";")[[1]], alter_fun = mut_alter_fun, col = mutcol, 
	                heatmap_legend_param = list(title = "Mutation", at = c("Missense", "Nonsense", "Splicing","Frame shift","In Frame Indel","SV","CNV","Others"), 
		            labels = c("Missense", "Nonsense", "Splicing","Frame shift","In frame indel","SV","CNV","Others")),
                    show_row_barplot = FALSE, show_column_names = TRUE,top_annotation = NULL)
      ghtlist<-ght@ht_list
      gcorder<-ghtlist[[names(ghtlist[2])]]@column_order
      #gcorder <- sort(gcorder)
      corder<-c(corder,gsamples[gcorder])
    }
    if(samplefile == 'NA'){
      globalcorder<-corder
    }
    group_column = HeatmapAnnotation('Mutation Burden' = anno_barplot(SampleMutGeneCount,border=FALSE,axis=TRUE,gp = gpar(fill = "#1F267E")),
                                   Group = groupdf$Group,
                                   col = gcol,show_annotation_name = TRUE,annotation_name_rot = c(90,0),annotation_name_side = c('left','right'),
                                   annotation_height = unit(c(3,0.5), "cm"),annotation_name_offset = unit(c(10,2), "mm")) 
}

######################################################

CosmicGene<-read.table(file=cosmicfile,as.is=TRUE,sep="\t",quote="")
if(atype != 'all'){
    CosmicGene = subset(CosmicGene, V2 == atype)
}
cbar<-rep('N',length(gene2))
cbar[match(CosmicGene$V1,gene2)]<-'Y'
####
intogene<-read.table(file=igfile,head=TRUE,as.is=TRUE,sep="\t")
ibar<-rep('N',length(gene2))
if(btype != 'all'){
    intogene<-subset(intogene, Tumor_type == btype)
}
ibar[match(intogene$geneHGNCsymbol,gene2)]<-'Y'
####
if(atype != 'NA'){
	cdf = data.frame(COSMIC = cbar)
	cha = HeatmapAnnotation(df = cdf, col = list(COSMIC = c("Y" = "red", "N" = "blue")),which = "row",show_annotation_name = TRUE)
}
if(btype != 'NA'){
	idf = data.frame(IntOGen = ibar)
	iha = HeatmapAnnotation(df = idf, col = list(IntOGen = c("Y"="cyan","N"="green")),which = "row",show_annotation_name = TRUE)
}
if(atype != 'NA' & btype != 'NA'){
	cidf = data.frame(COSMIC = cbar,IntOGen = ibar)
	ciha = HeatmapAnnotation(df = cidf, col = list(COSMIC = c("Y" = "red", "N" = "blue"),IntOGen = c("Y"="cyan","N"="green")),which = "row",show_annotation_name = TRUE)
}

###################################### darw ############################################

if(phenofile == 'NA' & groupfile == 'NA'){
	pdf(file=outputpdf,width=width,height=height)
	ht<-oncoPrint(mmat, get_type = function(x) strsplit(x, ";")[[1]],
			      alter_fun = mut_alter_fun, col = mutcol,show_pct = FALSE,
                  heatmap_legend_param = list(title = "Mutation"),
                  show_row_barplot = FALSE, show_column_names = ShowColName,
                 # top_annotation = SampleMutGeneCountAnnoBar,
                  row_order = SortSmutCountPer, column_order = globalcorder)
    if(atype != 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+ciha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype != 'NA' & btype == 'NA'){
		draw(MutGenePer+ht+cha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+iha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype == 'NA'){
		draw(MutGenePer+ht,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	dev.off()
}

##################################may not need############################################
if(phenofile != 'NA' & groupfile != 'NA'){
    pdf(file=outputpdf,width=width,height=height)
    ht<-oncoPrint(mmat, get_type = function(x) strsplit(x, ";")[[1]],
                  alter_fun = mut_alter_fun, col = mutcol, show_pct = FALSE,
                  heatmap_legend_param = list(title = "Mutation"),
              	  show_row_barplot = FALSE, show_column_names = ShowColName,
			      bottom_annotation = pheno_column,
			      top_annotation = group_column,
              	  row_order = SortSmutCountPer, column_order = globalcorder)
    if(atype != 'NA' & btype != 'NA'){
        draw(MutGenePer+ht+ciha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype != 'NA' & btype == 'NA'){
		draw(MutGenePer+ht+cha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+iha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype == 'NA'){
		draw(MutGenePer+ht,padding = unit(c(20, 2, 2, 2), "mm"))
	}
    dev.off()
}

if(phenofile == 'NA' & groupfile != 'NA'){
    pdf(file=outputpdf,width=width,height=height)
    ht<-oncoPrint(mmat, get_type = function(x) strsplit(x, ";")[[1]],
			      alter_fun = mut_alter_fun, col = mutcol,show_pct = FALSE,
			      heatmap_legend_param = list(title = "Mutation"),
              	  show_row_barplot = FALSE, show_column_names = ShowColName,
			      top_annotation = group_column,
              	  row_order = SortSmutCountPer, column_order = globalcorder)
    if(atype != 'NA' & btype != 'NA'){
        draw(MutGenePer+ht+ciha,padding = unit(c(20, 2, 2, 2), "mm"))
    }
	if(atype != 'NA' & btype == 'NA'){
		draw(MutGenePer+ht+cha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+iha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype == 'NA'){
		draw(MutGenePer+ht,padding = unit(c(20, 2, 2, 2), "mm"))
	}
    dev.off()
}
###################################may not need##########################################

if(phenofile != 'NA' & groupfile == 'NA'){
    pdf(file=outputpdf,width=width,height=height)
    ht<-oncoPrint(mmat, get_type = function(x) strsplit(x, ";")[[1]],
			      alter_fun = mut_alter_fun, col = mutcol,show_pct = FALSE,
			      heatmap_legend_param = list(title = "Mutation"),
              	  show_row_barplot = FALSE, show_column_names = ShowColName,
			      bottom_annotation = pheno_column,
			     # top_annotation = SampleMutGeneCountAnnoBar,## top annotation bar without mutation type
              	  row_order = SortSmutCountPer, column_order = globalcorder)
    if(atype != 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+ciha,padding = unit(c(20, 2, 2, 2), "mm"))
    }
	if(atype != 'NA' & btype == 'NA'){
		draw(MutGenePer+ht+cha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype != 'NA'){
		draw(MutGenePer+ht+iha,padding = unit(c(20, 2, 2, 2), "mm"))
	}
	if(atype == 'NA' & btype == 'NA'){
		draw(MutGenePer+ht,padding = unit(c(20, 2, 2, 2), "mm"))
	}
    dev.off()
}
######################## END #############################################
