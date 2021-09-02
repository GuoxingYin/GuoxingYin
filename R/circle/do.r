snv_vcf=read.table('./G3_2_somatic.snvs.ann.sort.vcf1.vcf',header = T,sep='\t')  
indel_vcf = read.table('./G3_2_somatic.indels.ann.sort.vcf1.vcf',header = T,sep='\t')
pdf('./circos.pdf')
  colors = rainbow(10, alpha=0.8)
  library(OmicCircos)
  par(mar=c(2, 2, 2, 2));
  plot(c(1,800) , c(1,800) , type="n", axes=FALSE, xlab="", ylab="", main="");
  circos(R=400, cir='mm10', type="chr", col='blank', print.chr.lab=TRUE, W=4, scale=TRUE);
  circos(R=300, cir='mm10', W=60, mapping = snv_vcf, type="b3", B=F, col = 'red', lwd = 0.5);
  circos(R=200, cir='mm10', W=60, mapping = indel_vcf, type="s2", B=F, col = 'blue', cex=0.5);
  dev.off()
  
  
