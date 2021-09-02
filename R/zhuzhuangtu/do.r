library(ggplot2)
data_plot=read.table("/Users/ygx/Desktop/genome/mm10/gtf_gene_element/shell/snv_es.txt",header=T)
data_plot$pos<-factor(data_plot$pos,levels = c('promoter', 'exon', 'intron', "five_proximal", "three_proximal", "five_distal", "three_distal", 'gene_desert'))
data_plot$Condition<-factor(data_plot$Condition,levels = c('Terc_WT','Terc_OK'))

g = ggplot(data_plot, aes(x = pos, y = count, fill = Condition)) +scale_fill_manual(values=c("#E64B35","#00A087"))+
	geom_bar(stat = 'identity', position = 'dodge', width = 0.6) +
	theme_classic() +
	theme(axis.ticks.length=unit(0.2, "cm")) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	scale_x_discrete(labels= c('promoter', 'exon', 'intron', "five_proximal", "three_proximal", "five_distal", "three_distal", 'gene_desert')) +
	scale_y_continuous(limits = c(0, 45000))+
	ylab('number') +
	xlab('genome position')
g


