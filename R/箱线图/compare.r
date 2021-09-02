data<-read.table("clipboard",header=T,sep="\t")

library(ggplot2)
library(magrittr)
library(ggpubr)
library(survminer) 

compare_means(HED_mean~Race, data=data)
my_comparisons <- list(c("Asian","European"))
ggboxplot(data, x="Race", y="HED_mean",bxp.errorbar=TRUE,
color = "Race", add = "jitter", 
order=c("Asian","European"),
title="comparation of HED_score in different races")+
rotate_x_text(angle = 45)+
geom_hline(yintercept = mean(data1$value), linetype=2)+
stat_compare_means(comparisons=my_comparisons, label.y = 15)+ 
stat_compare_means(label = "p.signif", method = "kruskal.test", ref.group = ".all.")+
stat_compare_means(label.y = 20)
