#useage:Rscript **.r **.txt **.pdf 
tmp<-commandArgs(T)
#print(tmp)
data<-read.table(tmp[1],header=T,sep="\t")

library(survival)
library(survminer)
library(ggpubr)
library(magrittr)
library(ggpubr)

my.surv <- Surv(data$PFS, data$PFS_event)
fit <- survfit(my.surv ~ data$group)
data.survdiff <- survdiff(my.surv ~ data$group)
pdf(tmp[2],onefile = FALSE)  
p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
  HR = (data.survdiff$obs[2]/data.survdiff$exp[2])/(data.survdiff$obs[1]/data.survdiff$exp[1])
  up95 = exp(log(HR) + qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
  low95 = exp(log(HR) - qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
  HR <- paste("Hazard Ratio = ", round(HR,2), sep = "")
  CI <- paste("95% CI: ", paste(round(low95,2), round(up95,2), sep = " - "), sep = "")

ggsurvplot(fit, data = data ,
xlim =c(0,30),           
break.time.by=10,
ggtheme = theme_bw(),
           conf.int = F,
           conf.int.style = "step",
           censor = F, 
           #palette = c("black","red","purple","green","yellow","tan4","orange","tan1","lightblue","gold4","lightblue4","green2","gold","green4"), 
           risk.table=TRUE, 
           risk.table.height=0.3,
           font.legend = 11,
           #legend.labs=c("CCND1","CCND2","CCND3","CCNE1","CDK12","CDK4","CDK6","CDK8","CDKN1A","CDKN1B","CDKN2A","CDKN2B","CDKN2C","non"),
           font.title = 12,font.x = 10,font.y = 1,
           pval = paste(pval = ifelse(p.val < 0.001, "p < 0.001", 
                        paste("p = ",round(p.val,3), sep = "")),HR, CI, sep = "\n"),
           pval.coord=c(10, 0.8))
dev.off()
