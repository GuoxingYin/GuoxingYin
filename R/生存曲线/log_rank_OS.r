tmp = commandArgs(T)
library(survival)
library(ggplot2)
library(ggpubr)
library(survminer)
lxrsurdata<-read.table(tmp[1], header =TRUE)
pdf(tmp[2],onefile = FALSE)
fit<-survfit(Surv(OS,OS_event)~group,data=lxrsurdata)
res_cox<-coxph(Surv(OS, OS_event) ~group, data= lxrsurdata)
p3= ggsurvplot(fit, data = lxrsurdata,
           conf.int = FALSE,
          # pval = TRUE,
           fun = "pct",
           surv.median.line = "hv",
           risk.table = TRUE,
           size = 1,
           #linetype = "strata",
           #palette = c(rgb(234,123,11,maxColorValue=255),rgb(121,23,234,maxColorValue=255),rgb(1,23,234,maxColorValue=255)),
           legend = "bottom",
           #legend.labs = c("s1","s2","s3"),
           #legend.title = "不同sss",)
)

p3$plot = p3$plot +ggplot2::annotate("text",x = 15, y = 75,
                    label = paste("HR :",round(summary(res_cox)$conf.int[1],2))) + ggplot2::annotate("text",x = 15, y = 80,
label = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = ""))+
  ggplot2::annotate("text",x = 15, y = 70,
                    label = paste("P:",round(summary(res_cox)$coef[5],4)))
p3
dev.off()

