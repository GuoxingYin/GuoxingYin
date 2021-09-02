#########usage:Rscript do.r result_LTR_terc_WT.txt result_LTR_terc_WT.pdf LTR
library(ggplot2)
args=commandArgs(T)
#args=c("result_SINE_a49_1.txt","result_SINE_a49_1.pdf","SINE")
L1="#4135FE"
SINE="#38BD98"
LTR="#F26920"
data=read.table(args[1])
length=data[,2]
count=data[,3]
df=data.frame(x=length,y=count)
pdf(args[2])
ggplot(data=df,mapping = aes(x = factor(length), y = count)) + geom_bar(stat="identity",width=1.0,fill=get(args[3])) +
  labs(title = args[2])+theme_classic()
 dev.off()
