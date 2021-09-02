library(ggpubr)
library(ggplot2)
#args=commandArgs(T)
L1="#4135FE"
SINE="#38BD98"
LTR="#F26920"
my.args = function(a,b,c){
  args=c(a,b,c)
  data=read.table(args[1])
  length=data[,2]
  count=data[,3]
  df=data.frame(x=length,y=count)
  ggplot(data=df,mapping = aes(x = factor(length), y = count)) + geom_bar(stat="identity",width=1.0,fill=get(args[2])) +scale_y_continuous(limits =c(0, 25)) +
    labs(title = args[3],x='length',y='count')+theme_classic()+
    theme(axis.title.x =element_text(size=24,family = 'serif', color="black"),axis.title.y=element_text(size=24,family = 'serif',color="black"))+
    theme(axis.text.x = element_text(size = 15,family = 'serif', color = "black"),axis.text.y= element_text(size = 15,family = 'serif', color = "black"))+
    theme(title = element_text(size=20,family = 'serif',color="black"))
  
}
p1=my.args('result_L1_a49_1.txt','L1','L1_late passage terc -/-')
p2=my.args('result_L1_a49_2.txt','L1','L1_late passage terc -/-')
p3=my.args('result_L1_n33_1.txt','L1','L1_late passage WT')
p4=my.args('result_L1_n33_2.txt','L1','L1_late passage WT')
p5=my.args('result_L1_terc_KO.txt','L1','L1_early passage terc -/-')
p6=my.args('result_L1_terc_WT.txt','L1','L1_early passage WT')
p7=my.args('result_SINE_a49_1.txt','SINE','SINE_late passage terc -/-')
p8=my.args('result_SINE_a49_2.txt','SINE','SINE_late passage terc -/-')
p9=my.args('result_SINE_n33_1.txt','SINE','SINE_late passage WT')
p10=my.args('result_SINE_n33_2.txt','SINE','SINE_late passage WT')
p11=my.args('result_SINE_terc_KO.txt','SINE','SINE_early passage terc -/-')
p12=my.args('result_SINE_terc_WT.txt','SINE','SINE_early passage WT')
p13=my.args('result_LTR_a49_1.txt','LTR','LTR_late passage terc -/-')
p14=my.args('result_LTR_a49_2.txt','LTR','LTR_late passage terc -/-')
p15=my.args('result_LTR_n33_1.txt','LTR','LTR_late passage WT')
p16=my.args('result_LTR_n33_2.txt','LTR','LTR_late passage WT')
p17=my.args('result_LTR_terc_KO.txt','LTR','LTR_early passage terc -/-')
p18=my.args('result_LTR_terc_WT.txt','LTR','LTR_early passage WT')
pdf("result.pdf",width = 18,height = 24)
ggarrange(p6,p12,p18,p5,p11,p17,p1,p7,p13,p2,p8,p14,p3,p9,p15,p4,p10,p17,ncol=3,nrow=6,labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R"))
dev.off()


