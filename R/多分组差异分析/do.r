tmp=commandArgs(T)
library(pgirmess)
data4=read.table(tmp[1],header=T)
p=kruskalmc(data4$percent,data4$pos)
p2=kruskal.test(data4$percent,data4$pos)
p2
write.table(p,tmp[2],sep="\t")
