tmp=commandArgs(T)
data = read.table(tmp[1],header = T)
info = data$num
names = data$kind


# 涂色（可选）
cols = c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F")
# 计算百分比
piepercent = paste(round(100*info/sum(info)), "%")
# 绘图
pdf(tmp[2])
pie(info, labels=info, main = tmp[1], col=cols, family='GB1')
# 添加颜色样本标注
legend("topright", names, cex=0.8, fill=cols)
dev.off()