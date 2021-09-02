library(dplyr)
data= read.table(".//2overlap//te_type.txt",header = T)
data$sample = data[,1]
data$group = data[,2]
data$num = data[,3]
data$group2 = data[,4]
df1 = data %>% dplyr::group_by(group,group2) %>% dplyr::summarise(avg=mean(num)) 
df2 = data %>% dplyr::group_by(group,group2) %>% dplyr::summarise(sd =sd(num))
df3 = dplyr::full_join(x=df1,y=df2,by=c("group2","group"))

p = ggplot(df3, aes(x = group, y = avg,fill = group2))+ 
  geom_bar(stat = "identity", color="black", width = 0.55, position = 'dodge') +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg + sd), width = 0.2, position = position_dodge(0.6))+
  theme_classic()
pdf('.//2overlap//te_type.pdf')
p
dev.off()