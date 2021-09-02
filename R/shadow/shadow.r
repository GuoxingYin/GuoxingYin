library(ggplot2)
data = read.table("data2.txt",header = T, sep = "\t")
ggplot(data, aes(x = msaf, y =btmb )) +
geom_point() +
scale_colour_brewer(palette = "Set1") +
geom_smooth(method = lm)+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"))
