library (readxl)
library(forestplot)
ForestPlot <- read_excel("data.xlsx")
attach(ForestPlot)
pdf("forest.pdf",width=15,height=4)
forestplot(as.matrix(ForestPlot[,1:4]), HR, LowerCI, UpperCI, graph.pos=4, zero=1, 
           graphwidth=unit(50,"mm"), lineheight="auto", boxsize=0.3, 
           xlab="HR(95% CI)",
           xlog = TRUE,
           xticks = c(0.5,1,2,5),
           is.summary = c(FALSE,FALSE,FALSE,FALSE,TRUE),
           col= fpColors(box="#749CAA",summary="#749CAA",lines='#749CAA'))

dev.off()

