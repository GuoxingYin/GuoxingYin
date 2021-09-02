library(tidyverse)
library(clusterProfiler)
library(org.Hs.eg.db)

library(STRINGdb)
library(igraph)
library(ggraph)
string_db <- STRINGdb$new( version="11", species=9606,
                           score_threshold=400, input_directory="")
gene = read.table("test.txt",sep = "\t",header=T)
gene <- gene[,1] %>% bitr(fromType = "SYMBOL", 
                      toType = "ENTREZID", 
                      OrgDb = "org.Hs.eg.db", 
                      drop = T)
data_mapped <- gene %>% string_db$map(my_data_frame_id_col_names = "ENTREZID", 
                removeUnmappedRows = TRUE)
string_db$plot_network( data_mapped$STRING_id )

