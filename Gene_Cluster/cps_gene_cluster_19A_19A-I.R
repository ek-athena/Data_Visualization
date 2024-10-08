setwd("E:/Github/Gene_Cluster/")
library(ggplot2)
library(gggenes)
genes <- read.table("19A_cps.txt", header=TRUE, sep="\t")
genes
ggplot(genes, aes(xmin = start, xmax = end, y = molecule, fill = gene, label = gene)) + 
  geom_gene_arrow(arrowhead_height = unit(8, "mm"), arrowhead_width = unit(3, "mm"), arrow_body_height = unit(5, "mm")) +
  facet_wrap(~ molecule, scales = "free", ncol = 1) + 
  geom_gene_label(align = "centre", grow = TRUE) +
  theme_genes()


genes1 <- read.table("19A_subtype_I_cps.txt", header=TRUE, sep="\t")
genes1
ggplot(genes1, aes(xmin = start, xmax = end, y = molecule, fill = gene, label = gene)) + 
  geom_gene_arrow(arrowhead_height = unit(8, "mm"), arrowhead_width = unit(3, "mm"), arrow_body_height = unit(5, "mm")) +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  theme_genes()
