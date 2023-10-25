library(pathview)
args <- commandArgs(trailingOnly = T)
file_enrich <- args[1]
file_gene   <- args[2]
file_cpd    <- args[3]
#show_type   <- args[4]

# file_enrich <- "gene_compound.association.kegg.xls"
# file_gene   <- "../Group.M9_LSC_vs_WT_LSC.degene_all.xls"
# file_cpd    <- "../Group.M9_LSC_vs_WT_LSC.pos.xls"
show_type   <- "log2fc"

data_enrich <- read.table(file = file_enrich, header = T,sep = '\t',quote = '')
path.ids    <- as.character(data_enrich$TermID[data_enrich$Combine_P_values < 0.1])
path.ids2   <- gsub(pattern = "rno", replacement = "", x = path.ids)
#path.ids2   <- path.ids
#path.ids2   <- c("rno00230","rno04912","rno04921")


data_gene   <- read.table(file = file_gene, header = T)
#z_gene      <- data.frame(id=data_gene$unigene_id, log2fc = data_gene[,2], mean_a = data_gene[,2], mean_b = data_gene[,3])
z_gene      <- data.frame(id=data_gene$id, log2fc = data_gene[,4])
#z_gene      <- data.frame(id=data_gene$ko, log2fc = data_gene[,4], mean_a = data_gene[,2], mean_b = data_gene[,3])
rownames(z_gene) <- z_gene$id

data_cpd    <- read.table(file = file_cpd,  header = T, quote = "", sep="\t", comment.char = "")
data_cpd    <- data_cpd[!is.na(data_cpd$kegg_id),]
#data_cpd$kegg_id <- gsub(pattern = "cpd:", replacement = "", x = data_cpd$kegg_id)
z_cpd       <- data.frame(id=data_cpd$kegg_id, log2fc = data_cpd[,4], vip = data_cpd[,7])
rownames(z_cpd) <- z_cpd$id

if(show_type == "log2fc"){
  a = z_gene[,2]
  names(a) <- row.names(z_gene)
  b = z_cpd[,2]
  names(b) <- row.names(z_cpd)
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data = b, 
                                                          kegg.dir = '~/work/RNA_PROGREM/bioanalysis/YX20230310601-fanfei-mrna_cpd/FX230310161306/gene_compound_intersection/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno", 
                                                          kegg.native = T, 
                                                          same.layer = T, 
                                                          out.suffix='log2fc.type_a'))
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data  = b, 
                                                          kegg.dir  = '~/work/RNA_PROGREM/bioanalysis/YX20230310601-fanfei-mrna_cpd/FX230310161306/gene_compound_intersection/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno",
                                                          kegg.native = T, 
                                                          same.layer = F, 
                                                          out.suffix='log2fc.type_b'))
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data  = b, 
                                                          kegg.dir  = '~/work/RNA_PROGREM/bioanalysis/YX20230310601-fanfei-mrna_cpd/FX230310161306/gene_compound_intersection/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno",
                                                          kegg.native = F, 
                                                          same.layer = T, 
                                                          out.suffix='log2fc.type_c'))
}else if(show_type == "expr"){
  a = log2(z_gene[,c(3,4)]+0.1)
  b = log10(z_cpd[,c(3,4)]+0.1)
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data = b, 
                                                          kegg.dir = '~/work/RNA_PROGREM/bioanalysis/YX20210308501-xuyanping-bioanalysis/20210420/script/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno", 
                                                          limit = list(gene=range(a),cpd=range(b)),
                                                          kegg.native = T, 
                                                          same.layer = T, 
                                                          out.suffix='expr.type_a'))
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data  = b, 
                                                          kegg.dir  = '~/work/RNA_PROGREM/bioanalysis/YX20210308501-xuyanping-bioanalysis/20210420/script/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno",
                                                          limit = list(gene=range(a),cpd=range(b)),
                                                          kegg.native = T, 
                                                          same.layer = F, 
                                                          out.suffix='expr.type_b'))
  pv.out.list <- sapply(path.ids2, function(pid) pathview(gene.data = a, 
                                                          cpd.data  = b, 
                                                          kegg.dir  = '~/work/RNA_PROGREM/bioanalysis/YX20210308501-xuyanping-bioanalysis/20210420/script/tmp', 
                                                          gene.idtype = 'ENSEMBL', 
                                                          cpd.idtype = 'kegg', 
                                                          pathway.id = pid, 
                                                          species = "rno",
                                                          limit = list(gene=range(a),cpd=range(b)),
                                                          kegg.native = F, 
                                                          same.layer = T, 
                                                          out.suffix='expr.type_c'))
}
