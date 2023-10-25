args <- commandArgs(trailingOnly = T)
input_g  = args[1]
input_c  = args[2]
prefix = args[3]
data_g<- (read.table(input_g,sep="\t", check.names = FALSE, header=T,row.names=1))
data_c<- (read.table(input_c,sep="\t", check.names = FALSE, header=T,row.names=1))
#data <- subset(data,select=-Name)
data_g <- t(data_g)
data_c <- t(data_c)
#C=cor(data,use="pairwise.complete.obs",method="spearman")

#library(Hmisc)
#res= rcorr(data_g,data_c,type = "spearman")
library(psych)
C <- corr.test(data_g,data_c,method="spearman")

#flattenCorrMatrix <- function(cormat, pmat) {
# 	ut <- upper.tri(cormat)
#	data.frame(
# 		row = rownames(cormat)[row(cormat)[ut]],
#		column = rownames(cormat)[col(cormat)[ut]],
#		cor  =(cormat)[ut],
#		p = pmat[ut]
#	)
#}
#final<-flattenCorrMatrix(C$r,C$p)
library(reshape2)
M_c=melt(data=C$r)
colnames(M_c) <- c('gene',"compound","cor")
M_p=melt(data=C$p)
colnames(M_p) <- c('gene',"compound","pval")
#M <- merge(x=M_c,y=M_p,by.x='gene',by.y="compound")
M_c$pval = M_p$pval
M_c <- M_c[order(M_c$pval,decreasing=F),]
write.table(x=M_c, file=paste0(prefix,".COR.xls"),quote=F,row.names=F,sep="\t")


#write.table(x=C,paste0(prefix,".COR.xls"),sep="\t",quote=F)
library("pheatmap")
pheatmap(C$r,filename=paste0(prefix,".COR.noname.pdf"), width=10, height=10,show_rownames = F,show_colnames = F)
pheatmap(C$r,filename=paste0(prefix,".COR.pdf"), width=10, height=10,show_rownames = T,show_colnames = T,fontsize_row = 2, fontsize_col = 2, angle_col = 45)
#pheatmap(C$r,filename=paste0(prefix,".COR.nocluster.png"), width=10, height=10,show_rownames = F,show_colnames = F,cluster_rows = FALSE,cluster_cols = FALSE)
