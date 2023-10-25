
data <- read.table("gene_meta.summary.txt",header = TRUE,sep="\t")
library("ggplot2")
data$Description <- factor(data$Description, levels=rev(as.character(data$Description)))

library(reshape2)
data <- melt(data,ID=c("TermID","Description"))
p<-ggplot(data,aes(Description,-log(value),fill=variable))+geom_bar(stat='identity',position="dodge",width=0.5)+coord_flip()+scale_fill_manual(values=c("blue","orange"))
pdf("gene_meta.kegg.pvalue.bar.pdf",width=10,height=10)
p <- p+ labs(x="",y="-log(Pvalue)")+theme(title= element_text(size=12, color="black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12,vjust=1, hjust=1),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.title = element_blank())
p
dev.off()

data <- read.table("gene_meta.counts.txt",header = TRUE,sep="\t")
data$Description <- factor(data$Description, levels=(as.character(data$Description)))

library(reshape2)
data <- melt(data,ID=c("TermID","Description"))
p<-ggplot(data,aes(Description,value,fill=variable))+geom_bar(stat='identity',position="dodge",width=0.5)+scale_fill_manual(values=c("blue","orange"))
pdf("gene_meta.kegg.counts.bar.pdf",width=10,height=10)
p <- p+ labs(x="",y="Number of Transcripts/Metabolites")+theme(title= element_text(size=12, color="black"),axis.text.x = element_text(size=10,vjust=1, hjust=1,angle = 45),axis.text.y = element_text(size=12,vjust=1, hjust=1),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.title = element_blank())
p
dev.off()

