args <- commandArgs(trailingOnly = T)
file_in = args[1]
file_ou = args[2]

data <- read.table(file_in,sep = '\t',comment.char = '',quote = '',header = T)

library("ggplot2")
data$Description <- factor(data$Description, levels=rev(as.character(data$Description)))

library(reshape2)
data <- melt(data,id.vars=c("TermID","Description","Category"))
p<-ggplot(data,aes(Description,-log(value,2),fill=variable))+geom_bar(stat='identity',position="dodge",width=0.5)+coord_flip()+scale_fill_manual(values=c("blue","orange"))
p<-p+geom_hline(yintercept=-log(0.05,2), lty=3,color = 'black', lwd=0.8)
pdf(file_ou,width=10,height=10)
p <- p+ labs(x="",y="-log2(Pvalue)")+theme(title= element_text(size=12, color="black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12,vjust=1, hjust=1),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.title = element_blank())
p
dev.off()
