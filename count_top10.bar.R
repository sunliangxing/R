args <- commandArgs(trailingOnly = T)
file_in = args[1]
file_ou = args[2]

data <- read.table(file_in,,sep = '\t',comment.char = '',quote = '',header = T)
data <- data[order(data[['Total']], decreasing = T),]
data <- data[1:10,]
data$Description <- factor(data$Description, levels=(as.character(data$Description)))

library(reshape2)
library(ggplot2)
data <- melt(data,id.vars=c("TermID","Description","Category","Total"))
p<-ggplot(data,aes(Description,value,fill=variable))+geom_bar(stat='identity',position="dodge",width=0.5)+scale_fill_manual(values=c("blue","orange"))
pdf(file_ou,width=10,height=10)
p <- p+ labs(x="",y="Number of Transcripts/Metabolites")+theme(title= element_text(size=12, color="black"),axis.text.x = element_text(size=10,vjust=1, hjust=1,angle = 45),axis.text.y = element_text(size=12,vjust=1, hjust=1),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.title = element_blank())
p
dev.off()

