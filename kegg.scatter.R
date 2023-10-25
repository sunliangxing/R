library('ggplot2')
library('ggrepel')
args <- commandArgs(trailingOnly = T)
file_in = args[1]
file_ou = args[2]

D <- read.table(file_in, sep = "\t", comment.char = "", quote = "", header = T, check.names=FALSE)
D$label = D$Description
D$label[D$significant == 'None of Significant'] <- ''
D$label[D$significant == 'Either of Significant'] <- ''
vp <- ggplot(D, aes(x=-log(Transcripts,2),y=-log(Metabolites,2),color=significant,label = label))+
                geom_point()+
#                scale_colour_manual(values=c("red","green","grey"))+
				scale_colour_manual(values=c("green","grey"))+
				geom_text_repel(force=T,color="black",max.overlaps=200)

vp <- vp + labs(y='-log2(Metabolites P_value)',x='-log2(Transcripts P_value)')+theme(title= element_text(size=15, color="black"),axis.title= element_text(size=12, color="black", vjust=0.5, hjust=0.5),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent', color='grey'))+guides(size=FALSE)

pdf(file_ou, width=8,height=6)
print(vp)
dev.off()

