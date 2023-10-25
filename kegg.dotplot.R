library('ggplot2')
#args <- commandArgs(trailingOnly = T)
args <- commandArgs(trailingOnly = T)
file_in = args[1]
file_ou = args[2]


target  = 'Enrichment Score'
bysize  = 'GeneCount'
bycolor = "pvalue"

df <- read.table(file_in,sep = '\t',comment.char = '',quote = '',header = T)
#df$GeneRatio[df$GeneRatio > 0.1]=0.1
#df$GeneRatio[df$GeneRatio < -0.1]=-0.1
p <- ggplot(df, aes_string(x='Rich_Factor', y='Description', size='GeneCount', color='P_value',shape ='Omics'))
  p <- p + geom_point() 
  p <- p + scale_size_continuous(range=c(4,6))
  p <- p + scale_color_continuous(low="red", high="blue", name = bycolor, guide=guide_colorbar(reverse=TRUE))
  p <- p + xlab(target) + ylab(NULL)
  p <- p + facet_wrap(~Omics)
 # p <- p + xlim(-0.3,0.3)
  p <- p + theme( 
				panel.border = element_rect(colour = 'black', fill=NA, size=1),
				panel.background = element_rect(fill="white",color="grey50"),
				panel.grid.major = element_blank(),
				axis.text=element_text(size=12),axis.title=element_text(size=14,hjust=0.5),
				strip.text = element_text(size = 12,face="bold")
 				 ) 
  pdf(file = file_ou, width = 10, height = 8)
  print(p)
  dev.off()
#}


