setwd("C:/Users/liang/Desktop/工作内容/研发/报告/报告markdown/")
params<-list(param1="YX202310131323",param2="FX202310131323",param3="XXX",param4="元莘生物",param5="6个小球藻",
             param6=as.numeric(0.05),param7=as.numeric(1),
             param8=log2(as.numeric(1)),param9=as.numeric(0.05),
             param10=log2(as.numeric(2)),param11=as.numeric(0.05))
library(rmarkdown)
library(rticles)
library(tidyr)
render(input = "report-pdf.Rmd",output_file = "report-pdf.pdf",params = params)
