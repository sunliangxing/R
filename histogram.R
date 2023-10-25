library(ggrepel)#避免文字重叠
library(reshape2)
library(forcats)
library(ggsci)
library(Rmisc)
library(grid)
library(gridBase)
library(stringr)
library(scales)
histogram<-function(datafile=NULL,datafile1=NULL,datafile2=NULL,fill_color=1,
                    angle=90,family="serif",face=1,width=0.7,y="GeneCount",trans="na",
                    text_size=10,top_value=20,ysort="P_value",
                    x_text_color="black",filename="barplot",type="png",sign=T,
                    low_high_color=1 ,module="facet"){
  plot_name=filename
  plot_name1=paste(plot_name,"pdf",sep=".")
  if(fill_color==1){
    color_bar=pal_npg()(10)
  }else if(fill_color==2){
    color_bar=pal_aaas()(10)
  }else if(fill_color==3){
    color_bar=pal_nejm()(8)
  }else if(fill_color==4){
    color_bar=pal_jama()(7)
  }else if(fill_color==5){
    color_bar=pal_jco()(10)
  }else if(fill_color==6){
    color_bar=pal_tron()(7)
  }else if(fill_color==7){
    color_bar=pal_simpsons()(10)
  }else if(fill_color==8){
    color_bar=pal_ucscgb()(10)
  }else{
    color_bar=pal_futurama()(10)
  }
  if(low_high_color==1){
    low_high_color1<-c("blue","red")
  }else if(low_high_color==2){
    low_high_color1<-c("#0564a8","#e75500")
  }else if(low_high_color==3){
    low_high_color1<-c("#2ea99e","#e2862a")
  }else if(low_high_color==4){
    low_high_color1<-c("#343399","#fddb57")
  }else if(low_high_color==5){
    low_high_color1<-c("#1f6c98","#fd9b2e")
  }else if(low_high_color==6){
    low_high_color1<-c("#009899","#fe0034")
  }else if(low_high_color==7){
    low_high_color1<-c("#5ac4ad","#ff7659")
  }else if(low_high_color==8){
    low_high_color1<-c("#66669a","#ff999a")
  }else{
    low_high_color1<-c("#0ed0c0","#c75ca7")
  }
  if(module!="bothside"){
    data<- read.table(datafile, sep = "\t", comment.char = "", quote = "", header = T, check.names=FALSE)
    if(top_value>nrow(data)){
      top_value=nrow(data)
    }
    if(ysort=="P_value"){
      data<-data[order(data$P_value),]
      plotdata<-data[1:top_value,]
    }else if(ysort=="P_adjust"){
      data<-data[order(data$P_adjust),]
      plotdata<-data[1:top_value,]
    }else{
      data<-data[order(data$GeneCount,decreasing = T),]
      plotdata<-data[1:top_value,]
    }
    if(y=="P_value"){
      yvalue=-log10(plotdata$P_value)
    }else if(y=="P_adjust"){
      yvalue=-log10(plotdata$P_adjust)
    }else{
      yvalue=plotdata$GeneCount
    }
    if(trans=="log2"){
      yvalue<-log2(yvalue)
    }else if(trans=="log10"){
      yvalue<-log10(yvalue)
    }else{
      yvalue<-yvalue
    }
    plotdata$yvalue<-yvalue
    a<-unique(plotdata$Category)
    color=color_bar
    color=color[1:length(a)]
    i=1
    text_color=c()
    a<-sort(as.factor(a))
    for(i in 1:length(a)){
      length=length(which(plotdata$Category==a[i]))
      b<-rep(color[i],length)
      text_color=c(text_color,b)
    }
    if("_"%in%as.character(unlist(strsplit(as.vector(a[1]),split = "")))){
      i=1
      Cate_name<-c()
      Catename<-c()
      for(i in 1:length(a)){
        Catename=unlist(strsplit(as.vector(a[i]),split = "_"))
        Catename=substr(Catename, start = 1,stop = 1)
        Catename=paste(Catename,sep = "", collapse = '')
        Cate_name<-c(Cate_name,Catename)
      }
    }
    else{
      Cate_name=sapply(stringr::str_extract_all(a, '[A-Z]'),paste0, collapse = '')
    }
    Cate_name<-toupper(Cate_name)
    Cate_name1=as.vector(a)
    names(Cate_name)<-Cate_name1
    plotdata$P_value<-as.numeric(plotdata$P_value)
    i=1
    for (i in 1:nrow(plotdata)) {
      if(plotdata$P_value[i]<=0.05&&plotdata$P_value[i]>0.01){
        plotdata$sig[i]<-"*"
      }else if(plotdata$P_value[i]<=0.01&&plotdata$P_value[i]>0.001){
        plotdata$sig[i]<-"**"
      }else if(plotdata$P_value[i]<=0.001){
        plotdata$sig[i]<-"***"
      }else{
        plotdata$sig[i]<-NA
      }
    }
    i=1
    for(i in 1:length(plotdata$Description)){
      if(nchar(as.character(plotdata$Description[i]))>40){
        o<-strsplit(as.character(plotdata$Description[i]),split = '')[[1]]
        p<-o[-c(41:length(o))]
        p<-c(p,"...")
        p<-paste(p,sep = "",collapse = "")
        if(T%in%(p%in%plotdata$Description)){
          p<-paste(" ",p,sep = "")
        }
        plotdata$Description[i]<-p
      }
    }
  }
  if(module=="facet"){
    if(x_text_color!="black"){
      x_text_color<-text_color
    }
    plotdata$Description = with(plotdata, factor(Description, 
                                                 levels=Description[order(plotdata$Category,-yvalue)]))
    p<-ggplot(plotdata,aes(reorder(Description,yvalue),yvalue,fill=-log10(P_value)))+
      geom_bar(stat="identity",position="dodge",width = width)+
      scale_fill_gradient(low=low_high_color1[1],high=low_high_color1[2])+
      theme_bw() + theme(panel.grid=element_blank())+
      theme(axis.text = element_text(size=text_size,family = family,face=face,color=x_text_color,angle = 0),
            legend.key.size = unit(0.2,"cm"),
            legend.text =element_text(size=text_size,family = family,face=face,color="black"),
            legend.title = element_text(size=text_size,family = family,face=face,color="black"),
            axis.title.y = element_blank(),strip.text = element_text(size = 10),
            axis.title.x=element_text(size=text_size,family = family,face=face,color="black"))+
      theme(axis.ticks=element_line(color="black",linewidth =0.1))+
      facet_grid(plotdata$Category~., scales = "free", space = "free",labeller = as_labeller(Cate_name))+
      coord_flip()
    if(sign==T){
      p<-p+geom_text(aes(x=Description,y=yvalue,label=sig),size=5,vjust=0.5,hjust=0.5)
    }
    if(y=="P_value"){
      p<-p+labs(y="-log(p-value)")
    }else if(y=="P_adjust"){
      p<-p+labs(y="-log(p_adjust)")
    }else{
      if(trans!="na"){
        p<-p+labs(y="log(Genecount)")
      }else{
        p<-p+labs(y="Genecount")
      }
    }
    ggsave(plot_name1,p,height=8.5,width =8)
  }
  if(module=="normal"){
    if(x_text_color!="black"){
      x_text_color<-text_color
    }
    plotdata$Description = with(plotdata, factor(Description, 
                                                 levels=Description[order(plotdata$Category,-yvalue)]))
    p<-ggplot(plotdata,aes(as.factor(Description),yvalue,fill=str_wrap(Category,18)))+
      geom_bar(stat="identity",position="dodge",width = width)+
      scale_fill_manual(values=color)+
      scale_y_continuous(limits=c(0,max(yvalue)+(max(yvalue)/5)*0.1),expand = c(0,0))+
      theme_classic() + theme(panel.grid=element_blank())+
      theme(axis.line = element_line(colour = "black",linewidth=0.2),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = angle,hjust=1,size=text_size,family = family,face=face,color=x_text_color),
            legend.text =element_text(size=text_size,family = family,face=face,color="black"),
            legend.title = element_blank(),legend.position = "right",
            axis.title.y =element_text(size=text_size,family = family,face=face,color="black"),
            axis.text.y =element_text(size=text_size,family = family,face=face,color="black"),
      )+theme(axis.ticks=element_line(color="black",linewidth=0.2))
    if(sign==T){
      p<-p+geom_text(aes(x=Description,y=yvalue,label=sig),size=5,vjust=0.5,hjust=0.5)
    }
    if(y=="P_value"){
      p<-p+labs(y="-log(p-value)")+
        geom_hline(aes(yintercept=-log10(0.05)),color="black",alpha=0.7,linetype=2)+
        geom_text(aes(x=0.5,y=-log10(0.05)-0.1),label="p-value=0.05",size=3,color="black",alpha=0.7,hjust=0,vjust=0.5)
    }else if(y=="P_adjust"){
      p<-p+labs(y="-log(p-adjust)")+
        geom_hline(aes(yintercept=-log10(0.05)),color="black",alpha=0.7,linetype=2)+
        geom_text(aes(x=0.5,y=-log10(0.05)-0.1),label="p-value=0.05",size=1.3,color="black",alpha=0.7,hjust=0,vjust=0.5)
    }else{
      if(trans!="na"){
        p<-p+labs(y="log(Genecount)")
      }else{
        p<-p+labs(y="Genecount")
      }
    }
    ggsave(plot_name1,p,height=8.5,width =8)
  }
  if(module=="xvar"){
    plotdata<-plotdata[order(plotdata$Category),]
    plotdata<-data.frame(plotdata$Description,plotdata$Category,plotdata$P_value,
                         plotdata$P_adjust,plotdata$GeneCount,plotdata$yvalue,plotdata$sig)
    colnames(plotdata)<-c("Description","Category","P_value","P_adjust","GeneCount","yvalue","sig")
    i=1
    length1<-c()
    length2<-c()
    a<-as.character(a)
    for(i in 1:length(a)){
      yihang=c(paste(" ",a[i],sep = ""),a[i],0,0,0,0,NA)
      length=length(which(plotdata[,2]==a[i]))
      length1=sum(length1,length)
      length2=c(length2,length1)
      if(i==length(a)){
        plotdata<-rbind(plotdata,yihang)
      }else{
        plotdata<-rbind(plotdata[1:(length1+i-1),], yihang, plotdata[(i+length1):nrow(plotdata),])
      }
    }
    plotdata$yvalue<-as.numeric(plotdata$yvalue)
    if(y=="P_value"){
      yvalue=round(plotdata$yvalue,2)
      yvalue[which(yvalue==0)]<-NA
    }else if(y=="P_adjust"){
      yvalue=round(plotdata$yvalue,2)
      yvalue[which(yvalue==0)]<-NA
    }else{
      yvalue=plotdata$yvalue
      yvalue[which(yvalue==0)]<-NA
    }
    yvalue<-as.numeric(yvalue)
    i=1
    text_color<-c()
    for(i in 1:length(a)){
      length=length(which(plotdata$Category==a[i]))
      b<-rep(color[i],length)
      text_color=c(text_color,b)
    }
    text_color4<-text_color
    text_color4[!is.na(yvalue)]<-"black"
    text_color[is.na(yvalue)]<-"black"
    if(x_text_color!="black"){
      x_text_color<-text_color
    }else{
      x_text_color<-text_color4
    }
    plotdata$Description = with(plotdata, factor(Description, 
                                                 levels=Description[order(plotdata$Category,-yvalue)]))
    p<-ggplot(plotdata,aes(Description,yvalue,fill=as.factor(Category)))+
      geom_col(width = width)+
      scale_fill_manual(values=color)+
      theme_classic() + theme(panel.grid=element_blank())+
      theme(axis.text.y = element_text(size=text_size,family = family,face=face,color=x_text_color),
            axis.title.y = element_blank(),
            legend.text =element_text(size=text_size,family = family,face=face,color="black"),
            legend.title = element_blank(),
            axis.text.x=element_text(size=text_size,family = family,face=face,color="black"),
            axis.title.x=element_text(size=text_size,family = family,face=face,color="black"),
            axis.line = element_line(colour = "black",linewidth=0.2))+
      theme(axis.ticks= element_blank())+
      coord_flip()
    if(sign==T){
      p<-p+geom_text(aes(x=Description,y=yvalue,label=sig),size=4,vjust=0.5,hjust=0.5)
    }
    if(y=="P_value"){
      p<-p+scale_y_continuous(limits=c(0,max(plotdata$yvalue)+0.5),expand = c(0,0))+
        labs(y="-log(p-value)")
    }else if(y=="P_adjust"){
      p<-p+scale_y_continuous(limits=c(0,max(plotdata$yvalue)+0.5),expand = c(0,0))+
        labs(y="-log(p-adjust)")
    }else{
      p<-p+scale_y_continuous(limits=c(0,round(max(plotdata$yvalue)+max(plotdata$yvalue)/5,0)),expand = c(0,0))
      if(trans!="na"){
        p<-p+labs(y="log(Genecount)")
      }else{
        p<-p+labs(y="Genecount")
      }
    }
    ggsave(plot_name1,p,height=8.5,width =8)
  }
  if(module=="yvar"){
    plotdata<-plotdata[order(plotdata$Category),]
    i=1
    length1<-c()
    length2<-c()
    length3<-c()
    text_color=c()
    b<-c()
    for(i in 1:length(a)){
      length=length(which(plotdata$Category==a[i]))
      length1=sum(length1,length)
      length2=c(length2,length1)
      if(i==1){
        length3=c(round(0+length2[1]/2,1))
      }else{
        length3=c(length3,round((length2[i]+length2[i-1])/2,1))
      }
      b<-rep(color[i],length)
      text_color=c(text_color,b)
    }
    if(x_text_color!="black"){
      x_text_color<-text_color
    }
    length4<-c(0+width/2,length2[-length(length2)])
    if(y=="P_value"){
      line_df <- data.frame(
        x = length4+width/2,
        y = max(yvalue),
        xend = length2+width/2,
        yend = max(yvalue))
      text_df<-data.frame(x = length3,
                          y = max(yvalue),
                          label=a)
    }else if(y=="P_adjust"){
      line_df <- data.frame(
        x = length4+width/2,
        y = max(yvalue),
        xend = length2+width/2,
        yend = max(yvalue))
      text_df<-data.frame(x = length3,
                          y = max(yvalue),
                          label=a)
    }else{
      line_df <- data.frame(
        x = length4+width/2,
        y = max(yvalue),
        xend = length2+width/2,
        yend = max(yvalue)
      )
      text_df<-data.frame(x = length3,
                          y = max(yvalue),
                          label=a)
    }
    plotdata$Description = with(plotdata, factor(Description, 
                                                 levels=Description[order(plotdata$Category,yvalue)]))
    p<-ggplot(plotdata,aes(Description,yvalue,fill=Category))+
      geom_bar(stat="identity",position="dodge",width=width)+
      scale_fill_manual(values=color)+
      theme_bw() + theme(panel.grid=element_blank())+
      theme(panel.border=element_blank(),
            axis.title.y = element_blank(),
            axis.text.y= element_text(size=text_size,family = family,face=face,color=x_text_color),
            axis.text.x= element_text(size=text_size,family = family,face=face),
            legend.position =  "none",
            axis.title.x=element_text(size=text_size,family = family,face=face,color="black"),
            axis.line = element_line(colour = "black",linewidth=0.3))+
      theme(axis.ticks=element_blank())+
      coord_flip()+
      geom_segment(data = line_df, mapping = aes(x=x, y=y+max(yvalue)/8, xend=xend, yend=yend+max(yvalue)/8), color=color,linewidth=2,inherit.aes = FALSE)+
      geom_text(data=text_df,mapping=aes(x=x,y=y+max(yvalue)/7,label=str_wrap(label, width=18)),size=4,vjust=0,hjust=0,inherit.aes = FALSE,family=family)
    if(sign==T){
      p<-p+geom_text(aes(x=Description,y=yvalue,label=sig),size=4,vjust=0.5,hjust=0.5)
    }
    if(y=="P_value"){
      p<-p+scale_y_continuous(limits=c(0,round(max(yvalue)+max(yvalue)/2,2)),expand = c(0,0))+
        labs(y="log(p_value)")
    }else if(y=="P_adjust"){
      p<-p+scale_y_continuous(limits=c(0,round(max(yvalue)+max(yvalue)/2,2)),expand = c(0,0))+
        labs(y="log(p-adjust)")
    }else{
      p<-p+scale_y_continuous(limits=c(0,round(max(yvalue)+max(yvalue)/2,0)),expand = c(0,0))
      if(trans!="na"){
        p<-p+labs(y="log(Genecount)")
      }else{
        p<-p+labs(y="Genecount")
      }
    }
    ggsave(plot_name1,p,height=8.5,width =8)
  }
  if(module=="bothside"){
    data_up<-read.table(datafile1, sep = "\t", comment.char = "", quote = "", header = T, check.names=FALSE)
    data_down<- read.table(datafile2, sep = "\t", comment.char = "", quote = "", header = T, check.names=FALSE)
    if(ysort=="P_value"){
      data_up<-data_up[order(data_up$P_value),]
      data_down<-data_down[order(data_down$P_value),]
    }else if(ysort=="P_adjust"){
      data_up<-data_up[order(data_up$P_adjust),]
      data_down<-data_down[order(data_down$P_adjust),]
    }else{
      data_up<-data_up[order(data_up$GeneCount,decreasing = T),]
      data_down<-data_down[order(data_down$GeneCount,decreasing = T),]
    }
    if(top_value>nrow(data_up)){
      data_up<-data_up
    }else{
      data_up<-data_up[1:top_value,]
    }
    if(top_value>nrow(data_down)){
      data_down<-data_down
    }else{
      data_down<-data_down[1:top_value,]
    }
    if(y=="P_value"){
      yvalue_up=-log10(data_up$P_value)
      yvalue_down=-log10(data_down$P_value)
    }else if(y=="P_adjust"){
      yvalue_up=-log10(data_up$P_adjust)
      yvalue_down=-log10(data_down$P_adjust)
    }else{
      yvalue_up=data_up$GeneCount
      yvalue_down=data_down$GeneCount
    }
    if(trans=="log2"){
      yvalue_up<-log2(yvalue_up)
      yvalue_down<-log2(yvalue_down)
    }else if(trans=="log10"){
      yvalue_up<-log10(yvalue_up)
      yvalue_down<-log10(yvalue_down)
    }else{
      yvalue_up<-yvalue_up
      yvalue_down<-yvalue_down
    }
    yvalue_down<-(-yvalue_down)
    yvalue<-c(yvalue_up,yvalue_down)
    data_up$var<-"Up"
    data_down$var<-"Down"
    if(T%in%(data_up$Description%in%data_down$Description)){
      data_up$Description[data_up$Description%in%data_down$Description]<-paste(data_up$Description[data_up$Description%in%data_down$Description]," ",sep = "")
    }
    data_up<-cbind(data_up$Description,data_up$Category,data_up$P_value,data_up$P_adjust,yvalue_up,data_up$var)
    colnames(data_up)<-c("Description","Category","P_value","P_adjust","yvalue","var")
    data_down<-cbind(data_down$Description,data_down$Category,data_down$P_value,data_down$P_adjust,yvalue_down,data_down$var)
    colnames(data_down)<-c("Description","Category","P_value","P_adjust","yvalue","var")
    plotdata3<-rbind(data_up,data_down)
    colnames(plotdata3)<-c("Description","Category","P_value","P_adjust","yvalue","var")
    plotdata3<-as.data.frame(plotdata3)
    plotdata3$yvalue<-as.numeric(plotdata3$yvalue)
    plotdata3$P_value<-as.numeric(plotdata3$P_value)
    i=1
    plotdata3$sig<-NA
    for (i in 1:nrow(plotdata3)) {
      if(plotdata3$P_value[i]<=0.05&plotdata3$P_value[i]>0.01){
        plotdata3$sig[i]<-"*"
      }else if(plotdata3$P_value[i]<=0.01&&plotdata3$P_value[i]>0.001){
        plotdata3$sig[i]<-"**"
      }else if(plotdata3$P_value[i]<=0.001){
        plotdata3$sig[i]<-"***"
      }else{
        plotdata3$sig[i]<-NA
      }
    }
    i=1
    for(i in 1:length(plotdata3$Description)){
      if(nchar(as.character(plotdata3$Description[i]))>60){
        o<-strsplit(as.character(plotdata3$Description[i]),split = '')[[1]]
        p<-o[-c(61:length(o))]
        p<-c(p,"...")
        p<-paste(p,sep = "",collapse = "")
        plotdata3$Description[i]<-p
      }
    }
    p<-ggplot(plotdata3,aes(reorder(Description,yvalue),yvalue,fill=var))+
      geom_bar(stat="identity",position="dodge",width = width)+
      scale_fill_manual(values=low_high_color1,na.translate=FALSE)+
      geom_text(data=plotdata3[plotdata3$var=="Up",],aes(x = Description, y = -(max(abs(plotdata3$yvalue))/3*0.25), label = Description), hjust=1,size = 4,family=family,fontface=face)+
      geom_text(data=plotdata3[plotdata3$var=="Down",],aes(x = Description, y = max(abs(plotdata3$yvalue))/3*0.25, label = Description), hjust=0,size = 4,family=family,fontface=face)+
      theme_bw() + theme(panel.grid=element_blank())+
      theme(panel.border=element_blank(),
            axis.text.y = element_blank(),
            legend.text =element_text(size=text_size,family = family,face=face,color="black"),
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size=text_size,family = family,face=face,color="black"),
            axis.title.x =element_text(size=text_size,family = family,face=face,color="black"),
            axis.line.x = element_line(colour = "black",linewidth = 0.2))+
      theme(axis.ticks.length.x = unit(0.1,"cm"),axis.ticks.x=element_line(color="black",linewidth=0.2),axis.ticks.y = element_blank())+
      guides(fill = guide_legend(reverse = TRUE))+
      coord_flip()
    if(sign==T){
      p<-p+geom_text(data=plotdata3[plotdata3$var=="Up",],aes(x = Description, y = yvalue, label = sig), hjust=0,size = 4)+
        geom_text(data=plotdata3[plotdata3$var=="Down",],aes(x = Description, y = yvalue, label = sig), hjust=1,size = 4)
    }
    if(y=="P_value"){
      bre=round(max(abs(plotdata3$yvalue))/3,2)
      p<-p+labs(y="log(P_value)")+
        scale_y_continuous(limits = c(-max(abs(plotdata3$yvalue)),max(abs(plotdata3$yvalue))),
                           breaks = c(-bre*3,-bre*2,-bre,0,bre,bre*2,bre*3),
                           labels = c(bre*3,bre*2,bre,0,bre,bre*2,bre*3))
    }else if(y=="P_adjust"){
      bre=round(max(abs(plotdata3$yvalue))/3,2)
      p<-p+scale_y_continuous(limits = c(-max(abs(plotdata3$yvalue)),max(abs(plotdata3$yvalue))),
                              breaks = c(-bre*3,-bre*2,-bre,0,bre,bre*2,bre*3),
                              labels = c(bre*3,bre*2,bre,0,bre,bre*2,bre*3))+
        labs(y="log(P_adjust)")
    }else{
      bre=round(max(abs(plotdata3$yvalue))/3,0)
      p<-p+scale_y_continuous(limits = c(-max(abs(plotdata3$yvalue)),max(abs(plotdata3$yvalue))),
                              breaks = c(-bre*3,-bre*2,-bre,0,bre,bre*2,bre*3),
                              labels = c(bre*3,bre*2,bre,0,bre,bre*2,bre*3))
      if(trans!="na"){
        p<-p+labs(y="log(Genecount)")
      }else{
        p<-p+labs(y="Genecount")
      }
    }
    ggsave(plot_name1,p,height=8.5,width =8)
  }
}
