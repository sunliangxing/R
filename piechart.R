#小工具饼图的设计
library(openxlsx)
library(ggplot2)
library(plotrix)
library(ggsci)
library(cowplot)
library(stringr)
library(ggrepel)
library(RColorBrewer)
#预计的参数：输入矩阵，只用一张表用read.table,看后缀如果是xlsx则用openxlsx如果是其他则用read.table
  #颜色：color、饼图距离：expand
Piechart<-function(datafilename,colors=NULL,module,order="number",color_expand="white",stastic=T,color_mumal=NULL){
  #读入数据
  a<-strsplit(datafilename,split = ".",fixed = T)[[1]]
  if(a[length(a)]=="xlsx"){
    data<-read.xlsx(datafilename,sheet = 1,startRow = 1,colNames = T)
  }else{
    data<-read.table(datafilename,header=T,sep="\t",quote = "",fill = T)
  }
  #数据处理部分
  if(stastic==T){
    data[is.na(data)]<-"Unknown"
    color1<-list()
    plotdata<-list()
    o=1
    for(o in 1:ncol(data)){
      i=1
      c<-unique(data[,o])
      sumnumer<-nrow(data)
      number<-c()
      percentage<-c()
      num_text<-c()
      for (i in 1:length(c)) {
        number<-c(number,length(data[which(data[,o]==c[i]),o]))
        percentage<-c(percentage,round(number[i]/sumnumer,4))
        num_text<-c(num_text,paste(percentage[i]*100,"%",sep=""))
      }
      plotdata[[o]]<-data.frame(class=c,number=number,percentage=percentage,num_text=num_text)
      if(order=="number"){
        if(length(c)>9){
          plotdata[[o]]<-plotdata[[o]][order(plotdata[[o]]$number,decreasing = T),]
          other<-plotdata[[o]][10:nrow(plotdata[[o]]),]
          other<-data.frame(class="others",number=sum(other$number),percentage=sum(other$percentage),num_text=paste(sum(other$percentage)*100,"%",sep=""))
          plotdata[[o]]<-rbind(plotdata[[o]][1:9,],other)
        }
      }else if(order=="percentage"){
        other<-plotdata[[o]][which(plotdata[[o]]$percentage<0.01),]
        if(nrow(other)!=0){
          other<-data.frame(class="others",number=sum(other$number),percentage=sum(other$percentage),num_text=paste(sum(other$percentage)*100,"%",sep=""))
          plotdata[[o]]<-rbind(plotdata[[o]][which(!(plotdata[[o]]$percentage<0.01)),],other)
        }
        
      }
      names(plotdata)[o]<-colnames(data)[o]
      plotdata[[o]]$class<-paste0(plotdata[[o]]$class," [",plotdata[[o]]$num_text,"]")
      plotdata[[o]]$class<-str_wrap(plotdata[[o]]$class,width = 47)
    }
    write.xlsx(plotdata[[1]],file = "pie-stastic.xlsx")
  }else{
    plotdata<-list()
    if(sum(data[,2])!=1){
      plotdata[[1]]<-data
      percentage<-c()
      num_text<-c()
      i=1
      for (i in 1:nrow(data)) {
        sumnum<-sum(data[,2])
        percentage<-c(percentage,round(plotdata[[1]][i,2]/sumnum,4))
        num_text<-c(num_text,paste0(percentage[i]*100,"%"))
      }
      plotdata[[1]]<-cbind(plotdata[[1]],percentage=percentage,num_text=num_text)
      colnames(plotdata[[1]])<-c("class","number","percentage","num_text")
    }else{
      plotdata[[1]]<-data
      names(plotdata)[1]<-colnames(data)[1]
      number<-data[,2]*100
      num_text<-paste0(data[,2]*100,"%")
      plotdata[[1]]<-cbind(plotdata[[1]],number=number,num_text=num_text)
      plotdata[[1]]<-plotdata[[1]][,c(1,3,2,4)]
      colnames(plotdata[[1]])<-c("class","number","percentage","num_text")
    }
    names(plotdata)[1]<-colnames(data)[1]
    plotdata[[1]]$class<-paste0(plotdata[[1]]$class," [",plotdata[[1]]$num_text,"]")
    plotdata[[1]]$class<-str_wrap(plotdata[[1]]$class,width = 47)
  }
  
  #获取颜色
  if(is.null(color_mumal)){
    #number，10颜色处理
    if(colors==1){
      color=pal_npg("nrc")(10)
    }else if(colors==2){
      color=pal_aaas("default")(10)
    }else if(colors==3){
      color=pal_ucscgb("default")(10)
    }else if(colors==4){
      color=pal_d3("category20")(10)
    }else if(colors==5){
      color=pal_igv("default")(10)
    }else if(colors==6){
      color=pal_futurama("planetexpress")(10)
    }else if(colors==7){
      color=pal_rickandmorty("schwifty")(10)
    }else if(colors==8){
      color=pal_simpsons("springfield")(10)
    }
  }else{
    #percentage颜色处理部分
    color1<-list()
    o=1
    for(o in 1:length(plotdata)){
      if(color_mumal==1){
        color1[[o]]<-colorRampPalette(colors = brewer.pal(8,"Set1")[1:8])(nrow(plotdata[[o]]))
      }else if(color_mumal==2){
        color1[[o]]<-colorRampPalette(colors = brewer.pal(8,"Set2")[1:8])(nrow(plotdata[[o]]))
      }else if(color_mumal==3){
        color1[[o]]<-colorRampPalette(colors = brewer.pal(8,"Set3")[1:8])(nrow(plotdata[[o]]))
      }
    }
  }
  
  #绘图
  #3Dpie图
  if(module=="threeD"){
    i=1
    for (i in 1:length(plotdata)) {
      if(order=="percentage"){
        color=color1[[i]]
      }else{
        if(nrow(plotdata[[i]]<10)){
          color<-color[1:nrow(plotdata[[i]])]
        }
      }
      filename=paste(names(plotdata)[i],"piechart",sep = "-")
      png(file = paste(filename,"png",sep="."),width = 10,height = 6,units="in",res=300)
      par(mar=c(par('mar')[1:3], 0))
      pie3D(x=plotdata[[i]]$number, 
            radius=1,#图的大小
            height=0.1,#饼图的高度 
            theta=pi/6,#图形的观测视角
            explode=0.05,#饼图间的间隙 
            main="PieChart",
            col=color ,#设置各部分颜色 
            border = color_expand, #分界线颜色
            labels=plotdata[[i]]$num_text,
            labelcex = 1,labelcol = "black"
      )
      
      legend(par('usr')[2]+par('usr')[2]/3, par('usr')[4]-par('usr')[4]/2, bty='n', xpd=NA,legend = plotdata[[i]]$class,col=color,pt.cex = 0.7,cex=0.5,text.col="black",pch=15)

      
      dev.off()
      pdf(file = paste(filename,"pdf",sep = "."),width = 10,height = 6)
      par(mar=c(par('mar')[1:3], 0))
      pie3D(x=plotdata[[i]]$number, 
            radius=1,#图的大小
            height=0.1,#饼图的高度 
            theta=pi/6,#图形的观测视角
            explode=0.05,#饼图间的间隙 
            main="PieChart",
            col=color ,#设置各部分颜色 
            border = color_expand,#分界线颜色,
            labels=plotdata[[i]]$num_text,
            labelcex = 1,labelcol = "black")
        
      legend(par('usr')[2]+par('usr')[2]/3, par('usr')[4]-par('usr')[4]/2, bty='n', xpd=NA,legend = plotdata[[i]]$class,col=color,pt.cex = 0.7,cex=0.5,text.col="black",pch=15)
      dev.off()
    }
  }
  else if(module=="normal"){
    #常规
    i=1
    for (i in 1:length(plotdata)) {
      filename=paste(names(plotdata)[i],"piechart",sep = "-")
      ymax<-0
      m=1
      p<-0
      for(m in 1:nrow(plotdata[[i]])){
        p<-plotdata[[i]]$number[m]
        ymax<-sum(ymax,p)
        plotdata[[i]]$ymax[m]<-ymax
        plotdata[[i]]$ymin[m]<-ymax-p
      }
      plotdata[[i]]$mid<-(plotdata[[i]]$ymax-plotdata[[i]]$ymin)/2+plotdata[[i]]$ymin
      if(order=="percentage"){
        color=color1[[i]]
      }else{
        if(nrow(plotdata[[i]]<10)){
          color<-color[1:nrow(plotdata[[i]])]
        }
      }
      p<-ggplot(plotdata[[i]], aes(1, plotdata[[i]]$number, fill= plotdata[[i]]$class),color=color_expand) + 
        geom_bar(aes(fill= factor(plotdata[[i]]$class,levels = plotdata[[i]]$class)),color=color_expand,stat = "identity",position=position_stack(reverse = TRUE),width = 1)+
        scale_fill_manual(values=color) + 
        labs(fill=names(plotdata)[i],title = "PieChart")+xlim(c(0.5,1.6))+ylim(c(0,sum(plotdata[[i]]$number)))+
        geom_text_repel(data = plotdata[[i]],aes(x=1.49,y=plotdata[[i]]$mid,label=plotdata[[i]]$num_text),direction="y",nudge_x = 0.1,nudge_y = 0.1,
                        arrow = arrow(length=unit(0.01, "npc")),segment.color="#cccccc")+
        coord_polar(theta = "y",start=0)+labs(x=NULL,y=NULL)+
        theme(panel.background = element_blank(),panel.grid.major = element_blank(),panel.border = element_blank(),
              axis.text = element_blank(),axis.ticks = element_blank())+
        theme(plot.title = element_text(hjust=0.5,color = 'black',face = 'bold',size = 20,vjust=0.5))
      if(nrow(plotdata[[i]])>20){
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.position="bottom",legend.text=element_text(size=4),
                   legend.key.size =unit(5,"pt") )+guides(fill = guide_legend( ncol = 4, byrow = TRUE))
      }else{
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.key.size =unit(10,"pt"),legend.text=element_text(size=8))+guides(fill = guide_legend( ncol = 1, byrow = TRUE))
      }
      
      ggsave(filename = paste(filename,"pdf",sep="."),plot = p,width=8,height = 6)
      ggsave(filename = paste(filename,"png",sep="."),plot = p,width=8,height = 6)
    }
  }
  else if(module=="circle"){
    #环状图
    i=1
    for(i in 1:length(plotdata)){
      if(order=="percentage"){
        color=color1[[i]]
      }else{
        if(nrow(plotdata[[i]]<10)){
          color<-color[1:nrow(plotdata[[i]])]
        }
      }
      filename=paste(names(plotdata)[i],"piechart",sep = "-")
      p<-ggplot(plotdata[[i]],aes(reorder(class,number),percentage,fill=class))+
        geom_bar(stat="identity",position="dodge",width = 0.7,colour = color_expand)+
        ylim(0,max(plotdata[[i]]$percentage)+0.05)+
        scale_fill_manual(values = color)+
        coord_polar(theta = "y")+xlab("")+ylab("")+
        labs(title = "Circle chart",fill = names(plotdata)[i])+
        theme_bw()+ 
        theme(plot.title = element_text(hjust = 0.5,vjust = 0.5))+
        theme(panel.background = element_blank())+
        theme(axis.text = element_blank(),axis.ticks = element_blank())
      if(nrow(plotdata[[i]])>20){
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.position="bottom",legend.text=element_text(size=4),
                   legend.key.size =unit(5,"pt") )+guides(fill = guide_legend( ncol = 4, byrow = TRUE))
      }else{
        p<-p+
          theme(legend.spacing.y = unit(0, 'cm'),legend.key.size =unit(10,"pt"),legend.text=element_text(size=8))+guides(fill = guide_legend( ncol = 1, byrow = TRUE))
      }
      
      
      ggsave(filename = paste(filename,"pdf",sep="."),plot = p,width=8,height = 6)
      ggsave(filename = paste(filename,"png",sep="."),plot = p,width=8,height = 6)
    }
  }
  else if(module=="rose"){
    #南丁格尔玫瑰图
    i=1
    for(i in 1:length(plotdata)){
      filename=paste(names(plotdata)[i],"piechart",sep = "-")
      if(order=="percentage"){
        color=color1[[i]]
      }else{
        if(nrow(plotdata[[i]]<10)){
          color<-color[1:nrow(plotdata[[i]])]
        }
      }
      p<-ggplot(plotdata[[i]],aes(reorder(class,number),percentage,fill=class))+
        geom_bar(stat = "identity", colour = color_expand,lwd = 1,width = 0.6)+
        scale_fill_manual(values=color)+
        coord_polar(start = 1)+ylim(c(0,max(plotdata[[i]]$percentage)))+
        xlab("")+ylab("")+labs(title = "Nightingale rose chart",fill = names(plotdata)[i])+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5,vjust = 0.5))+
        theme(panel.background = element_blank())+
        theme(axis.text=element_blank(),axis.ticks = element_blank())
      if(nrow(plotdata[[i]])>20){
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.position="bottom",legend.text=element_text(size=4),
                   legend.key.size =unit(5,"pt") )+guides(fill = guide_legend( ncol = 4, byrow = TRUE))
      }else{
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.key.size =unit(10,"pt"),legend.text=element_text(size=8))+guides(fill = guide_legend( ncol = 1, byrow = TRUE))
      }
      ggsave(filename = paste(filename,"pdf",sep="."),plot = p,width=8,height = 6)
      ggsave(filename = paste(filename,"png",sep="."),plot = p,width=8,height = 6)
    }
  }
  else if(module=="ring"){
    #戒指图
    i=1
    for(i in 1:length(plotdata)){
      filename=paste(names(plotdata)[i],"piechart",sep = "-")
      ymax<-0
      m=1
      p<-0
      for(m in 1:nrow(plotdata[[i]])){
        p<-plotdata[[i]]$number[m]
        ymax<-sum(ymax,p)
        plotdata[[i]]$ymax[m]<-ymax
        plotdata[[i]]$ymin[m]<-ymax-p
      }
      plotdata[[i]]$mid<-(plotdata[[i]]$ymax-plotdata[[i]]$ymin)/2+plotdata[[i]]$ymin
      if(order=="percentage"){
        color=color1[[i]]
      }else{
        if(nrow(plotdata[[i]]<10)){
          color<-color[1:nrow(plotdata[[i]])]
        }
      }
      p<-ggplot(plotdata[[i]], aes(1, plotdata[[i]]$number, fill= plotdata[[i]]$class),color=color_expand) + 
        geom_bar(aes(fill= factor(plotdata[[i]]$class,levels = plotdata[[i]]$class)),color=color_expand,stat = "identity",position=position_stack(reverse = TRUE),width = 0.3)+
        scale_fill_manual(values=color) + 
        labs(fill=names(plotdata)[i])+xlim(c(0.5,1.5))+ylim(c(0,sum(plotdata[[i]]$number)))+
        geom_text_repel(data = plotdata[[i]],aes(x=1.15,y=plotdata[[i]]$mid,label=plotdata[[i]]$num_text),direction="both",nudge_x = 0.1,nudge_y = 0.1,
                        arrow = arrow(length=unit(0.01, "npc")),segment.color="#cccccc")+
        coord_polar(theta = "y",start=0)+labs(x=NULL,y=NULL)+
        theme(panel.background = element_blank(),panel.grid.major = element_blank(),panel.border = element_blank(),
              axis.text = element_blank(),axis.ticks = element_blank())+
        theme(plot.title = element_text(hjust=0.5,color = 'black',face = 'bold',size = 20,vjust=0.5))+
        annotate("text",x=0.5,y=0,label="Ring plot",color="black",size=5)
      if(nrow(plotdata[[i]])>20){
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.position="bottom",legend.text=element_text(size=4),
                     legend.key.size =unit(5,"pt") )+guides(fill = guide_legend( ncol = 4, byrow = TRUE))
      }else{
        p<-p+theme(legend.spacing.y = unit(0, 'cm'),legend.key.size =unit(10,"pt"),legend.text=element_text(size=8))+guides(fill = guide_legend( ncol = 1, byrow = TRUE))
      }
      
      ggsave(filename = paste(filename,"pdf",sep="."),plot = p,width=8,height = 6)
      ggsave(filename = paste(filename,"png",sep="."),plot = p,width=8,height = 6)
    }
  }
}
