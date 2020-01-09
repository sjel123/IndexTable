

PlotFunction <- function(num=1, meta=Meta){
  require(ggplot2)
  require(cowplot)
  require(ggrepel)
  DF <- meta[num,]
  DF1 <- data.frame()
  index1 <- grep("Size$|Size.[xy]", names(meta))
  index2 <- grep("FDR", names(meta))
    FC= DF[index1]
    pval = DF[index2]
  DF1 <- as.data.frame(cbind(as.numeric(FC), as.numeric(pval)))
  #DF1$Tissue <- DF1$label
  DF1$Tissue <- names(DF[index1])
    DF1$Tissue <- gsub("_effectSize", "", DF1$Tissue)
    DF1$label<-DF1$Tissue
    DF1$Disease <- DF1$Tissue
    DF1$Tissue[grep('blood', DF1$label)]="Blood"
    DF1$Tissue[grep('blood', DF1$label,invert = T)]="Tissue"
    
    row.names(DF1) <- colnames(FC)
    row.names(DF1) <- gsub("_effectSize", "", row.names(DF1))
  colnames(DF1) <- c("FC", "pVal", "Tissue", "Label", "Disease")
    DF1$label <- rownames(DF1)
   temp1 <- strsplit(DF1$label,split = "_",fixed = TRUE)
    DF1$Disease <- sapply(temp1, "[",1)
   
    DF1$Label2 <- gsub("_colon|_blood|_synovial|_skin|_liver", "",DF1$label)
    # All labels should be to the right of 3.
    x_limits <- c( NA,min(DF1$FC, na.rm = TRUE)+1)
   g <- ggplot(DF1, aes(x=FC, y=-log10(pVal), label = Label2, size=4, shape=Tissue))+
     geom_point(aes(color=Disease,size=4))+
     xlim(min(DF1$FC)-1, NA) +
     geom_text_repel(aes(color=Disease),
       arrow = arrow(length = unit(0.02, "npc"), type = "open", ends = "last"),
       force = 3,
       direction = "y",
       #xlim=x_limits,
       nudge_x  = min(DF1$FC, na.rm = T)-1,
       hjust = 0

     )
    g <- g + geom_hline(yintercept = 1.3, linetype="dashed") 
    g <- g + geom_vline(xintercept = 0)
    g <- g + ggtitle(meta$gene[num]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g <- g + scale_size(guide=FALSE) + theme(legend.text = element_text(colour="blue", size=16)) +
      guides(shape = guide_legend(override.aes = list(size = 5)))+
      guides(color = guide_legend(override.aes = list(size = 5)))
    g <- g+facet_wrap(~Disease,nrow = 3)
    #+ theme(legend.key.size = unit(3,"line")
     DF1 <- DF1[order(row.names(DF1)),]
     DF1$Signif <- ifelse(DF1$pVal>0.001|is.na(DF1$pVal),"FALSE","TRUE") #FDR Cutoff
     DF1$Bold   <- ifelse(DF1$Signif, "bold", "plain")
     DF2 <- DF1[c(-1,-2,-3),]
     #Eliminate Matt AD analysis from figure
    gg <- ggplot(DF2, aes(x=FC, y=label, size= -log10(pVal), color=Signif))+geom_point()+ggtitle(meta[num,1])
    gg <- gg + geom_vline(xintercept = 0, color="red", alpha = 0.4, linetype="dashed")
    gg <- gg  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    gg <- gg  + theme(legend.text = element_text(colour="blue", size=16),
                      panel.border = element_rect(color = "black", fill=NA, size=1, linetype = 1, inherit.blank = FALSE)) 
    gg <- gg + guides(color=FALSE)
    gg <- gg +  scale_colour_manual(values = c("blue", "red"))
    gg <- gg + theme(axis.text.y = element_text(face = DF2$Bold))
    gg <- gg + geom_hline(yintercept = c(3.5, 5.5,8.5, 12.5, 14.5, 15.5 ), linetype="dashed", alpha = 0.2)
    gg 

   
        my_list=list("g"=g, "gg"=gg)
       return(my_list)                                                            
}



VolcanoPlot <- function(Data=Data1,  Disease=Disease){
  require(plotly)
  colnames(Data) <- c("Gene", "FC", "PVal")
  p <- ggplot(Data, aes(x=FC, y=PVal, label=Gene)) + geom_point() +ggtitle(Disease) +
    theme(legend.position="none")
  p <- ggplotly(p)
  p
  
}

IndPlotFunc <- function(Gene="KRT16", Data2=Data2, Meta=Meta){ 
  require(reshape2)
  require(ggplot2)
  require(cowplot)
  require(plotly)
  A <-  Data2[Data2$Symbol==Gene, grep("FDR", names(Data2))]
  B <-  Data2[Data2$Symbol==Gene, grep("logFC", names(Data2))] 
  C <-  Data2[Data2$Symbol==Gene, grep("AveExpr", names(Data2))] 
  D <-  Data2[Data2$Symbol==Gene, grep(".SE", names(Data2), fixed=TRUE)] 
  E <-  Meta[Meta$gene==Gene, grep("ADy", names(Meta))]
  DF <- cbind(t(A),t(B),t(C),t(D))
  colnames(DF) <- c("FDR", "logFC", "AveExpr", "SE")
  DF <- as.data.frame(DF)
  Anno <- as.data.frame(t(as.data.frame((strsplit(row.names(DF), split = "\\.")))))
  DF$Study <- Anno$V1
  DF$Contrast <- Anno$V2
  
  E <- data.frame(matrix(E, nrow = 3, byrow = TRUE)) 
  E$X1 <- unlist(E$X1)
  E$X2 <- unlist(E$X2)
  names(E)<- c("logFC", "FDR")
  row.names(E) <- names(Meta)[grep("ADy", names(Meta))][c(1,3,5)]
  row.names(E) <- gsub("effectSize", "",row.names(E))
  E$Contrast <- gsub("_Meta|ADy_", "", row.names(E))
  E$Contrast <- gsub("NL", "ANL", E$Contrast)
  E$Contrast <- gsub("_$", "", E$Contrast)
  E$AveExpr <- max(DF$AveExpr)
  E$Study <- "Meta"
  
  p <- ggplot(DF, aes(x=logFC, y=-log10(FDR), color=Contrast, size=AveExpr, label=Study))+geom_point() +
    geom_point(data=E, aes(x=logFC, y=-log10(FDR)),size=10, shape=18) +ggtitle(Gene)
  p <- ggplotly(p)
  print(p)
  return(p)
}

# # pp <- PlotFunction(23956)
# # ggsave (pp, height=7.5, width=7.5, filename="test.jpg")
# DF1 <- DF1[order(row.names(DF1)),]
# DF1$Signif <- DF1$pVal<0.001
# DF1$Bold <- ifelse(DF1$Signif, "bold", "plain")
# 
# gg <- ggplot(DF1, aes(x=FC, y=label, size= -log10(pVal), color=Signif))+geom_point()+ggtitle(Gene)
# gg <- gg + geom_vline(xintercept = 0, linetype="dashed")
# gg <- gg + ggtitle(meta$gene[num]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# gg <- gg  + theme(legend.text = element_text(colour="blue", size=16)) 
# gg <- gg + guides(color=FALSE)
# gg <- gg +  scale_colour_manual(values = c("blue", "red"))
# gg + theme(axis.text.y = element_text(face = DF1$Bold))
# gg


####
Gene="FAP"

PlotFunctionSingle <- function(Gene="FAP"){
  num = row.names(meta)[which(meta$gene==Gene)]
  
  PlotFunction(num = num)[[2]]
  
  png(paste0(Gene,".png"), width=1000, height=600, res=120)
    print(PlotFunction(num = num)[[2]])
  dev.off() 
}


CreateGraphs <- function(){
for(i in c("SIK1", "SIK2", "SIK3",
           "PFKFB3", "ITK", "TMEM173", 
           "FAP", "RASGRP1", "IRAK1",
           "IRAK4","VAV1")){
  print(i)

PlotFunctionSingle(Gene=i)
}
}         



PlotFunction2 <- function(num=1, meta=Meta){
  require(ggplot2)
  require(cowplot)
  require(ggrepel)
  DF <- meta[num,]
  DF1 <- data.frame()
  index1 <- grep("Size$|Size.[xy]", names(meta))
  index2 <- grep("FDR", names(meta))
  index3 <- grep("Pval", names(meta))
  FC= DF[index1]
  pval = DF[index2]
  het = DF[index3]
  DF1 <- as.data.frame(cbind(as.numeric(FC), as.numeric(pval), as.numeric(het)))
  #DF1$Tissue <- DF1$label
  DF1$Tissue <- names(DF[index1])
  DF1$Tissue <- gsub("_effectSize", "", DF1$Tissue)
  DF1$label<-DF1$Tissue
  DF1$Disease <- DF1$Tissue
  DF1$Tissue[grep('blood', DF1$label)]="Blood"
  DF1$Tissue[grep('blood', DF1$label,invert = T)]="Tissue"
  
  row.names(DF1) <- colnames(FC)
  row.names(DF1) <- gsub("_effectSize", "", row.names(DF1))
  colnames(DF1) <- c("FC", "pVal", "Hetp", "Tissue", "Label", "Disease")
  DF1$label <- rownames(DF1)
  temp1 <- strsplit(DF1$label,split = "_",fixed = TRUE)
  DF1$Disease <- sapply(temp1, "[",1)
  
  DF1$Label2 <- gsub("_colon|_blood|_synovial|_skin|_liver", "",DF1$label)
  # All labels should be to the right of 3.
  x_limits <- c( NA,min(DF1$FC, na.rm = TRUE)+1)
  g <- ggplot(DF1, aes(x=FC, y=-log10(pVal), label = Label2, size=4, shape=Tissue))+
    geom_point(aes(color=Disease,size=4))+
    xlim(min(DF1$FC)-1, NA) +
    geom_text_repel(aes(color=Disease),
                    arrow = arrow(length = unit(0.02, "npc"), type = "open", ends = "last"),
                    force = 3,
                    direction = "y",
                    #xlim=x_limits,
                    nudge_x  = min(DF1$FC, na.rm = T)-1,
                    hjust = 0
                    
    )
  g <- g + geom_hline(yintercept = 1.3, linetype="dashed") 
  g <- g + geom_vline(xintercept = 0)
  g <- g + ggtitle(meta$gene[num]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g <- g + scale_size(guide=FALSE) + theme(legend.text = element_text(colour="blue", size=16)) +
    guides(shape = guide_legend(override.aes = list(size = 5)))+
    guides(color = guide_legend(override.aes = list(size = 5)))
  g <- g+facet_wrap(~Disease,nrow = 3)
  #+ theme(legend.key.size = unit(3,"line")
  DF1 <- DF1[order(row.names(DF1)),]
  DF1$Signif <- ifelse(DF1$pVal>0.001|is.na(DF1$pVal),"black", "red")
  DF1$SignHet <- ifelse(DF1$Hetp>0.001|is.na(DF1$Hetp),"FALSE", "TRUE")
  DF1$Bold   <- ifelse(DF1$Signif=="red", "bold", "plain")
  #DF2 <- DF1[c(-1,-2,-3),]
  DF2=DF1
  #Eliminate Matt AD analysis from figure
  gg <- ggplot(DF2, aes(x=FC, y=label, size= -log10(pVal),color=Signif, shape=SignHet))+geom_point(  )+ggtitle(meta[num,1])
  
  gg 
  gg <- gg + geom_vline(xintercept = 0, color="red", alpha = 0.4, linetype="dashed")
  gg <- gg  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  gg <- gg  + theme(legend.text = element_text(colour="blue", size=16),
                    panel.border = element_rect(color = "black", fill=NA, size=1, linetype = 1, inherit.blank = FALSE)) 
  gg <- gg + guides(color=FALSE)
  gg <- gg +  scale_colour_manual(values = c("blue", "red"))
  gg <- gg + theme(axis.text.y = element_text(face = DF2$Bold))
  gg <- gg + geom_hline(yintercept = c(c(0.5, 2.5, 6.5,8.5, 11.5, 13.5, 15.5, 16.5, 18.5, 21.5, 24.5, 25.5, 26.5)+3 ), linetype="dashed", alpha = 0.2)
  gg 
  
  
  my_list=list("g"=g, "gg"=gg)
  return(my_list)                                                            
}
#PlotFunction2(num=6, meta=Meta1)

