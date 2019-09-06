

PlotFunction <- function(num=1, meta=Meta){
  require(ggplot2)
  require(cowplot)
  require(ggrepel)
  DF <- meta[num,]
  DF1 <- data.frame()
  index1 <- grep("Size$", names(Meta))
  index2 <- grep("FDR", names(Meta))
    FC= DF[index1]
    pval = DF[index2]
  DF1 <- as.data.frame(cbind(as.numeric(FC), as.numeric(pval)))
  row.names(DF1) <- colnames(FC)
    row.names(DF1) <- gsub("_effectSize", "", row.names(DF1))
  colnames(DF1) <- c("FC", "pVal")
    DF1$label <- rownames(DF1)
   temp1 <- strsplit(DF1$label,split = "_",fixed = TRUE)
    DF1$Disease <- sapply(temp1, "[",1)
   g <- ggplot(DF1, aes(x=FC, y=-log10(pVal), label = label, color=Disease))+geom_point()+geom_text_repel()
    g <- g + geom_hline(yintercept = 1.3, linetype="dashed") 
    g <- g + geom_vline(xintercept = 0)
    g <- g + ggtitle(Meta$gene[num])
    g
  }