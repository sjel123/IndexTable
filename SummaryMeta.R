
#Load Library files
library(tmod)

# Load Data

Meta <- read.csv("./Data/MetaAnalysis.csv", stringsAsFactors = F)
Anno <- read.csv("./Data/DatabaseExtract_v_1.01.csv", stringsAsFactors = F)
Anno <- Anno[,c(2,4)]
  Anno <- Anno[Anno$Is.TF.=="Yes",]

msig <- tmodImportMSigDB("/app/PathwayAnalysis/Data/msigdb_v6.0.xml") 

###Calculate FC and Pvale
index1 <- grep("Size$", names(Meta))
index2 <- grep("FDR", names(Meta))

  Meta$FC <- apply(Meta[,index1], 1, function(x) max(abs(x), na.rm = T))
  Meta$Pval <- apply(Meta[,index2], 1, function(x) min(abs(x), na.rm = T))

  #Add Annotations  
msig$MODULES[grep("innate", msig$MODULES$Title),] #M13213
msig$MODULES[grep("adaptive", msig$MODULES$Title),] #M1058 M13847
msig$MODULES[grep("tissue", msig$MODULES$Title),] #M14192

innate <- msig$MODULES2GENES$M13213
adaptive <- unique(c(msig$MODULES2GENES$M1058, msig$MODULES2GENES$M13847))
fibrosis <- msig$MODULES2GENES$M14192

#Filter for TF
Meta$TF <- (Meta$gene%in%Anno$HGNC.symbol)
  Meta$innate <- (Meta$gene%in%innate)
  Meta$adaptive <- (Meta$gene%in%adaptive)
  Meta$fibrosis <- (Meta$gene%in%fibrosis)

Meta.tf <- Meta[Meta$TF=="TRUE"&(Meta$innate=="TRUE"|Meta$adaptive=="TRUE"|Meta$fibrosis=="TRUE"),]

Meta.tf.fil <- Meta.tf[Meta.tf$Pval <0.001& Meta.tf$FC >1.5,]

save(Meta, file="./Data/Meta.Rdata")
