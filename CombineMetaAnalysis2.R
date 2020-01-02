### December 17 2019
### Create Singel File with MAtts updated Metaanalysis

###Load Libraries
library(dplyr)

###Load Data

MetaFiles <- list.files("~/projects/IndexTable2/Data/MetaAnalysis2/MetaAnalysis/")

Meta1 <- read.table(paste0("~/projects/IndexTable2/Data/MetaAnalysis2/MetaAnalysis/",MetaFiles[1]), sep="\t", header=TRUE, 
                colClasses=c(NA, NA, "NULL",
                             "NULL", NA, "NULL",
                             "NULL", "NULL", NA,
                             "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))
  names(Meta1) <- gsub("effectSize", paste0(MetaFiles[1],"_effectSize"),names(Meta1))
  names(Meta1) <- gsub("heterogeneity", paste0(MetaFiles[1],"_het"),names(Meta1))
  names(Meta1) <- gsub("_meta.tsv", "",names(Meta1))
  
  for (i in MetaFiles[2:length(MetaFiles)]){
    Meta2 <- read.table(paste0("~/projects/IndexTable2/Data/MetaAnalysis2/MetaAnalysis/",i), sep="\t", header=TRUE, 
                      colClasses=c(NA, NA, "NULL",
                                   "NULL", NA,  "NULL",
                                   "NULL", "NULL", NA,
                                   "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))
    names(Meta2) <- gsub("effectSize", paste0(i,"_effectSize"),names(Meta2))
    names(Meta2) <- gsub("heterogeneity", paste0(i,"_het"),names(Meta2))
    names(Meta2) <- gsub("_meta.tsv", "",names(Meta2))
    
    Meta1 <- merge(Meta1, Meta2, by="gene", all= TRUE)
}

head(Meta1)
PlotFunction(num = 6, meta = Meta1)

MinMeta1 <- apply(Meta1[,-1],1, min, na.rm=TRUE)
min(MinMeta1)
save(Meta1, file="./Data/MetaAnalysis2/Meta2_0.rds")
