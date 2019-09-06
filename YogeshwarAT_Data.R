load("./Data/Meta.Rdata")



####Add Yogeshwars Data
AT <- read.table("Data/20180918_Meta-analysis_distilled_fixedRandomResolved_transcriptomicsOnly.tsv", header=TRUE, sep="\t")

#Filter for only p val and fold change
index3 <- grep("Symbol$|.est|.adj.p", colnames(AT))
AT <- AT[,index3]
colnames(AT) <- gsub("LS", "AD_LS", colnames(AT))
colnames(AT) <- gsub("ANL", "AD_NL", colnames(AT))
colnames(AT) <- gsub(".est", "_effectSize", colnames(AT))
colnames(AT) <- gsub(".adj.p", "_effectSizeFDR", colnames(AT), fixed=TRUE)

#Merge AT with Meta data
Meta.1 <- merge(Meta, AT, by.x="gene", by.y="Symbol", all=TRUE)
Meta <- Meta.1[,c(1:35,42:47,36:41)]

##Round values
index <- grep("FDR", names(Meta))
Meta[,index]<- signif(Meta[index],2)
index <- grep("effectSize$|FC", names(Meta))
Meta[,index]<- round(Meta[index],2)

save(Meta, file="Data/Metadev.Rdata")
write.table(Meta, file = "www/MetadevAnalysis.txt", sep="\t")
