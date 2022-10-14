Sys.setenv(lang = "en_US")

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("ChemmineR")

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("fmcsR") 

library("ChemmineR") # Loads the package
library("fmcsR")
library("ggplot2")

#Import library
sdfset <- read.SDFset("3D_AOP30stressors.sdf")

valid <- validSDF(sdfset) # Identifies invalid SDFs in SDFset objects 
sdfset <- sdfset[valid] # Removes invalid SDFs, if there are any 

length(sdfset) # Returns number of molecules

plot(sdfset[1:6])

# Create an atom pair descriptor database

apset <- sdf2ap(sdfset)

#Remove AP duplicates
apdups <- cmp.duplicated(apset, type=1)
sdfset[which(!apdups)]
apset1 <- apset[which(!apdups)]
sdfset1 <- sdfset[which(!apdups)]

q <- grepSDFset("CMP3107", sdfset1, field="datablock", mode="subset") 

#Information on deduped sdfset and identification of "parent" compounds used for queries

datablock <- datablock(sdfset1)

##Save containers as binary R objects
save(sdfset1, file = "sdfset.rda", compress = TRUE)
save(apset1, file = "apset.rda", compress = TRUE)

load("sdfset.rda")
load("apset.rda")

write.SDF(sdfset1, file="3D_AOPs30stressors_dedup.sdf", sig=TRUE)

write.csv(sdfset1,"3D_AOPs30stressors_dedup.csv", row.names = FALSE)

## Molecular property functions
# Atom Frequency
# For clustering but not for de-duplicating
propma <- atomcountMA(sdfset, addH=FALSE) 
boxplot(propma, col="blue", main="Atom Frequency")

boxplot(rowSums(propma), main="All Atom Frequency")

#generate data frame with molecular information
propma <- data.frame(MF=MF(sdfset, addH=FALSE), MW=MW(sdfset, addH=FALSE),
                     Ncharges=sapply(bonds(sdfset, type="charge"), length),
                     atomcountMA(sdfset, addH=FALSE), 
                     groups(sdfset, type="countMA"), 
                     rings(sdfset, upper=6, type="count", arom=TRUE))


#Creating an FPset container (fingerprints)
fpset <- desc2fp(apset1)

##Clustering

#Binning clustering of apset object
#Force save a distance matrix in the directory
clusters <- cmp.cluster(db=apset1, cutoff = c(0.7, 0.8, 0.9), save.distances="distmat.rda")

save(clusters, file = "clusters.rda", compress = TRUE)

load("clusters.rda")

#Clustering information retrieval
cluster.df <- as.data.frame(clusters)

cluster_stat <- cluster.sizestat(clusters, cluster.result = 2)

#Visualization, Multi dimensional scaling
cluster.visualize(apset1, clusters, size.cutoff = 20, highlight.compounds = c(3324, 3474, 3877, 1, 3054, 4874, 384, 4235, 2216, 2217, 4598, 1268), cluster.result = 1) # Color codes clusters with at least XX members (depending on chosen cut off value, highlights "parent" compounds)

load("distmat.rda")

hc <- hclust(as.dist(distmat), method="single") 
hc[["labels"]] <- cid(apset) # Assign correct item labels 
plot(as.dendrogram(hc), edgePar=list(col=4, lwd=2), horiz=T)

library(gplots) 
heatmap.2(1-distmat, Rowv=as.dendrogram(hc), Colv=as.dendrogram(hc), 
          col=colorpanel(40, "darkblue", "yellow", "white"), 
          density.info="none", trace="none")


