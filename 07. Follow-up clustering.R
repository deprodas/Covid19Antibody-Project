setwd("E:\\8) Follow Up (Heatmap) - Review") 

library("readxl") 
library(dplyr)
library(tidyverse) 
library("RColorBrewer")
library(viridis) 
library(rcartocolor)
library(pheatmap) 

#===================== COLOR PALETTE =================================================================================

# Rcartocolor 
sunset <- rcartocolor::carto_pal(7, "ag_Sunset") 
ramp_sunset <- colorRampPalette(sunset)(300)

# RColorBrewer
brew1 <- RColorBrewer::brewer.pal(7, "YlGnBu")
ramp_brew1 <- colorRampPalette(brewer.pal(7, "YlGnBu"))(300) 

brew2 <- RColorBrewer::brewer.pal(7, "RdPu")
ramp_brew2 <- colorRampPalette(brewer.pal(7, "RdPu"))(300) 

# Viridis
plasma <- viridis::plasma(7) 
ramp_plasma <- colorRampPalette(plasma)(300) 


#===================== Data sets input ===============================================================================

# Main Data 

dia_main <- read_excel("Diabetes_FINAL.xlsx") 
kid_main <- read_excel("Kidney_FINAL.xlsx") 

# Diabetes data 

dia_fil <- dia_main %>% column_to_rownames(var = "SL") %>% 
  select(c("IgG_level1", "TotalAntibody1", "IgG_level2", "TotalAntibody2", "IgG_level3", "TotalAntibody3")) %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

dia_IgG <- dia_fil %>% select(c("IgG_level1", "IgG_level2", "IgG_level3")) 
dia_tot <- dia_fil %>% select(c("TotalAntibody1", "TotalAntibody2", "TotalAntibody3"))  


# Kidney Disease data 

kid_fil <- kid_main %>% column_to_rownames(var = "SL") %>% 
  select(c("IgG_level1", "TotalAntibody1", "IgG_level2", "TotalAntibody2", "IgG_level3", "TotalAntibody3")) %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

kid_IgG <- kid_fil %>% select(c("IgG_level1", "IgG_level2", "IgG_level3")) 
kid_tot <- kid_fil %>% select(c("TotalAntibody1", "TotalAntibody2", "TotalAntibody3"))  



#===================== Z-score calculations ========================================================================== 

cal_z_score <- function(x){
  (x - mean(x)) / sd(x)
}

dia_ZIgG <- t(apply(dia_IgG, 1, cal_z_score))
dia_Ztot <- t(apply(dia_tot, 1, cal_z_score))
kid_ZIgG <- t(apply(kid_IgG, 1, cal_z_score))
kid_Ztot <- t(apply(kid_tot, 1, cal_z_score))


#===================== Diabetes (IgG) ================================================================================

library(dendextend) 

# Hierarchical Clustering 

hclust1 <- hclust(dist(dia_ZIgG), method = "complete") 
diaIgG_col <- cutree(tree = as.dendrogram(hclust1), k = 3)

# Annotation row 

diaIgG_col <- as.data.frame(diaIgG_col) %>% rename(Clusters = diaIgG_col) %>% 
  mutate(Clusters = ifelse(Clusters == 1, "Cluster-1", 
                    ifelse(Clusters == 2, "Cluster-2", "Cluster-3"))) 
# Merge 

diaIgG_heat <- merge(dia_ZIgG, diaIgG_col, by = "row.names", all = TRUE) %>% column_to_rownames(var = "Row.names") 

# Heatmap 

set.seed(1234) 

Dia_IgG <- pheatmap(diaIgG_heat[ , 1:3], 
                    cutree_rows = 3, 
                    annotation_row = diaIgG_col,
                    color = ramp_plasma, 
                    border_color = "white", 
                    clustering_distance_cols = "euclidean", 
                    clustering_distance_rows = "euclidean") 
Dia_IgG 

ggsave("Diabetes_IgG.pdf", plot= Dia_IgG, width = 4, height = 8, units = "in")


#===================== Diabetes (Total) ==============================================================================

# Hierarchical Clustering 

hclust2 <- hclust(dist(dia_Ztot), method = "complete") 
diatot_col <- cutree(tree = as.dendrogram(hclust2), k = 3)

# Annotation row 

diatot_col <- as.data.frame(diatot_col) %>% rename(Clusters = diatot_col) %>% 
  mutate(Clusters = ifelse(Clusters == 1, "Cluster-1", 
                    ifelse(Clusters == 2, "Cluster-2", "Cluster-3"))) 
# Merge 

diatot_heat <- merge(dia_Ztot, diatot_col, by = "row.names", all = TRUE) %>% column_to_rownames(var = "Row.names") 

# Heatmap 

set.seed(1234) 

Dia_tot <- pheatmap(diatot_heat[ , 1:3], 
                    cutree_rows = 3, 
                    annotation_row = diatot_col,
                    color = ramp_plasma, 
                    border_color = "white", 
                    clustering_distance_cols = "euclidean", 
                    clustering_distance_rows = "euclidean") 
Dia_tot 

ggsave("Diabetes_total.pdf", plot= Dia_tot, width = 4, height = 8, units = "in")


#===================== Kidney Disease (IgG) ==========================================================================


# Clustering method: 'ward', 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid' 

# Hierarchical Clustering 

hclust3 <- hclust(dist(kid_ZIgG), method = "complete") 
kidIgG_col <- cutree(tree = as.dendrogram(hclust3), k = 3)

# Annotation row 

kidIgG_col <- as.data.frame(kidIgG_col) %>% rename(Clusters = kidIgG_col) %>% 
  mutate(Clusters = ifelse(Clusters == 1, "Cluster-1", 
                    ifelse(Clusters == 2, "Cluster-2", "Cluster-3"))) 
# Merge 

kidIgG_heat <- merge(kid_ZIgG, kidIgG_col, by = "row.names", all = TRUE) %>% column_to_rownames(var = "Row.names") 

# Heatmap 

set.seed(1234) 

Kid_IgG <- pheatmap(kidIgG_heat[ , 1:3], 
                    cutree_rows = 3, 
                    annotation_row = kidIgG_col,
                    color = ramp_plasma, 
                    border_color = "white", 
                    clustering_distance_cols = "euclidean", 
                    clustering_distance_rows = "euclidean") 
Kid_IgG 

ggsave("Kidney_IgG.pdf", plot = Kid_IgG, width = 4, height = 8, units = "in")


#===================== Kidney Disease (Total) ========================================================================

# Hierarchical Clustering 

hclust4 <- hclust(dist(kid_Ztot), method = "complete") 
kidtot_col <- cutree(tree = as.dendrogram(hclust4), k = 3)

# Annotation row 

kidtot_col <- as.data.frame(kidtot_col) %>% rename(Clusters = kidtot_col) %>% 
  mutate(Clusters = ifelse(Clusters == 1, "Cluster-1", 
                    ifelse(Clusters == 2, "Cluster-2", "Cluster-3"))) 
# Merge 

kidtot_heat <- merge(kid_Ztot, kidtot_col, by = "row.names", all = TRUE) %>% column_to_rownames(var = "Row.names") 

# Heatmap 

set.seed(1234) 

Kid_tot <- pheatmap(kidtot_heat[ , 1:3], 
                    cutree_rows = 3, 
                    annotation_row = kidtot_col,
                    color = ramp_plasma, 
                    border_color = "white", 
                    clustering_distance_cols = "euclidean", 
                    clustering_distance_rows = "euclidean") 
Kid_tot

ggsave("Kidney_total.pdf", plot = Kid_tot, width = 4, height = 8, units = "in")


#===================== Merged Data Export ============================================================================ 


# Main data
dia_main
kid_main

# Status data 
dia_status <- read.csv("Diabetes_Status.csv")
Kid_status <- read.csv("Kidney_Status.csv")


diaIgG_heat <- diaIgG_heat %>% rename(IgG_z1 = IgG_level1, 
                                      IgG_z2 = IgG_level2, 
                                      IgG_z3 = IgG_level3) %>% rownames_to_column(var = "SL") 

diaIgG_merge <- merge(x = dia_main, y = diaIgG_heat, by = "SL", all.y = TRUE) 
diaIgG_heat <- merge(x = dia_status, y = diaIgG_heat, by = "SL", all.y = TRUE) 
write.csv(diaIgG_merge, file = "Diabetes_IgG Cluster_Details_SC.csv", row.names = FALSE)
write.csv(diaIgG_heat, file = "Diabetes_IgG.csv", row.names = FALSE)


diatot_heat <- diatot_heat %>% rename(TotalAb_z1 = TotalAntibody1, 
                                      TotalAb_z2 = TotalAntibody2, 
                                      TotalAb_z3 = TotalAntibody3) %>% rownames_to_column(var = "SL") 

diatot_merge <- merge(x = dia_main, y = diatot_heat, by = "SL", all.y = TRUE) 
diatot_heat <- merge(x = dia_status, y = diatot_heat, by = "SL", all.y = TRUE) 
write.csv(diatot_merge, file = "Diabetes_totalAb Cluster_Details_SC.csv", row.names = FALSE)
write.csv(diatot_heat, file = "Diabetes_totalAb.csv", row.names = FALSE)


kidIgG_heat <- kidIgG_heat %>% rename(IgG_z1 = IgG_level1, 
                                      IgG_z2 = IgG_level2, 
                                      IgG_z3 = IgG_level3) %>% rownames_to_column(var = "SL") 

kidIgG_merge <- merge(x = kid_main, y = kidIgG_heat, by = "SL", all.y = TRUE)
kidIgG_heat <- merge(x = Kid_status, y = kidIgG_heat, by = "SL", all.y = TRUE) 
write.csv(kidIgG_merge, file = "Kidney_IgG Cluster_Details_SC.csv", row.names = FALSE)
write.csv(kidIgG_heat, file = "Kidney_IgG.csv", row.names = FALSE)


kidtot_heat <- kidtot_heat %>% rename(TotalAb_z1 = TotalAntibody1, 
                                      TotalAb_z2 = TotalAntibody2, 
                                      TotalAb_z3 = TotalAntibody3) %>% rownames_to_column(var = "SL") 

kidtot_merge <- merge(x = kid_main, y = kidtot_heat, by = "SL", all.y = TRUE)
kidtot_heat <- merge(x = Kid_status, y = kidtot_heat, by = "SL", all.y = TRUE) 
write.csv(kidtot_merge, file = "Kidney_totalAb Cluster_Details_SC.csv", row.names = FALSE)
write.csv(kidtot_heat, file = "Kidney_totalAb.csv", row.names = FALSE)
