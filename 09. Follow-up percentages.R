setwd("E:\\Mousumi Ma'am (Project-1)\\9) Dia _Kid SWAP - Review") 

library(dplyr)
library(tidyverse) 
library(ggrepel)
library(ggplot2)
library(RColorBrewer) 

# Data Import 

pieD_IgG <- read.csv("Diabetes_IgG Cluster_Details_SC.csv") %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

# write.csv(pieD_IgG, file = "Diabetes_IgG Cluster_Details_SC.csv")  

# Data Import 

pieD_tot <- read.csv("Diabetes_totalAb Cluster_Details_SC.csv") %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

# write.csv(pieD_tot, file = "Diabetes_totalAb Cluster_Details_SC.csv")  

# Data Import 

pieK_IgG <- read.csv("Kidney_IgG Cluster_Details_SC.csv") %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

# write.csv(pieK_IgG, file = "Kidney_IgG Cluster_Details_SC.csv")  

# Data Import 

pieK_tot <- read.csv("Kidney_totalAb Cluster_Details_SC.csv") %>% 
  filter(IgG_level1 >= 1000 & IgG_level1 <= 140000 & TotalAntibody1 >= 1000) %>% 
  filter(IgG_level2 >= 1000 & IgG_level2 <= 140000 & TotalAntibody2 >= 1000) %>% 
  filter(IgG_level3 >= 1000 & IgG_level3 <= 140000 & TotalAntibody3 >= 1000) 

# write.csv(pieK_tot, file = "Kidney_totalAb Cluster_Details_SC.csv")  


#===================== IgG Diabetes =========================================================================== 

# Cluster 1 (30) IgG 

# Percentage - Cluster 1 (30) IgG 
pieD_IgG_clus1 <- pieD_IgG %>% 
  filter(Cluster == "Cluster-1") %>%  
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_IgG_clus1 <- ggplot(pieD_IgG_clus1, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) + 
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_IgG_clus1,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_IgG_clus1
ggsave("Dia_IgG_clus1 (pie).pdf", G_pieD_IgG_clus1, width = 4, height = 4, units = "in") 


# Cluster 2 (28) IgG

# Percentage - Cluster 2 (28) IgG  
pieD_IgG_clus2 <- pieD_IgG %>% 
  filter(Cluster == "Cluster-2") %>% 
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_IgG_clus2 <- ggplot(pieD_IgG_clus2, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_IgG_clus2,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_IgG_clus2
ggsave("Dia_IgG_clus2 (pie).pdf", G_pieD_IgG_clus2, width = 4, height = 4, units = "in") 


# Cluster 3 (29) IgG 

# Percentage - Cluster 3 (29) IgG 
pieD_IgG_clus3 <- pieD_IgG %>% 
  filter(Cluster == "Cluster-3") %>% 
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_IgG_clus3 <- ggplot(pieD_IgG_clus3, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_IgG_clus3,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_IgG_clus3
ggsave("Dia_IgG_clus3 (pie).pdf", G_pieD_IgG_clus3, width = 4, height = 4, units = "in") 


#===================== Total Ab Diabetes  ===================================================================== 

# Cluster 1 (30) Total Ab

# Percentage - Cluster 1 (30) Total Ab 
pieD_tot_clus1 <- pieD_tot %>% 
  filter(Cluster == "Cluster-1") %>% 
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_tot_clus1 <- ggplot(pieD_tot_clus1, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_tot_clus1,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_tot_clus1
ggsave("Dia_tot_clus1 (pie).pdf", G_pieD_tot_clus1, width = 4, height = 4, units = "in") 


# Cluster 2 (29) Total Ab 

# Percentage - Cluster 2 (29) Total Ab 
pieD_tot_clus2 <- pieD_tot %>% 
  filter(Cluster == "Cluster-2") %>% 
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_tot_clus2 <- ggplot(pieD_tot_clus2, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_tot_clus2,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_tot_clus2
ggsave("Dia_tot_clus2 (pie).pdf", G_pieD_tot_clus2, width = 4, height = 4, units = "in") 


# Cluster 3 (28) Total Ab 

# Percentage - Cluster 3 (28) Total Ab  
pieD_tot_clus3 <- pieD_tot %>% 
  filter(Cluster == "Cluster-3") %>%  
  count(Kidney_Disease) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieD_tot_clus3 <- ggplot(pieD_tot_clus3, aes(x = "" , y = Percentage, fill = fct_inorder(Kidney_Disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4DB8C4", "#FFD32D")) + 
  geom_label_repel(data = pieD_tot_clus3,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieD_tot_clus3 
ggsave("Dia_tot_clus3 (pie).pdf", G_pieD_tot_clus3, width = 4, height = 4, units = "in") 


#===================== IgG Kidney Disease ===================================================================== 

# Cluster 1 (36) IgG 

# Percentage - Cluster 1 (36) IgG  
pieK_IgG_clus1 <- pieK_IgG %>% 
  filter(Cluster == "Cluster-1") %>% 
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_IgG_clus1 <- ggplot(pieK_IgG_clus1, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_IgG_clus1,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_IgG_clus1
ggsave("Kid_IgG_clus1 (pie).pdf", G_pieK_IgG_clus1, width = 4, height = 4, units = "in") 


# Cluster 2 (34) IgG 

# Percentage - Cluster 2 (34) IgG 
pieK_IgG_clus2 <- pieK_IgG %>% 
  filter(Cluster == "Cluster-2") %>% 
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_IgG_clus2 <- ggplot(pieK_IgG_clus2, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_IgG_clus2,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_IgG_clus2
ggsave("Kid_IgG_clus2 (pie).pdf", G_pieK_IgG_clus2, width = 4, height = 4, units = "in") 


# Cluster 3 (33) IgG 

# Percentage - Cluster 3 (33) IgG 
pieK_IgG_clus3 <- pieK_IgG %>% 
  filter(Cluster == "Cluster-3") %>%  
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_IgG_clus3 <- ggplot(pieK_IgG_clus3, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_IgG_clus3,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_IgG_clus3 
ggsave("Kid_IgG_clus3 (pie).pdf", G_pieK_IgG_clus3, width = 4, height = 4, units = "in") 


#===================== Total Ab Kidney Disease ================================================================  

# Cluster 1 (33) Total Ab

# Percentage - Cluster 1 (33) Total Ab 
pieK_tot_clus1 <- pieK_tot %>% 
  filter(Cluster == "Cluster-1") %>% 
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_tot_clus1 <- ggplot(pieK_tot_clus1, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_tot_clus1,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_tot_clus1
ggsave("Kid_tot_clus1 (pie).pdf", G_pieK_tot_clus1, width = 4, height = 4, units = "in") 


# Cluster 2 (29) Total Ab 

# Percentage - Cluster 2 (29) Total Ab 
pieK_tot_clus2 <- pieK_tot %>% 
  filter(Cluster == "Cluster-2") %>%  
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_tot_clus2 <- ggplot(pieK_tot_clus2, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_tot_clus2,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_tot_clus2 
ggsave("Kid_tot_clus2 (pie).pdf", G_pieK_tot_clus2, width = 4, height = 4, units = "in") 


# Cluster 3 (36) Total Ab 

# Percentage - Cluster 3 (36) Total Ab 
pieK_tot_clus3 <- pieK_tot %>% 
  filter(Cluster == "Cluster-3") %>% 
  count(Diabetes) %>% 
  mutate(Percentage = n/sum(n)*100) %>%  
  mutate(csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos))

G_pieK_tot_clus3 <- ggplot(pieK_tot_clus3, aes(x = "" , y = Percentage, fill = fct_inorder(Diabetes))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) + 
  geom_label_repel(data = pieK_tot_clus3,
                   aes(y = pos, label = paste0(Percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 
G_pieK_tot_clus3 
ggsave("Kid_tot_clus3 (pie).pdf", G_pieK_tot_clus3, width = 4, height = 4, units = "in") 
