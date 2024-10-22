setwd("E:\\8) Follow Up (Profile Plot) - Review") 

library(dplyr)
library(tidyverse) 
library(ggrepel)
library(ggplot2)
library(RColorBrewer) 
require(scales) 

# Data import 

# Diabetes IgG 
dia_IgG <- read.csv("Diabetes_IgG.csv") 
pieD_IgG <- read.csv("Diabetes_IgG Cluster_Details_SC.csv") 

# Diabetes TAb 
dia_tot <- read.csv("Diabetes_totalAb.csv") 
pieD_tot <- read.csv("Diabetes_totalAb Cluster_Details_SC.csv") 

# Kidney Disease IgG 
kid_IgG <- read.csv("Kidney_IgG.csv") 
pieK_IgG <- read.csv("Kidney_IgG Cluster_Details_SC.csv") 

# Kidney Disease TAb 
kid_tot <- read.csv("Kidney_totalAb.csv") 
pieK_tot <- read.csv("Kidney_totalAb Cluster_Details_SC.csv") 

#===================== IgG Diabetes (Cluster 1) ==============================================================================

diaIgG_clus1 <- dia_IgG %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols = 4:6, names_to = "time", values_to = "Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL, color = "#A6D854"), size = 0.5) + 
  geom_point(size = 1.5, pch = 21) +  
  scale_fill_manual(values = c("#A6D854")) + 
  scale_colour_manual(values = c("#A6D854")) + 
  theme_bw()   

diaIgG_clus1

ggsave("Dia_IgG_clus1 (COPY).pdf", diaIgG_clus1, width = 4, height = 1.5, units = "in")  


# New Profile Plot - Cluster 1 
diaIgG_clus1 <- dia_IgG %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols = 4:6, names_to = "time", values_to = "Z_value") 


p_diaIgG_clus1 <- ggplot() + 
  geom_line(data = diaIgG_clus1[diaIgG_clus1$Status_2 == "Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diaIgG_clus1[diaIgG_clus1$Status_2 == "Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaIgG_clus1 

p_diaIgG_clus1 <- p_diaIgG_clus1 + 
  geom_line(data = diaIgG_clus1[diaIgG_clus1$Status_2 != "Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = diaIgG_clus1[diaIgG_clus1$Status_2 != "Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  
  
p_diaIgG_clus1 <- p_diaIgG_clus1 + theme_bw() 

p_diaIgG_clus1

ggsave("Dia_IgG_clus1.pdf", p_diaIgG_clus1, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 1 IgG 

pieD_IgG_clus1 <- pieD_IgG %>% 
  filter(Clusters == "Cluster-1") %>%  
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


# Stacked Bar Plot - Cluster 1 IgG 

pieD_IgG %>% 
  filter(Clusters == "Cluster-1") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill = Kidney_Disease)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c("#AADADC", "#e86e77")) + 
  theme_bw() 

ggsave("Dia_IgG_clus1 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== IgG Diabetes (Cluster 2) ==============================================================================

diaIgG_clus2 <- dia_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols = 4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size = 0.5, color= "#8DA0CB") + 
  geom_point(size = 1.5, pch = 21) +  
  scale_fill_manual(values = c("#8DA0CB")) + 
  theme_bw() 

diaIgG_clus2 

ggsave("Dia_IgG_clus2 (COPY).pdf", diaIgG_clus2, width = 4, height = 1.5, units = "in")  


# New Profile Plot - Cluster 2 IgG 

diaIgG_clus2 <- dia_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols = 4:6, names_to = "time", values_to = "Z_value") 

p_diaIgG_clus2 <- ggplot() + 
  geom_line(data = diaIgG_clus2[diaIgG_clus2$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diaIgG_clus2[diaIgG_clus2$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaIgG_clus2 

p_diaIgG_clus2 <- p_diaIgG_clus2 + 
  geom_line(data = diaIgG_clus2[diaIgG_clus2$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#3F8F7E", "#FFBD35")) + 
  geom_point(data = diaIgG_clus2[diaIgG_clus2$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#3F8F7E", "#FFBD35"))  

p_diaIgG_clus2 <- p_diaIgG_clus2 + theme_bw() 

p_diaIgG_clus2

ggsave("Dia_IgG_clus2.pdf", p_diaIgG_clus2, width = 4, height = 1.5, units = "in") 



# Percentage - Cluster 2 IgG  

pieD_IgG_clus2 <- pieD_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
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


# Stacked Bar Plot - Cluster 2 IgG  

pieD_IgG %>% 
  filter(Clusters == "Cluster-2") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>% 
  add_row(Kidney_Disease = "No", n = 0, Percentage = 0) %>%
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill = Kidney_Disease)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Dia_IgG_clus2 (bar).pdf", width = 3, height = 2.5, units = "in") 

#===================== IgG Diabetes (Cluster 3) ==============================================================================

diaIgG_clus3 <- dia_IgG %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols = 4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill=Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#E78AC3") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#E78AC3")) + 
  theme_bw() 

diaIgG_clus3 

ggsave("Dia_IgG_clus3 (COPY).pdf", diaIgG_clus3, width = 4, height = 1.5, units = "in")  


# New Profile Plot - Cluster 3 

diaIgG_clus3 <- dia_IgG %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols = 4:6, names_to = "time", values_to = "Z_value") 

p_diaIgG_clus3 <- ggplot() + 
  geom_line(data = diaIgG_clus3[diaIgG_clus3$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diaIgG_clus3[diaIgG_clus3$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaIgG_clus3 

p_diaIgG_clus3 <- p_diaIgG_clus3 + 
  geom_line(data = diaIgG_clus3[diaIgG_clus3$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = diaIgG_clus3[diaIgG_clus3$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  

p_diaIgG_clus3 <- p_diaIgG_clus3 + theme_bw() 

p_diaIgG_clus3

ggsave("Dia_IgG_clus3.pdf", p_diaIgG_clus3, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 3 IgG 

pieD_IgG_clus3 <- pieD_IgG %>% 
  filter(Clusters == "Cluster-3") %>% 
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


# Stacked Bar Plot - Cluster 3 IgG  

pieD_IgG %>% 
  filter(Clusters == "Cluster-3") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Kidney_Disease = "No", n = 0, Percentage = 0) %>% 
  add_row(Kidney_Disease = "Yes", n = 0, Percentage = 0) %>% 
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill=Kidney_Disease)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw()  

ggsave("Dia_IgG_clus3 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== Total Ab Diabetes (Cluster 1) =========================================================================

diatot_clus1 <- dia_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols= 4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill=Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#A6D854") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#A6D854")) + 
  theme_bw() 

diatot_clus1 

ggsave("Dia_tot_clus1 (COPY).pdf", diatot_clus1, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 1 Total Ab 

diatot_clus1 <- dia_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols= 4:6, names_to="time", values_to="Z_value") 


p_diaTot_clus1 <- ggplot() + 
  geom_line(data = diatot_clus1[diatot_clus1$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diatot_clus1[diatot_clus1$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaTot_clus1 

p_diaTot_clus1 <- p_diaTot_clus1 + 
  geom_line(data = diatot_clus1[diatot_clus1$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#3F8F7E", "#FFBD35")) + 
  geom_point(data = diatot_clus1[diatot_clus1$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#3F8F7E", "#FFBD35"))  

p_diaTot_clus1 <- p_diaTot_clus1 + theme_bw() 

p_diaTot_clus1

ggsave("Dia_tot_clus1.pdf", p_diaTot_clus1, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 1 Total Ab 

pieD_tot_clus1 <- pieD_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
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


# Stacked Bar Plot - Cluster 1 Total Ab 

pieD_tot %>% 
  filter(Clusters == "Cluster-1") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Kidney_Disease = "No", n = 0, Percentage = 0) %>%  
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill=Kidney_Disease)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Dia_tot_clus1 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== Total Ab Diabetes (Cluster 2) =========================================================================

diatot_clus2 <- dia_tot %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols= 4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill=Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#8DA0CB") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#8DA0CB")) + 
  theme_bw() 

diatot_clus2  

ggsave("Dia_tot_clus2 (COPY).pdf", diatot_clus2, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 2 Total Ab 

diatot_clus2 <- dia_tot %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 


p_diaTot_clus2 <- ggplot() + 
  geom_line(data = diatot_clus2[diatot_clus2$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diatot_clus2[diatot_clus2$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaTot_clus2 

p_diaTot_clus2 <- p_diaTot_clus2 + 
  geom_line(data = diatot_clus2[diatot_clus2$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = diatot_clus2[diatot_clus2$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  

p_diaTot_clus2 <- p_diaTot_clus2 + theme_bw() 

p_diaTot_clus2  

ggsave("Dia_tot_clus2.pdf", p_diaTot_clus2, width = 4, height = 1.5, units = "in") 



# Percentage - Cluster 2 Total Ab  

pieD_tot_clus2 <- pieD_tot %>% 
  filter(Clusters == "Cluster-2") %>% 
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


# Stacked Bar Plot - Cluster 2 Total Ab  

pieD_tot %>% 
  filter(Clusters == "Cluster-2") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Kidney_Disease = "No", n = 0, Percentage = 0) %>%  
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill=Kidney_Disease)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Dia_tot_clus2 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== Total Ab Diabetes (Cluster 3) =========================================================================

diatot_clus3 <- dia_tot %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols= 4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#E78AC3") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#E78AC3")) + 
  theme_bw() 

diatot_clus3 

ggsave("Dia_tot_clus3 (COPY).pdf", diatot_clus3, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 3 Total Ab 

diatot_clus3 <- dia_tot %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_diaTot_clus3 <- ggplot() + 
  geom_line(data = diatot_clus3[diatot_clus3$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = diatot_clus3[diatot_clus3$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_diaTot_clus3 

p_diaTot_clus3 <- p_diaTot_clus3 + 
  geom_line(data = diatot_clus3[diatot_clus3$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = diatot_clus3[diatot_clus3$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  

p_diaTot_clus3 <- p_diaTot_clus3 + theme_bw() 

p_diaTot_clus3  

ggsave("Dia_tot_clus3.pdf", p_diaTot_clus3, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 3 Total Ab  

pieD_tot_clus3 <- pieD_tot %>% 
  filter(Clusters == "Cluster-3") %>%  
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


# Stacked Bar Plot - Cluster 3 Total Ab  

pieD_tot %>% 
  filter(Clusters == "Cluster-3") %>%  
  count(Kidney_Disease) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Kidney_Disease = "Yes", n = 0, Percentage = 0) %>%  
  ggplot(aes(x = Kidney_Disease, y = Percentage, fill=Kidney_Disease)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Dia_tot_clus3 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== IgG Kidney Disease (Cluster 1) ========================================================================

kidIgG_clus1 <- kid_IgG %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#A6D854") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#A6D854")) + 
  theme_bw() 

kidIgG_clus1 

ggsave("Kid_IgG_clus1 (COPY).pdf", kidIgG_clus1, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 1 IgG  

kidIgG_clus1 <- kid_IgG %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols= 4:6, names_to="time", values_to="Z_value") 

p_kidIgG_clus1 <- ggplot() + 
  geom_line(data = kidIgG_clus1[kidIgG_clus1$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidIgG_clus1[kidIgG_clus1$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidIgG_clus1 

p_kidIgG_clus1 <- p_kidIgG_clus1 + 
  geom_line(data = kidIgG_clus1[kidIgG_clus1$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#F16D91", "#FFBD35")) + 
  geom_point(data = kidIgG_clus1[kidIgG_clus1$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#F16D91", "#FFBD35"))  

p_kidIgG_clus1 <- p_kidIgG_clus1 + theme_bw() 

p_kidIgG_clus1  

ggsave("Kid_IgG_clus1.pdf", p_kidIgG_clus1, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 1 IgG  

pieK_IgG_clus1 <- pieK_IgG %>% 
  filter(Clusters == "Cluster-1") %>% 
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


# Stacked Bar Plot - Cluster 1 IgG  

pieK_IgG %>% 
  filter(Clusters == "Cluster-1") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>% 
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Kid_IgG_clus1 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== IgG Kidney Disease (Cluster 2) ========================================================================

kidIgG_clus2 <- kid_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill=Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#8DA0CB") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#8DA0CB")) + 
  theme_bw() 

kidIgG_clus2 

ggsave("Kid_IgG_clus2 (COPY).pdf", kidIgG_clus2, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 2 IgG  

kidIgG_clus2 <- kid_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_kidIgG_clus2 <- ggplot() + 
  geom_line(data = kidIgG_clus2[kidIgG_clus2$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidIgG_clus2[kidIgG_clus2$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidIgG_clus2 

p_kidIgG_clus2 <- p_kidIgG_clus2 + 
  geom_line(data = kidIgG_clus2[kidIgG_clus2$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#3F8F7E", "#FFBD35")) + 
  geom_point(data = kidIgG_clus2[kidIgG_clus2$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#3F8F7E", "#FFBD35"))  

p_kidIgG_clus2 <- p_kidIgG_clus2 + theme_bw() 

p_kidIgG_clus2  

ggsave("Kid_IgG_clus2.pdf", p_kidIgG_clus2, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 2 IgG  

pieK_IgG_clus2 <- pieK_IgG %>% 
  filter(Clusters == "Cluster-2") %>% 
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



# Stacked Bar Plot - Cluster 2 IgG 

pieK_IgG %>% 
  filter(Clusters == "Cluster-2") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Kid_IgG_clus2 (bar).pdf", width = 3, height = 2.5, units = "in") 



#===================== IgG Kidney Disease (Cluster 3) ========================================================================

kidIgG_clus3 <- kid_IgG %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#E78AC3") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#E78AC3")) + 
  theme_bw() 

kidIgG_clus3

ggsave("Kid_IgG_clus3 (COPY).pdf", kidIgG_clus3, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 3 IgG  

kidIgG_clus3 <- kid_IgG %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_kidIgG_clus3 <- ggplot() + 
  geom_line(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidIgG_clus3 

p_kidIgG_clus3 <- p_kidIgG_clus3 + 
  geom_line(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Unvac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Unvac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 
p_kidIgG_clus3

p_kidIgG_clus3 <- p_kidIgG_clus3 + 
  geom_line(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Nat_Inf", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = kidIgG_clus3[kidIgG_clus3$Status_2 =="Nat_Inf", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  


p_kidIgG_clus3 <- p_kidIgG_clus3 + theme_bw() 

p_kidIgG_clus3  

ggsave("Kid_IgG_clus3.pdf", p_kidIgG_clus3, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 3 IgG 

pieK_IgG_clus3 <- pieK_IgG %>% 
  filter(Clusters == "Cluster-3") %>%  
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


# Stacked Bar Plot - Cluster 3 IgG 

pieK_IgG %>% 
  filter(Clusters == "Cluster-3") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Kid_IgG_clus3 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== Total Ab Kidney Disease (Cluster 1) ===================================================================

kidtot_clus1 <- kid_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#A6D854") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#A6D854")) + 
  theme_bw() 

kidtot_clus1 

ggsave("Kid_tot_clus1 (COPY).pdf", kidtot_clus1, width = 4, height = 1.5, units = "in")  


# New Profile Plot - Cluster 1 Total Ab 

kidtot_clus1 <- kid_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_kidTot_clus1 <- ggplot() + 
  geom_line(data = kidtot_clus1[kidtot_clus1$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidtot_clus1[kidtot_clus1$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidTot_clus1 

p_kidTot_clus1 <- p_kidTot_clus1 + 
  geom_line(data = kidtot_clus1[kidtot_clus1$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#F16D91", "#FFBD35")) + 
  geom_point(data = kidtot_clus1[kidtot_clus1$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#F16D91", "#FFBD35"))  

p_kidTot_clus1 <- p_kidTot_clus1 + theme_bw() 

p_kidTot_clus1  

ggsave("Kid_tot_clus1.pdf", p_kidTot_clus1, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 1 Total Ab 

pieK_tot_clus1 <- pieK_tot %>% 
  filter(Clusters == "Cluster-1") %>% 
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


# Stacked Bar Plot - Cluster 1 Total Ab 

pieK_tot %>% 
  filter(Clusters == "Cluster-1") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw()  

ggsave("Kid_tot_clus1 (bar).pdf", width = 3, height = 2.5, units = "in") 



#===================== Total Ab Kidney Disease (Cluster 2) ===================================================================

kidtot_clus2 <- kid_tot %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill=Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#8DA0CB") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#8DA0CB")) + 
  theme_bw() 

kidtot_clus2 

ggsave("Kid_tot_clus2 (COPY).pdf", kidtot_clus2, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 2 Total Ab   

kidtot_clus2 <- kid_tot %>% 
  filter(Clusters == "Cluster-2") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_kidTot_clus2 <- ggplot() + 
  geom_line(data = kidtot_clus2[kidtot_clus2$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidtot_clus2[kidtot_clus2$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidTot_clus2 

p_kidTot_clus2 <- p_kidTot_clus2 + 
  geom_line(data = kidtot_clus2[kidtot_clus2$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#3F8F7E", "#FFBD35")) + 
  geom_point(data = kidtot_clus2[kidtot_clus2$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#3F8F7E", "#FFBD35"))  

p_kidTot_clus2 <- p_kidTot_clus2 + theme_bw() 

p_kidTot_clus2  

ggsave("Kid_tot_clus2.pdf", p_kidTot_clus2, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 2 Total Ab 

pieK_tot_clus2 <- pieK_tot %>% 
  filter(Clusters == "Cluster-2") %>%  
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


# Stacked Bar Plot - Cluster 2 Total Ab 

pieK_tot %>% 
  filter(Clusters == "Cluster-2") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Diabetes = "No", n = 0, Percentage = 0) %>%  
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Kid_tot_clus2 (bar).pdf", width = 3, height = 2.5, units = "in") 


#===================== Total Ab Kidney Disease (Cluster 3) ===================================================================

kidtot_clus3 <- kid_tot %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") %>%  
  
  ggplot(aes(x = time, y = Z_value, fill = Clusters)) + 
  geom_line(aes(group = SL), size=0.5, color= "#E78AC3") + 
  geom_point(size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#E78AC3")) + 
  theme_bw() 

kidtot_clus3 

ggsave("Kid_tot_clus3 (COPY).pdf", kidtot_clus3, width = 4, height = 1.5, units = "in") 


# New Profile Plot - Cluster 3 Total Ab 

kidtot_clus3 <- kid_tot %>% 
  filter(Clusters == "Cluster-3") %>% 
  pivot_longer(cols=4:6, names_to="time", values_to="Z_value") 

p_kidTot_clus3 <- ggplot() + 
  geom_line(data = kidtot_clus3[kidtot_clus3$Status_2 =="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  geom_point(data = kidtot_clus3[kidtot_clus3$Status_2 =="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) 

p_kidTot_clus3 

p_kidTot_clus3 <- p_kidTot_clus3 + 
  geom_line(data = kidtot_clus3[kidtot_clus3$Status_2 !="Vac", ] , 
            aes(x = time, y = Z_value, group = SL, color = Status_2), size=0.5) + 
  scale_color_manual(values = c("#FFBD35")) + 
  geom_point(data = kidtot_clus3[kidtot_clus3$Status_2 !="Vac", ] , 
             aes(x = time, y = Z_value, group = SL, fill = Status_2), size=1.5, pch = 21) +  
  scale_fill_manual(values = c("#FFBD35"))  

p_kidTot_clus3 <- p_kidTot_clus3 + theme_bw() 

p_kidTot_clus3  

ggsave("Kid_tot_clus3.pdf", p_kidTot_clus3, width = 4, height = 1.5, units = "in") 


# Percentage - Cluster 3 (36) Total Ab 

pieK_tot_clus3 <- pieK_tot %>% 
  filter(Clusters == "Cluster-3") %>% 
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


# Stacked Bar Plot - Cluster 3 Total Ab 

pieK_tot %>% 
  filter(Clusters == "Cluster-3") %>%  
  count(Diabetes) %>%  
  mutate(Percentage = n/sum(n)*100) %>%  
  add_row(Diabetes = "Yes", n = 0, Percentage = 0) %>%  
  ggplot(aes(x = Diabetes, y = Percentage, fill=Diabetes)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#fcb248", "#e86e77")) + 
  theme_bw() 

ggsave("Kid_tot_clus3 (bar).pdf", width = 3, height = 2.5, units = "in") 

