setwd("E:\\Mousumi Ma'am (Project-1)\\8) Follow Up (Profile Plot) - Review") 

library(Rmisc) 
library(dplyr)
library(tidyverse)

#================ Data import ======================================================

pieD_IgG <- read.csv("Diabetes_IgG Cluster_Details_SC.csv")
pieD_tot <- read.csv("Diabetes_totalAb Cluster_Details_SC.csv") 
pieK_IgG <- read.csv("Kidney_IgG Cluster_Details_SC.csv") 
pieK_tot <- read.csv("Kidney_totalAb Cluster_Details_SC.csv") 

#================ Diabetes cohort ==================================================

# Diabetes (Age) 

summary(pieD_IgG$Age)
CI(pieD_IgG$Age, ci=0.95)

# Diabetes (IgG Z)  

C1_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-1") 

summary(C1_D_IgG$IgG_z1) 
CI(C1_D_IgG$IgG_z1, ci=0.95) 
summary(C1_D_IgG$IgG_z2) 
CI(C1_D_IgG$IgG_z2, ci=0.95) 
summary(C1_D_IgG$IgG_z3) 
CI(C1_D_IgG$IgG_z3, ci=0.95) 

C2_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-2") 

summary(C2_D_IgG$IgG_z1) 
CI(C2_D_IgG$IgG_z1, ci=0.95) 
summary(C2_D_IgG$IgG_z2) 
CI(C2_D_IgG$IgG_z2, ci=0.95) 
summary(C2_D_IgG$IgG_z3) 
CI(C2_D_IgG$IgG_z3, ci=0.95) 

C3_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(C3_D_IgG$IgG_z1) 
CI(C3_D_IgG$IgG_z1, ci=0.95) 
summary(C3_D_IgG$IgG_z2) 
CI(C3_D_IgG$IgG_z2, ci=0.95) 
summary(C3_D_IgG$IgG_z3) 
CI(C3_D_IgG$IgG_z3, ci=0.95) 

# Diabetes (Total Ab Z) 

C1_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-1") 

summary(C1_D_tot$TotalAb_z1) 
CI(C1_D_tot$TotalAb_z1, ci=0.95) 
summary(C1_D_tot$TotalAb_z2) 
CI(C1_D_tot$TotalAb_z2, ci=0.95) 
summary(C1_D_tot$TotalAb_z3) 
CI(C1_D_tot$TotalAb_z3, ci=0.95) 

C2_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-2") 

summary(C2_D_tot$TotalAb_z1) 
CI(C2_D_tot$TotalAb_z1, ci=0.95) 
summary(C2_D_tot$TotalAb_z2) 
CI(C2_D_tot$TotalAb_z2, ci=0.95) 
summary(C2_D_tot$TotalAb_z3) 
CI(C2_D_tot$TotalAb_z3, ci=0.95) 

C3_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(C3_D_tot$TotalAb_z1) 
CI(C3_D_tot$TotalAb_z1, ci=0.95) 
summary(C3_D_tot$TotalAb_z2) 
CI(C3_D_tot$TotalAb_z2, ci=0.95) 
summary(C3_D_tot$TotalAb_z3) 
CI(C3_D_tot$TotalAb_z3, ci=0.95) 

#================ Kidney cohort ====================================================

# Kidney (Age) 

summary(pieK_IgG$Age)
CI(pieK_IgG$Age, ci=0.95)

# Kidney (IgG Z) 

C1_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-1") 

summary(C1_K_IgG$IgG_z1) 
CI(C1_K_IgG$IgG_z1, ci=0.95) 
summary(C1_K_IgG$IgG_z2) 
CI(C1_K_IgG$IgG_z2, ci=0.95) 
summary(C1_K_IgG$IgG_z3) 
CI(C1_K_IgG$IgG_z3, ci=0.95) 

C2_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-2") 

summary(C2_K_IgG$IgG_z1) 
CI(C2_K_IgG$IgG_z1, ci=0.95) 
summary(C2_K_IgG$IgG_z2) 
CI(C2_K_IgG$IgG_z2, ci=0.95) 
summary(C2_K_IgG$IgG_z3) 
CI(C2_K_IgG$IgG_z3, ci=0.95) 

C3_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(C3_K_IgG$IgG_z1) 
CI(C3_K_IgG$IgG_z1, ci=0.95) 
summary(C3_K_IgG$IgG_z2) 
CI(C3_K_IgG$IgG_z2, ci=0.95) 
summary(C3_K_IgG$IgG_z3) 
CI(C3_K_IgG$IgG_z3, ci=0.95) 

# Kidney (Total Ab Z) 

C1_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-1") 

summary(C1_K_tot$TotalAb_z1) 
CI(C1_K_tot$TotalAb_z1, ci=0.95) 
summary(C1_K_tot$TotalAb_z2) 
CI(C1_K_tot$TotalAb_z2, ci=0.95) 
summary(C1_K_tot$TotalAb_z3) 
CI(C1_K_tot$TotalAb_z3, ci=0.95) 

C2_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-2") 

summary(C2_K_tot$TotalAb_z1) 
CI(C2_K_tot$TotalAb_z1, ci=0.95) 
summary(C2_K_tot$TotalAb_z2) 
CI(C2_K_tot$TotalAb_z2, ci=0.95) 
summary(C2_K_tot$TotalAb_z3) 
CI(C2_K_tot$TotalAb_z3, ci=0.95) 

C3_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(C3_K_tot$TotalAb_z1) 
CI(C3_K_tot$TotalAb_z1, ci=0.95) 
summary(C3_K_tot$TotalAb_z2) 
CI(C3_K_tot$TotalAb_z2, ci=0.95) 
summary(C3_K_tot$TotalAb_z3) 
CI(C3_K_tot$TotalAb_z3, ci=0.95) 

#================ Date Conversion and Calculation ==================================

# Format fixing 
pieD_IgG$Date_antibody_cecking1 <- gsub('\\.', '-', pieD_IgG$Date_antibody_cecking1) 
pieD_IgG$Date_antibody_cecking2 <- gsub('\\.', '-', pieD_IgG$Date_antibody_cecking2)
pieD_IgG$Date_antibody_cecking3 <- gsub('\\.', '-', pieD_IgG$Date_antibody_cecking3)

pieD_tot$Date_antibody_cecking1 <- gsub('\\.', '-', pieD_tot$Date_antibody_cecking1) 
pieD_tot$Date_antibody_cecking2 <- gsub('\\.', '-', pieD_tot$Date_antibody_cecking2)
pieD_tot$Date_antibody_cecking3 <- gsub('\\.', '-', pieD_tot$Date_antibody_cecking3)

pieK_IgG$Date_antibody_cecking1 <- gsub('\\.', '-', pieK_IgG$Date_antibody_cecking1) 
pieK_IgG$Date_antibody_cecking2 <- gsub('\\.', '-', pieK_IgG$Date_antibody_cecking2)
pieK_IgG$Date_antibody_cecking3 <- gsub('\\.', '-', pieK_IgG$Date_antibody_cecking3)

pieK_tot$Date_antibody_cecking1 <- gsub('\\.', '-', pieK_tot$Date_antibody_cecking1) 
pieK_tot$Date_antibody_cecking2 <- gsub('\\.', '-', pieK_tot$Date_antibody_cecking2)
pieK_tot$Date_antibody_cecking3 <- gsub('\\.', '-', pieK_tot$Date_antibody_cecking3)

class(pieD_IgG$Date_antibody_cecking1)

pieD_IgG$Date_antibody_cecking1 <- as.Date(pieD_IgG$Date_antibody_cecking1, format('%d-%m-%Y')) 
pieD_IgG$Date_antibody_cecking2 <- as.Date(pieD_IgG$Date_antibody_cecking2, format('%d-%m-%Y')) 
pieD_IgG$Date_antibody_cecking3 <- as.Date(pieD_IgG$Date_antibody_cecking3, format('%d-%m-%Y')) 

pieD_tot$Date_antibody_cecking1 <- as.Date(pieD_tot$Date_antibody_cecking1, format('%d-%m-%Y')) 
pieD_tot$Date_antibody_cecking2 <- as.Date(pieD_tot$Date_antibody_cecking2, format('%d-%m-%Y')) 
pieD_tot$Date_antibody_cecking3 <- as.Date(pieD_tot$Date_antibody_cecking3, format('%d-%m-%Y')) 

pieK_IgG$Date_antibody_cecking1 <- as.Date(pieK_IgG$Date_antibody_cecking1, format('%d-%m-%Y')) 
pieK_IgG$Date_antibody_cecking2 <- as.Date(pieK_IgG$Date_antibody_cecking2, format('%d-%m-%Y')) 
pieK_IgG$Date_antibody_cecking3 <- as.Date(pieK_IgG$Date_antibody_cecking3, format('%d-%m-%Y')) 

pieK_tot$Date_antibody_cecking1 <- as.Date(pieK_tot$Date_antibody_cecking1, format('%d-%m-%Y')) 
pieK_tot$Date_antibody_cecking2 <- as.Date(pieK_tot$Date_antibody_cecking2, format('%d-%m-%Y')) 
pieK_tot$Date_antibody_cecking3 <- as.Date(pieK_tot$Date_antibody_cecking3, format('%d-%m-%Y')) 

class(pieD_IgG$Date_antibody_cecking1) 


# Calculate Differences & assign the time interval values to a new column  

library(Rmisc) 

# pieD_IgG (t2-t1) 

pieD_IgG$t2_t1 <- pieD_IgG$Date_antibody_cecking2 - pieD_IgG$Date_antibody_cecking1
pieD_IgG$t2_t1 <- round(pieD_IgG$t2_t1/30)

pieD_IgG$t2_t1 <- as.numeric(pieD_IgG$t2_t1) 

CL1_1_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-1")  
CL2_1_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-2") 
CL3_1_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_1_D_IgG$t2_t1)
CI(CL1_1_D_IgG$t2_t1, ci=0.95) 

summary(CL2_1_D_IgG$t2_t1)
CI(CL2_1_D_IgG$t2_t1, ci=0.95)  

summary(CL3_1_D_IgG$t2_t1)
CI(CL3_1_D_IgG$t2_t1, ci=0.95) 


# pieD_IgG (t3-t2) 

pieD_IgG$t3_t2 <- pieD_IgG$Date_antibody_cecking3 - pieD_IgG$Date_antibody_cecking2
pieD_IgG$t3_t2 <- round(pieD_IgG$t3_t2/30) 

pieD_IgG$t3_t2 <- as.numeric(pieD_IgG$t3_t2) 

CL1_2_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-1")  
CL2_2_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-2") 
CL3_2_D_IgG <- pieD_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_2_D_IgG$t3_t2)
CI(CL1_2_D_IgG$t3_t2, ci=0.95) 

summary(CL2_2_D_IgG$t3_t2)
CI(CL2_2_D_IgG$t3_t2, ci=0.95)  

summary(CL3_2_D_IgG$t3_t2)
CI(CL3_2_D_IgG$t3_t2, ci=0.95) 


# pieD_tot (t2-t1) 

pieD_tot$t2_t1 <- pieD_tot$Date_antibody_cecking2 - pieD_tot$Date_antibody_cecking1
pieD_tot$t2_t1 <- round(pieD_tot$t2_t1/30) 

pieD_tot$t2_t1 <- as.numeric(pieD_tot$t2_t1) 

CL1_1_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-1")  
CL2_1_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-2") 
CL3_1_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_1_D_tot$t2_t1)
CI(CL1_1_D_tot$t2_t1, ci=0.95) 

summary(CL2_1_D_tot$t2_t1)
CI(CL2_1_D_tot$t2_t1, ci=0.95)  

summary(CL3_1_D_tot$t2_t1)
CI(CL3_1_D_tot$t2_t1, ci=0.95)


# pieD_tot (t3-t2)  

pieD_tot$t3_t2 <- pieD_tot$Date_antibody_cecking3 - pieD_tot$Date_antibody_cecking2
pieD_tot$t3_t2 <- round(pieD_tot$t3_t2/30) 

pieD_tot$t3_t2 <- as.numeric(pieD_tot$t3_t2)

CL1_2_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-1")  
CL2_2_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-2") 
CL3_2_D_tot <- pieD_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_2_D_tot$t3_t2)
CI(CL1_2_D_tot$t3_t2, ci=0.95) 

summary(CL2_2_D_tot$t3_t2)
CI(CL2_2_D_tot$t3_t2, ci=0.95)  

summary(CL3_2_D_tot$t3_t2)
CI(CL3_2_D_tot$t3_t2, ci=0.95) 


# pieK_IgG (t2-t1)  

pieK_IgG$t2_t1 <- pieK_IgG$Date_antibody_cecking2 - pieD_tot$Date_antibody_cecking1
pieK_IgG$t2_t1 <- round(pieK_IgG$t2_t1/30) 

pieK_IgG$t2_t1 <- as.numeric(pieK_IgG$t2_t1)

CL1_1_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-1")  
CL2_1_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-2") 
CL3_1_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_1_K_IgG$t2_t1)
CI(CL1_1_K_IgG$t2_t1, ci=0.95) 

summary(CL2_1_K_IgG$t2_t1)
CI(CL2_1_K_IgG$t2_t1, ci=0.95)  

summary(CL3_1_K_IgG$t2_t1)
CI(CL3_1_K_IgG$t2_t1, ci=0.95) 

# pieK_IgG (t3-t2) 

pieK_IgG$t3_t2 <- pieK_IgG$Date_antibody_cecking3 - pieD_tot$Date_antibody_cecking2
pieK_IgG$t3_t2 <- round(pieK_IgG$t3_t2/30) 

pieK_IgG$t3_t2 <- as.numeric(pieK_IgG$t3_t2)

CL1_2_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-1")  
CL2_2_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-2") 
CL3_2_K_IgG <- pieK_IgG %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_2_K_IgG$t3_t2)
CI(CL1_2_K_IgG$t3_t2, ci=0.95) 

summary(CL2_2_K_IgG$t3_t2)
CI(CL2_2_K_IgG$t3_t2, ci=0.95)  

summary(CL3_2_K_IgG$t3_t2)
CI(CL3_2_K_IgG$t3_t2, ci=0.95) 


# pieK_tot (t2-t1) 

pieK_tot$t2_t1 <- pieK_tot$Date_antibody_cecking2 - pieD_tot$Date_antibody_cecking1
pieK_tot$t2_t1 <- round(pieK_tot$t2_t1/30) 

pieK_tot$t2_t1 <- as.numeric(pieK_tot$t2_t1)

CL1_1_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-1")  
CL2_1_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-2") 
CL3_1_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_1_K_tot$t2_t1)
CI(CL1_1_K_tot$t2_t1, ci=0.95) 

summary(CL2_1_K_tot$t2_t1)
CI(CL2_1_K_tot$t2_t1, ci=0.95)  

summary(CL3_1_K_tot$t2_t1)
CI(CL3_1_K_tot$t2_t1, ci=0.95) 

# pieK_tot (t3-t2)  

pieK_tot$t3_t2 <- pieK_tot$Date_antibody_cecking3 - pieD_tot$Date_antibody_cecking2 
pieK_tot$t3_t2 <- round(pieK_tot$t3_t2/30) 

pieK_tot$t3_t2 <- as.numeric(pieK_tot$t3_t2)

CL1_2_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-1")  
CL2_2_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-2") 
CL3_2_K_tot <- pieK_tot %>% 
  filter(Clusters == "Cluster-3") 

summary(CL1_2_K_tot$t3_t2)
CI(CL1_2_K_tot$t2_t1, ci=0.95) 

summary(CL2_2_K_tot$t3_t2)
CI(CL2_2_K_tot$t2_t1, ci=0.95)  

summary(CL3_2_K_tot$t3_t2)
CI(CL3_2_K_tot$t2_t1, ci=0.95) 


