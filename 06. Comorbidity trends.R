setwd("E:\\Mousumi Ma'am (Project-1)\\6) Comorbidities Trends - Review")

library("readxl") 
library(dplyr)
library(ggplot2) 
library(ggpubr) 
library(rstatix)

library(RColorBrewer)


#===================== Main Data & Classifying it into 3 seperate groups =======================================================================================================

all <- read_excel("Sheet1.xlsx") 

# Data tidying for Boxplot with Jitter (Vacc, Unvacc, Nat_infected)  

tidydata <- all %>% 
  mutate(Vacc_status = if_else(Vaccinated == "Yes", "vaccinated",
                       if_else(Vaccinated == "No" & Covid_event_1 == "No" & Covid_event_2 == "No", "unvaccinated", 
                       if_else(Vaccinated == "No" & Covid_event_1 == "Yes" | Vaccinated == "No" & Covid_event_2 == "Yes", "nat_infected", NULL)))) %>%  
  filter(Vacc_status %in% c("vaccinated", "unvaccinated", "nat_infected")) 

tidydata <- tidydata %>% dplyr::filter(IgG_level >= 1000 & IgG_level <= 140000 & TotalAntibody >= 1000) 



#===================== Date Conversion and Calculation =========================================================================================================================

# Format fixing 

tidydata$Dose1_Date <- gsub('\\.', '-', tidydata$Dose1_Date) 
tidydata$Dose2_Date <- gsub('\\.', '-', tidydata$Dose2_Date)  
tidydata$Positive_Date <- gsub('\\.', '-', tidydata$Positive_Date)
tidydata$Negative_Date <- gsub('\\.', '-', tidydata$Negative_Date) 
tidydata$Date_antibody_cecking <- gsub('\\.', '-', tidydata$Date_antibody_cecking)

class(tidydata$Date_antibody_cecking)

tidydata$Dose1_Date <- as.Date(tidydata$Dose1_Date, format('%d-%m-%Y')) 
tidydata$Dose2_Date <- as.Date(tidydata$Dose2_Date, format('%d-%m-%Y')) 
tidydata$Positive_Date <- as.Date(tidydata$Positive_Date, format('%d-%m-%Y')) 
tidydata$Negative_Date <- as.Date(tidydata$Negative_Date, format('%d-%m-%Y')) 
tidydata$Date_antibody_cecking <- as.Date(tidydata$Date_antibody_cecking, format('%d-%m-%Y')) 

class(tidydata$Date_antibody_cecking)


# Calculate Differences & assign the time interval values to a new column  

# Ab Check - Dose 1 
tidydata$int_Dose1_Ab <- tidydata$Date_antibody_cecking - tidydata$Dose1_Date
tidydata$int_Dose1_Ab <- round(tidydata$int_Dose1_Ab/30, digits = 0)  
tidydata$int_Dose1_Ab 
tidydata[tidydata$int_Dose1_Ab %in% c(-1:-50) , ] 
filter(tidydata, int_Dose1_Ab %in% c(-1:-50)) 

# Ab Check - Dose 2 
tidydata$int_Dose2_Ab <- tidydata$Date_antibody_cecking - tidydata$Dose2_Date
tidydata$int_Dose2_Ab <- round(tidydata$int_Dose2_Ab/30, digits = 0)  
tidydata$int_Dose2_Ab 
tidydata[tidydata$int_Dose2_Ab %in% c(-1:-50) , ] 
filter(tidydata, int_Dose2_Ab %in% c(-1:-50)) 

# Dose 1 - Covid (+) Date 
tidydata$int_Dose1_pos <- tidydata$Dose1_Date - tidydata$Positive_Date
tidydata$int_Dose1_pos <- round(tidydata$int_Dose1_pos/30, digits = 0)  
tidydata$int_Dose1_pos 
tidydata[tidydata$int_Dose1_pos %in% c(-1:-50) , ] 
tidydata[tidydata$int_Dose1_pos %in% c(0:50) , ] 
filter(tidydata, int_Dose1_pos %in% c(-1:-50)) 
filter(tidydata, int_Dose1_pos %in% c(0:50)) 

# Ab Check - Covid (+) Date 
tidydata$int_pos_Ab <- tidydata$Date_antibody_cecking - tidydata$Positive_Date
tidydata$int_pos_Ab <- round(tidydata$int_pos_Ab/30, digits = 0)  
tidydata$int_pos_Ab
tidydata[tidydata$int_pos_Ab %in% c(-1:-50) , ] 
filter(tidydata, int_pos_Ab %in% c(-1:-50))


#===================== Comorbidities all in 1 ==================================================================================================================================

Diabetes <- dplyr::filter(tidydata, Diabetes == "Diabetes") %>%  
  select(-c(Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)) 
#%>%  filter(int_Dose2_Ab %in% c(0:50))  

Hypertension <- dplyr::filter(tidydata, Hypertension == "Hypertension") %>%  
  select(-c(Diabetes, Heart_Disease, Kidney_Disease, Cancer, Others))
#%>% filter(int_Dose2_Ab %in% c(0:50))  

Heart_Disease <- dplyr::filter(tidydata, Heart_Disease == "Heart Disease") %>%  
  select(-c(Diabetes, Hypertension, Kidney_Disease, Cancer, Others))
#%>% filter(int_Dose2_Ab %in% c(0:50))  

Kidney_Disease <- dplyr::filter(tidydata, Kidney_Disease == "Kidney Disease") %>%  
  select(-c(Diabetes, Hypertension, Heart_Disease, Cancer, Others))
#%>% filter(int_Dose2_Ab %in% c(0:50))  

Cancer <- dplyr::filter(tidydata, Cancer == "Cancer") %>%  
  select(-c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Others))
#%>% filter(int_Dose2_Ab %in% c(0:50))  

Others <- dplyr::filter(tidydata, Others == "Others") %>%  
  select(-c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer))
#%>% filter(int_Dose2_Ab %in% c(0:50))  

comorALL_ALL <- bind_rows(Diabetes, Hypertension) %>% 
  bind_rows(Heart_Disease) %>%  
  bind_rows(Kidney_Disease) %>% 
  bind_rows(Cancer) %>% 
  bind_rows(Others) 


library(tidyr) 
comorInONE_ALL <- comorALL_ALL %>%  
  unite("Comorbidities_All", c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others), na.rm = TRUE)


comorInONE_ALL$Comorbidities_All <- factor(comorInONE_ALL$Comorbidities_All, levels = c("Diabetes", "Hypertension", "Heart Disease", "Kidney Disease", "Cancer", "Others"))



#===================== Stats ===================================================================================================================================================

# Spearman Correlation Coefficient test (IgG) 
Diabetes$int_Dose2_Ab <- as.numeric(Diabetes$int_Dose2_Ab) 
cor.test(Diabetes$IgG_level, Diabetes$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Hypertension$int_Dose2_Ab <- as.numeric(Hypertension$int_Dose2_Ab) 
cor.test(Hypertension$IgG_level, Hypertension$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Heart_Disease$int_Dose2_Ab <- as.numeric(Heart_Disease$int_Dose2_Ab) 
cor.test(Heart_Disease$IgG_level, Heart_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Kidney_Disease$int_Dose2_Ab <- as.numeric(Kidney_Disease$int_Dose2_Ab) 
cor.test(Kidney_Disease$IgG_level, Kidney_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Cancer$int_Dose2_Ab <- as.numeric(Cancer$int_Dose2_Ab) 
cor.test(Cancer$IgG_level, Cancer$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Others$int_Dose2_Ab <- as.numeric(Others$int_Dose2_Ab) 
cor.test(Others$IgG_level, Others$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 


# Spearman Correlation Coefficient test (Total Ab) 
Diabetes$int_Dose2_Ab <- as.numeric(Diabetes$int_Dose2_Ab) 
cor.test(Diabetes$TotalAntibody, Diabetes$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Hypertension$int_Dose2_Ab <- as.numeric(Hypertension$int_Dose2_Ab) 
cor.test(Hypertension$TotalAntibody, Hypertension$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Heart_Disease$int_Dose2_Ab <- as.numeric(Heart_Disease$int_Dose2_Ab) 
cor.test(Heart_Disease$TotalAntibody, Heart_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Kidney_Disease$int_Dose2_Ab <- as.numeric(Kidney_Disease$int_Dose2_Ab) 
cor.test(Kidney_Disease$TotalAntibody, Kidney_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Cancer$int_Dose2_Ab <- as.numeric(Cancer$int_Dose2_Ab) 
cor.test(Cancer$TotalAntibody, Cancer$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

Others$int_Dose2_Ab <- as.numeric(Others$int_Dose2_Ab) 
cor.test(Others$TotalAntibody, Others$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 




#===================== Plots ==================================================================================================================================================

comorInONE_ALL$Comorbidities_All <- factor(comorInONE_ALL$Comorbidities_All, levels = c("Diabetes", "Hypertension", "Heart Disease", "Kidney Disease", "Others", "Cancer"))

# IgG 

IgGtime <- ggplot(comorInONE_ALL, aes(x= int_Dose2_Ab, y= IgG_level)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", aes(fill = Comorbidities_All), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  scale_fill_manual(values = c("#ffa500", "#ff595e", "#2ec4b6", "#72cc50", "#396EB0", "#B4C6A6")) + 
  ylim(0, 200000) + 
  xlim(0, 8) + 
  facet_wrap(~ Comorbidities_All, nrow =2, scales= "free")  

IgGtime 

ggsave("IgG_All5.pdf", IgGtime, width = 7.5, height = 4, units = "in")


# Total Ab 

toTtime <- ggplot(comorInONE_ALL, aes(x= int_Dose2_Ab, y= TotalAntibody)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", aes(fill = Comorbidities_All), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  scale_fill_manual(values = c("#ffa500", "#ff595e", "#2ec4b6", "#72cc50", "#396EB0", "#B4C6A6")) + 
  ylim(0, 200000) + 
  xlim(0, 8) + 
  facet_wrap(~ Comorbidities_All, nrow =2, scales= "free")  

toTtime 

ggsave("total_All5.pdf", toTtime, width = 7.5, height = 4, units = "in")


library(Rmisc)

vaccinated <- dplyr::filter(tidydata, Vacc_status == "vaccinated") 


vaccinated$int_pos_Ab <- as.numeric(vaccinated$int_pos_Ab)

summary(na.omit(vaccinated$int_pos_Ab))  
CI(na.omit(vaccinated$int_pos_Ab))


# Experiment 

Kidney <- comorInONE_ALL %>% 
  filter(Comorbidities_All == "Kidney Disease") %>% 
  filter(int_Dose2_Ab >= 0 )

cor.test(Diabetes$TotalAntibody, Diabetes$int_Dose2_Ab, method = "pearson", use = "na.or.complete") 
cor.test(Diabetes$TotalAntibody, Diabetes$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

cor.test(Hypertension$TotalAntibody, Hypertension$int_Dose2_Ab, method = "pearson", use = "na.or.complete") 
cor.test(Hypertension$TotalAntibody, Hypertension$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

cor.test(Heart_Disease$TotalAntibody, Heart_Disease$int_Dose2_Ab, method = "pearson", use = "na.or.complete") 
cor.test(Heart_Disease$TotalAntibody, Heart_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

cor.test(Kidney_Disease$TotalAntibody, Kidney_Disease$int_Dose2_Ab, method = "pearson", use = "na.or.complete") 
cor.test(Kidney_Disease$TotalAntibody, Kidney_Disease$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

cor.test(Others$TotalAntibody, Others$int_Dose2_Ab, method = "pearson", use = "na.or.complete") 
cor.test(Others$TotalAntibody, Others$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

