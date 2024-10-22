setwd("E:\\Mousumi Ma'am (Project-1)\\1) ALL - Review") 

library("readxl") 
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(ggpubr)
library(rstatix)
library(FSA) 
library(forcats)


#===================== Main Data & Classifying it into 3 seperate groups ========================================================================================================== 

all <- read_excel("Sheet1.xlsx") 


# Data tidying for Boxplot with Jitter (Vacc, Unvacc, Nat_infected) ---- 

tidydata <- all %>% 
  mutate(Vacc_status = if_else(Vaccinated == "Yes", "vaccinated",
                       if_else(Vaccinated == "No" & Covid_event_1 == "No" & Covid_event_2 == "No", "unvaccinated", 
                       if_else(Vaccinated == "No" & Covid_event_1 == "Yes" | Vaccinated == "No" & Covid_event_2 == "Yes", "nat_infected", NULL)))) %>%  
  filter(Vacc_status %in% c("vaccinated", "unvaccinated", "nat_infected")) 

tidydata <- tidydata %>% dplyr::filter(IgG_level >= 1000 & IgG_level <= 140000 & TotalAntibody >= 1000) 


vaccinated <- dplyr::filter(tidydata, Vacc_status == "vaccinated")
unvaccinated <- dplyr::filter(tidydata, Vacc_status == "unvaccinated")
naturally_infected <- dplyr::filter(tidydata, Vacc_status == "nat_infected")


#====================== Checking Distribution of the Data (Shapiro-Wilk's normality test) ========================================================================================  

library(rstatix) 

dist_vacc_IgG <- vaccinated %>% shapiro_test(IgG_level)
dist_vacc_tot <- vaccinated %>% shapiro_test(TotalAntibody)

dist_unvacc_IgG <- unvaccinated %>% shapiro_test(IgG_level)
dist_unvacc_tot <- unvaccinated %>% shapiro_test(TotalAntibody) 

dist_nat_IgG <- naturally_infected %>% shapiro_test(IgG_level)
dist_nat_tot <- naturally_infected %>% shapiro_test(TotalAntibody) 


func <- function(x) {
  alpha <- 0.05 
  if(x$p > alpha){
    print("The sample has a Gaussian/normal distribution(Fail to reject the null hypothesis, the result is not significant)")
  } else {
    print("The sample does not have a Gaussian/normal distribution(Reject the null hypothesis, the result is significant)")
  }
  
} 

func(dist_vacc_IgG)  
func(dist_vacc_tot)  

func(dist_unvacc_IgG)  
func(dist_unvacc_tot) 

func(dist_nat_IgG)  
func(dist_nat_tot) 


# Distribution visualization  
library(ggpubr) 
ggqqplot(vaccinated$IgG_level) 
ggqqplot(vaccinated$TotalAntibody)

ggqqplot(unvaccinated$IgG_level) 
ggqqplot(unvaccinated$TotalAntibody) 

ggqqplot(naturally_infected$IgG_level) 
ggqqplot(naturally_infected$TotalAntibody) 


#==================================== TOTAL 3 GROUPS =============================================================================================================================  


# Kruskal-Wallis Test for IgG (Vacc + Unvacc + Nat_inf)  
tidydata %>% kruskal_test(IgG_level ~ Vacc_status) 

# Multiple pairwise-comparisons (Dunn's Test) between 3 groups 
tidydata %>% dunn_test(IgG_level ~ Vacc_status, p.adjust.method = "bonferroni") 

# IgG 

tidydata$Vacc_status <- factor(tidydata$Vacc_status, levels = c("vaccinated", "unvaccinated", "nat_infected")) 

IgG <- ggplot(tidydata, aes(x= Vacc_status, y= IgG_level, fill= Vacc_status)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#FFB61F", "#f75b5b", "#03B698"))+ 
  ylim(0, 160000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))
IgG 

ggsave(filename = "1) Vac, Unv, Nat (IgG).pdf", plot = IgG, width = 4.5, height = 3) 


# Kruskal-Wallis Test for Total Ab (Vacc + Unvacc + Nat_inf) 
tidydata %>% kruskal_test(TotalAntibody ~ Vacc_status) 

# Multiple pairwise-comparisons (Dunn's Test) between 3 groups
tidydata %>% dunn_test(TotalAntibody ~ Vacc_status, p.adjust.method = "bonferroni") 

# Totab Ab  

tidydata <- reorder_levels(tidydata, "Vacc_status", order = c("vaccinated", "unvaccinated", "nat_infected")) 

totalAb <- ggplot(tidydata, aes(x= Vacc_status, y= TotalAntibody, fill= Vacc_status))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#FFB61F", "#f75b5b", "#03B698"))+
  #ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
totalAb 

ggsave(filename = "1) Vac, Unv, Nat (Total).pdf", plot = totalAb, width = 4.5, height = 3) 



#==================================== Total Gender groups (Male, Female) =========================================================================================================  


# Wilcoxon signed rank test- (Unpaired due to missing Age information) 
wilcox.test(tidydata$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test/ Wilcoxon sum rank test for IgG (Comparison between 2 groups) 
tidydata %>% wilcox_test(IgG_level ~ Gender) 

# IgG 

IgG_gen <- ggplot(tidydata, aes(x= Gender, y= IgG_level, fill= Gender)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fb9300", "#4cbacc")) + 
  ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
IgG_gen 

ggsave(filename = "3) Male, Female (IgG).pdf", plot = IgG_gen, width = 3.5, height = 3) 



# Wilcoxon signed rank test- (Unpaired due to missing Age information) 
wilcox.test(tidydata$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
tidydata %>% wilcox_test(TotalAntibody ~ Gender) 

# Total Ab 

tot_gen <- ggplot(tidydata, aes(x= Gender, y= TotalAntibody, fill= Gender))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fb9300", "#4cbacc")) + 
  # ylim(0, 200000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
tot_gen 

ggsave(filename = "4) Male, Female (Total).pdf", plot = tot_gen, width = 3.5, height = 3) 



#==================================== Age Groups (<60, >=60) =====================================================================================================================  

ageALL <- tidydata %>% 
  mutate(Age_Status = if_else(Age >=60, "up60",
                      if_else(Age <60, "below60", NULL))) %>% 
  filter(Age_Status %in% c("up60", "below60")) 

# Spearman Correlation Coefficient test (p, r-value) 
cor.test(ageALL$IgG_level, ageALL$Age, method = "spearman", use = "na.or.complete") 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
ageALL %>% wilcox_test(IgG_level ~ Age_Status) 

# IgG 

IgG_age <- ggplot(ageALL, aes(x= Age_Status, y= IgG_level, fill= Age_Status))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fc4949", "#6cb440")) + 
  ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
IgG_age 

ggsave(filename = "5) Below50, Up60 (IgG).pdf", plot = IgG_age, width = 3.5, height = 3) 


# Spearman Correlation Coefficient test (p, r-value) 
cor.test(ageALL$TotalAntibody, ageALL$Age, method = "spearman", use = "na.or.complete") 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
ageALL %>% wilcox_test(TotalAntibody ~ Age_Status) 

# Total Ab 
tot_age <- ggplot(ageALL, aes(x= Age_Status, y= TotalAntibody, fill= Age_Status))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fc4949", "#6cb440"))+ 
  #ylim(0, 250000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
tot_age  

ggsave(filename = "6) Below50, Up60 (Total).pdf", plot = tot_age, width = 3.5, height = 3) 



#==================================== Comorbidities ==============================================================================================================================  


tidydata2 <- tidydata

# Sum of All comorbidities & Classify patients  
library(stringr) 

tidydata2$Diabetes = str_replace(tidydata2$Diabetes, "Diabetes", "1") 
tidydata2$Diabetes = str_replace(tidydata2$Diabetes, "No", "0")
tidydata2$Diabetes <- as.numeric(tidydata2$Diabetes)
class(tidydata2$Diabetes) 

tidydata2$Hypertension = str_replace(tidydata2$Hypertension, "Hypertension", "1") 
tidydata2$Hypertension = str_replace(tidydata2$Hypertension, "No", "0")
tidydata2$Hypertension <- as.numeric(tidydata2$Hypertension)
class(tidydata2$Hypertension) 

tidydata2$Heart_Disease = str_replace(tidydata2$Heart_Disease, "Heart Disease", "1") 
tidydata2$Heart_Disease = str_replace(tidydata2$Heart_Disease, "No", "0")
tidydata2$Heart_Disease <- as.numeric(tidydata2$Heart_Disease)
class(tidydata2$Heart_Disease) 

tidydata2$Kidney_Disease = str_replace(tidydata2$Kidney_Disease, "Kidney Disease", "1") 
tidydata2$Kidney_Disease = str_replace(tidydata2$Kidney_Disease, "No", "0")
tidydata2$Kidney_Disease <- as.numeric(tidydata2$Kidney_Disease)
class(tidydata2$Kidney_Disease) 

tidydata2$Cancer = str_replace(tidydata2$Cancer, "Cancer", "1") 
tidydata2$Cancer = str_replace(tidydata2$Cancer, "No", "0")
tidydata2$Cancer <- as.numeric(tidydata2$Cancer)
class(tidydata2$Cancer) 

tidydata2$Others = str_replace(tidydata2$Others, "Others", "1") 
tidydata2$Others = str_replace(tidydata2$Others, "No", "0")
tidydata2$Others <- as.numeric(tidydata2$Others)
class(tidydata2$Others) 

tidy_comor <- tidydata2 %>%  
  rowwise() %>% 
  mutate(Comor_Sum= sum(c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)))

tidy_comor <- tidy_comor %>% 
  mutate(Comor_name = ifelse(Comor_Sum==0, "none", 
                      ifelse(Comor_Sum==1, "one", 
                      ifelse(Comor_Sum==2, "two", 
                      ifelse(Comor_Sum==3, "three", 
                      ifelse(Comor_Sum==4, "four", 
                      ifelse(Comor_Sum==5, "five", 
                      ifelse(Comor_Sum==6, "six", "missing")))))))) 

# tidy_comor <- tidy_comor %>% dplyr::filter(!Comor_name == "none")  

tidy_comor$Comor_name <- factor(tidy_comor$Comor_name, levels = c("one", "two", "three", "four", "five", "six", "none"))


# Kruskal-Wallis Test for IgG (ALL = NO, 1, 2, 3, 4, 5, 6) 
tidy_comor %>% kruskal_test(IgG_level ~ Comor_name) 

# Multiple pairwise-comparisons for IgG (Dunn's Test) (NO, 1, 2, 3, 4, 5, 6)  
dunnTest(tidy_comor$IgG_level, tidy_comor$Comor_name, method = "bonferroni") 

# IgG (NO, 1, 2, 3, 4, 5, 6) 

IgG_com_allANDno <- ggplot(tidy_comor, aes(x= reorder(Comor_name, IgG_level, FUN=median), y= IgG_level, fill= Comor_name)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) + 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_com_allANDno 

ggsave(filename = "7) Comor 0-6 (IgG).pdf", plot = IgG_com_allANDno, width = 6.5, height = 3) 


# Kruskal-Wallis Test for Total Ab (ALL = NO, 1, 2, 3, 4, 5, 6) 
tidy_comor <- reorder_levels(tidy_comor, "Comor_name", order =  c("one", "two", "three", "four", "five", "six", "none")) 
tidy_comor %>% kruskal_test(TotalAntibody ~ Comor_name)

# Multiple pairwise-comparisons for Total Ab (Dunn's Test) (NO, 1, 2, 3, 4, 5, 6)  
dunnTest(tidy_comor$TotalAntibody, tidy_comor$Comor_name, method = "bonferroni") 

# Total Ab (NO, 1, 2, 3, 4, 5, 6) 

tot_com_allANDno <- ggplot(tidy_comor, aes(x= reorder(Comor_name, TotalAntibody, FUN= median), y= TotalAntibody, fill= Comor_name))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) + 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  #ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_com_allANDno 

ggsave(filename = "8) Comor 0-6 (Total).pdf", plot = tot_com_allANDno, width = 6.5, height = 3) 



# Comorbidity (Yes, No) 

tidy_comor_YN <- tidydata2 %>%  
  rowwise() %>%  
  mutate(Comor_Sum= sum(c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)))

tidy_comor_YN <- tidy_comor_YN %>% 
  mutate(Comor_name = ifelse(Comor_Sum==0, "none", 
                      ifelse(Comor_Sum==1, "comorbid", 
                      ifelse(Comor_Sum==2, "comorbid", 
                      ifelse(Comor_Sum==3, "comorbid", 
                      ifelse(Comor_Sum==4, "comorbid", 
                      ifelse(Comor_Sum==5, "comorbid", 
                      ifelse(Comor_Sum==6, "comorbid", "missing")))))))) 

tidy_comor_YN <- as.data.frame(tidy_comor_YN)

# Wilcoxon signed rank test- (Unpaired due to ANY missing information) 
wilcox.test(tidy_comor_YN$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test/ Wilcoxon sum rank test for IgG (Comparison between 2 groups) 
tidy_comor_YN %>% wilcox_test(IgG_level ~ Comor_name)  

# Wilcoxon signed rank test- (Unpaired due to ANY missing information) 
wilcox.test(tidy_comor_YN$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test/ Wilcoxon sum rank test for IgG (Comparison between 2 groups) 
tidy_comor_YN %>% wilcox_test(TotalAntibody ~ Comor_name)  


# IgG 

IgG_com_YN <- ggplot(tidy_comor_YN, aes(x= Comor_name, y= IgG_level, fill= Comor_name))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#99B9FF", "#FF94DB"))+ 
  ylim(0, 160000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_com_YN 

ggsave(filename = "9) Comor Yes-No (IgG).pdf", plot = IgG_com_YN, width = 3.5, height = 3) 


# Total antibody 

tot_com_YN <- ggplot(tidy_comor_YN, aes(x= Comor_name, y= TotalAntibody, fill= Comor_name))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#99B9FF", "#FF94DB"))+
  # ylim(0, 220000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_com_YN 

ggsave(filename = "10) Comor Yes-No (Total).pdf", plot = tot_com_YN, width = 3.5, height = 3) 



# Data tidying (All comorbidities in One Column)  

No_ALL <- dplyr::filter(tidydata, Diabetes== "No" & Hypertension== "No" & Heart_Disease== "No" & Kidney_Disease== "No" & Cancer== "No" & Others== "No") %>% 
  mutate(NoALL = Diabetes) %>%  
  select(-c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)) 

Diabetes_ALL <- dplyr::select(tidydata, -c(Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)) %>% 
  filter(Diabetes == "Diabetes") 

Hypertension_ALL <- dplyr::select(tidydata, -c(Diabetes, Heart_Disease, Kidney_Disease, Cancer, Others)) %>% 
  filter(Hypertension == "Hypertension")   

Heart_Disease_ALL <- dplyr::select(tidydata, -c(Diabetes, Hypertension, Kidney_Disease, Cancer, Others)) %>% 
  filter(Heart_Disease == "Heart Disease")  

Kidney_Disease_ALL <- dplyr::select(tidydata, -c(Diabetes, Hypertension, Heart_Disease, Cancer, Others)) %>% 
  filter(Kidney_Disease == "Kidney Disease")  

Cancer_ALL <- dplyr::select(tidydata, -c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Others)) %>% 
  filter(Cancer == "Cancer") 

Others_ALL <- dplyr::select(tidydata, -c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer)) %>% 
  filter(Others == "Others") 

comorALL_ALL <- bind_rows(Diabetes_ALL, Hypertension_ALL) %>% 
  bind_rows(Heart_Disease_ALL) %>%  
  bind_rows(Kidney_Disease_ALL) %>% 
  bind_rows(Cancer_ALL) %>% 
  bind_rows(Others_ALL) %>% 
  bind_rows(No_ALL) %>%  
  mutate(Age_Status = if_else(Age >=60, "up60",
                      if_else(Age <60, "below60", NULL))) %>% 
  filter(Age_Status %in% c("up60", "below60")) 

library(tidyr)
comorInONE_ALL <- comorALL_ALL %>%  
  unite("Comorbidities_All", c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others, NoALL), na.rm = TRUE)

# comorInONE_ALL <- comorInONE_ALL %>% dplyr::filter(!Comorbidities_All == "No")  


comorInONE_ALL$Comorbidities_All <- factor(comorInONE_ALL$Comorbidities_All, levels = c("Diabetes", "Hypertension", "Heart Disease", "Kidney Disease", "Cancer", "Others", "No"))


# Kruskal-Wallis Test for IgG 
comorInONE_ALL %>% kruskal_test(IgG_level ~ Comorbidities_All)

# Multiple pairwise-comparisons for IgG (Dunn's Test)  
saveIgG <- dunnTest(comorInONE_ALL$IgG_level, comorInONE_ALL$Comorbidities_All, method = "bonferroni") 
saveIgG[["res"]] 
write.csv(saveIgG[["res"]], file = "Comorbid IgG p-value.csv")

# IgG 

IgG_comALL <- ggplot(comorInONE_ALL, aes(x= reorder(Comorbidities_All, IgG_level, FUN=median), y= IgG_level, fill= Comorbidities_All))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_comALL 

ggsave(filename = "11) Comor ALL Names (IgG).pdf", plot = IgG_comALL, width = 7, height = 3) 


# Kruskal-Wallis Test for Total Ab 
comorInONE_ALL %>% kruskal_test(TotalAntibody ~ Comorbidities_All)

# Multiple pairwise-comparisons for Total Ab 
saveTot <- dunnTest(comorInONE_ALL$TotalAntibody, comorInONE_ALL$Comorbidities_All, method = "bonferroni") 
saveTot[["res"]] 
write.csv(saveTot[["res"]], file = "Comorbid TAb p-value.csv")

# Total Ab 

tot_comALL <- ggplot(comorInONE_ALL, aes(x= reorder(Comorbidities_All, TotalAntibody, FUN= median), y= TotalAntibody, fill= Comorbidities_All))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  #ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_comALL 

ggsave(filename = "12) Comor ALL Names (Total).pdf", plot = tot_comALL, width = 7, height = 3) 



#==================================== Occupation ================================================================================================================================== 

# Kruskal-Wallis Test for IgG 
tidydata %>% kruskal_test(IgG_level ~ Occupation)

# Multiple pairwise-comparisons for IgG 
dunnTest(tidydata$IgG_level, tidydata$Occupation, method = "bonferroni") 

# IgG 

IgG_occ <- ggplot(tidydata, aes(x= reorder(Occupation, IgG_level, FUN= median), y= IgG_level, fill= Occupation))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_occ

ggsave(filename = "13) Occupation (IgG).pdf", plot = IgG_occ, width = 6.5, height = 3) 



# Kruskal-Wallis Test for Total Ab 
tidydata %>% kruskal_test(TotalAntibody ~ Occupation)

# Multiple pairwise-comparisons for Total Ab 
dunnTest(tidydata$TotalAntibody, tidydata$Occupation, method = "bonferroni") 

# Total Ab 

tot_occ <- ggplot(tidydata, aes(x= reorder(Occupation, TotalAntibody, FUN= median), y= TotalAntibody, fill= Occupation))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  #ylim(0, 200000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
tot_occ 

ggsave(filename = "14) Occupation (Total).pdf", plot = tot_occ, width = 6.5, height = 3) 



scale_fill_manual(values=c("#F9C80E", "#F86624", "#EA3546", "#00F5D4" , "#00BBF9", "#662E9B", "#ddd8c4")) 



#==================================== Descriptive Statistics =====================================================================================================================

library(Rmisc) 

# All 
summary(vaccinated$IgG_level)
summary(vaccinated$TotalAntibody) 
CI(vaccinated$IgG_level, ci=0.95)
CI(vaccinated$TotalAntibody, ci=0.95)

summary(unvaccinated$IgG_level)
summary(unvaccinated$TotalAntibody) 
CI(unvaccinated$IgG_level, ci=0.95)
CI(unvaccinated$TotalAntibody, ci=0.95)


summary(naturally_infected$IgG_level)
summary(naturally_infected$TotalAntibody)
CI(naturally_infected$IgG_level, ci=0.95)
CI(naturally_infected$TotalAntibody, ci=0.95)

# Age 
ageALL %>% count("Age_Status") 

below60 <- ageALL %>% 
  filter(Age_Status %in% c("below60")) 

up60 <- ageALL %>% 
  filter(Age_Status %in% c("up60")) 

summary(below60$Age)
CI(below60$Age, ci=0.95)

summary(below60$IgG_level)
CI(below60$IgG_level, ci=0.95)

summary(below60$TotalAntibody)
CI(below60$TotalAntibody, ci=0.95)

summary(up60$Age)
CI(up60$Age, ci=0.95)

summary(up60$IgG_level)
CI(up60$IgG_level, ci=0.95)

summary(up60$TotalAntibody)
CI(up60$TotalAntibody, ci=0.95)

# Gender 
tidydata %>% count("Gender") 

male <- tidydata %>%  
  filter(Gender == "Male")

female <- tidydata %>%  
  filter(Gender == "Female") 

summary(male$IgG_level)
CI(male$IgG_level, ci=0.95)

summary(male$TotalAntibody)
CI(male$TotalAntibody, ci=0.95)

summary(female$IgG_level)
CI(female$IgG_level, ci=0.95)

summary(female$TotalAntibody)
CI(female$TotalAntibody, ci=0.95)

# Comor (Dia, Hyp etc) 
comorInONE_ALL %>% count("Comorbidities_All")

summary(Diabetes_ALL$IgG_level)
CI(Diabetes_ALL$IgG_level, ci=0.95)

summary(Diabetes_ALL$TotalAntibody)
CI(Diabetes_ALL$TotalAntibody, ci=0.95)


summary(Hypertension_ALL$IgG_level)
CI(Hypertension_ALL$IgG_level, ci=0.95)

summary(Hypertension_ALL$TotalAntibody)
CI(Hypertension_ALL$TotalAntibody, ci=0.95) 


summary(Heart_Disease_ALL$IgG_level)
CI(Heart_Disease_ALL$IgG_level, ci=0.95)

summary(Heart_Disease_ALL$TotalAntibody)
CI(Heart_Disease_ALL$TotalAntibody, ci=0.95)


summary(Kidney_Disease_ALL$IgG_level)
CI(Kidney_Disease_ALL$IgG_level, ci=0.95)

summary(Kidney_Disease_ALL$TotalAntibody)
CI(Kidney_Disease_ALL$TotalAntibody, ci=0.95) 


summary(Cancer_ALL$IgG_level)
CI(Cancer_ALL$IgG_level, ci=0.95)

summary(Cancer_ALL$TotalAntibody)
CI(Cancer_ALL$TotalAntibody, ci=0.95) 


summary(Others_ALL$IgG_level)
CI(Others_ALL$IgG_level, ci=0.95)

summary(Others_ALL$TotalAntibody)
CI(Others_ALL$TotalAntibody, ci=0.95) 


summary(No_ALL$IgG_level)
CI(No_ALL$IgG_level, ci=0.95)

summary(No_ALL$TotalAntibody)
CI(No_ALL$TotalAntibody, ci=0.95)

Diabetes_ALL 
Hypertension_ALL 
Heart_Disease_ALL 
Kidney_Disease_ALL 
Cancer_ALL 
Others_ALL 

# Comor (1, 2, 3 etc)   
oneC <- tidy_comor %>% filter(Comor_name == "one")
twoC <- tidy_comor %>% filter(Comor_name == "two") 
threeC <- tidy_comor %>% filter(Comor_name == "three") 
fourC <- tidy_comor %>% filter(Comor_name == "four") 
fiveC <- tidy_comor %>% filter(Comor_name == "five") 

summary(oneC$IgG_level)
CI(oneC$IgG_level, ci=0.95)

summary(oneC$TotalAntibody)
CI(oneC$TotalAntibody, ci=0.95)  


summary(twoC$IgG_level)
CI(twoC$IgG_level, ci=0.95)

summary(twoC$TotalAntibody)
CI(twoC$TotalAntibody, ci=0.95) 


summary(threeC$IgG_level)
CI(threeC$IgG_level, ci=0.95)

summary(threeC$TotalAntibody)
CI(threeC$TotalAntibody, ci=0.95) 


summary(fourC$IgG_level)
CI(na.omit(fourC$IgG_level), ci=0.95)

summary(fourC$TotalAntibody)
CI(fourC$TotalAntibody, ci=0.95) 


summary(fiveC$IgG_level)
CI(na.omit(fiveC$IgG_level), ci=0.95)

summary(fiveC$TotalAntibody)
CI(fiveC$TotalAntibody, ci=0.95) 
