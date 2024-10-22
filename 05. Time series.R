setwd("E:\\Mousumi Ma'am (Project-1)\\5) Vac Time Series - Review") 

library("readxl") 
library(dplyr)
library(ggplot2) 
library(ggpubr) 
library(rstatix)


#===================== Main Data & Classifying it into 3 seperate groups =======================================================================================

all <- read_excel("Sheet1.xlsx") 

# Data tidying for Boxplot with Jitter (Vacc, Unvacc, Nat_infected) 

tidydata <- all %>% 
  mutate(Vacc_status = if_else(Vaccinated == "Yes", "vaccinated",
                       if_else(Vaccinated == "No" & Covid_event_1 == "No" & Covid_event_2 == "No", "unvaccinated", 
                       if_else(Vaccinated == "No" & Covid_event_1 == "Yes" | Vaccinated == "No" & Covid_event_2 == "Yes", "nat_infected", NULL)))) %>%  
  filter(Vacc_status %in% c("vaccinated", "unvaccinated", "nat_infected")) 

tidydata <- tidydata %>% dplyr::filter(IgG_level >= 1000 & IgG_level <= 140000 & TotalAntibody >= 1000) 



#===================== Date Conversion and Calculation =========================================================================================================

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


# Classification into 3 major groups  
vaccinated <- dplyr::filter(tidydata, Vacc_status == "vaccinated") %>%  
  filter(int_Dose2_Ab %in% c(0:50))   

unvaccinated <- dplyr::filter(tidydata, Vacc_status == "unvaccinated")
naturally_infected <- dplyr::filter(tidydata, Vacc_status == "nat_infected")


#===================== Vaccinated ==============================================================================================================================


# Separation Based on Months 

vacc_sep <- vaccinated %>% 
  filter(int_Dose2_Ab %in% c(0:50)) %>%  
  mutate(TimeClass = if_else(int_Dose2_Ab == 0, "Zero", 
                     if_else(int_Dose2_Ab == 1, "One",
                     if_else(int_Dose2_Ab == 2, "Two",
                     if_else(int_Dose2_Ab == 3, "Three",
                     if_else(int_Dose2_Ab == 4, "Four", 
                     if_else(int_Dose2_Ab == 5, "Five", 
                     if_else(int_Dose2_Ab == 6, "Six", 
                     if_else(int_Dose2_Ab == 7, "Seven",
                     if_else(int_Dose2_Ab == 8, "Eight",
                     if_else(int_Dose2_Ab == 9, "Nine", "Twelve"))))))))))) 

vacc_sep$TimeClass <- factor(vacc_sep$TimeClass, levels = c("Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight"))

vacc_sep <- vacc_sep %>% dplyr::filter(TimeClass %in% c("Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight"))
vacc_sep %>% count(TimeClass) 

write.csv(vacc_sep, "Vaccinated Time-Class.csv")


# Spearman Correlation Coefficient test (p, r-value) 
vacc_sep$int_Dose2_Ab <- as.numeric(vacc_sep$int_Dose2_Ab) 
cor.test(vacc_sep$IgG_level, vacc_sep$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# IgG (Scatter plot)
IgGtime <- ggplot(vacc_sep, aes(x= int_Dose2_Ab, y= IgG_level)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#ffa500"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  xlim(0, 8) + 
  ylim(0, 150000) 

IgGtime 
ggsave(filename = "1) IgG (Vac ALL).pdf", plot = IgGtime, width = 3.75, height = 3.5) 


# Mann Whiteney U Test for IgG (Comparison between 6 months) 

dput(vacc_sep$IgG_level) 
dput(vacc_sep$TimeClass)
class(vacc_sep$IgG_level)
class(vacc_sep$TimeClass) 

READ_IgGtime <- vacc_sep %>% wilcox_test(IgG_level ~ TimeClass) 
READ_IgGtime 
write.csv(READ_IgGtime, "Vacc Time Series (IgG).csv")

# IgG (Boxplot) 
IgGtime_sep <- ggplot(vacc_sep, aes(x = TimeClass, y = IgG_level, fill = TimeClass)) + 
  geom_point(position = position_jitter(width = 0.05), alpha = 0.1, size = 2.5, pch = 21)+
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.3, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_classic() + 
  ylim(-100, 210000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) 

IgGtime_sep 
ggsave(filename = "3) IgG Time Class (Vac ALL).pdf", plot = IgGtime_sep, width = 10, height = 4) 


# Total Ab - Spearman Correlation Coefficient test (p, r-value)  
vacc_sep$int_Dose2_Ab <- as.numeric(vacc_sep$int_Dose2_Ab) 
cor.test(vacc_sep$TotalAntibody, vacc_sep$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# Total Ab (Scatter plot)
toTtime <- ggplot(vacc_sep, aes(x= int_Dose2_Ab, y= TotalAntibody)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#ffa500"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  xlim(0, 8) +  
  ylim(0, 200000) 

toTtime 
ggsave(filename = "2) Total (Vac ALL).pdf", plot = toTtime, width = 3.75, height = 3.5) 


# Mann Whiteney U Test for Total Ab (Comparison between 6 months) 

dput(vacc_sep$TotalAntibody) 
dput(vacc_sep$TimeClass)
class(vacc_sep$TotalAntibody)
class(vacc_sep$TimeClass) 

READ_toTtime <- vacc_sep %>% wilcox_test(TotalAntibody ~ TimeClass) 
READ_toTtime 
write.csv(READ_toTtime, "Vacc Time Series (Total).csv")

# Total Ab (Box plot)
tottime_sep <- ggplot(vacc_sep, aes(x= TimeClass, y= TotalAntibody, fill=TimeClass)) +
  geom_point(position=position_jitter(width = 0.05), alpha=0.1, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.3, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_classic() + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  ylim(-100, 250000)  

tottime_sep 
ggsave(filename = "4) Total Time Class (Vac ALL).pdf", plot = tottime_sep, width = 10, height = 4) 


#===================== Classification of Vaccinated Group ======================================================================================================

# Classify vaccinated class 

tidyVAC <- vacc_sep %>% 
  mutate(VaccINTO_status = if_else(int_Dose1_pos %in% c(0:50) & Covid_event_1 == "Yes" | int_Dose1_pos %in% c(0:50) & Covid_event_2 == "Yes" , "pre_inf",
                           if_else(int_Dose1_pos %in% c(-1:-50) & Covid_event_1 == "Yes" | int_Dose1_pos %in% c(-1:-50) & Covid_event_2 == "Yes" , "post_inf", 
                           if_else(Covid_event_1 == "No" & Covid_event_2 == "No", "not_inf", NULL)))) %>%  
  filter(VaccINTO_status %in% c("pre_inf", "post_inf", "not_inf")) 

pre_inf <- dplyr::filter(tidyVAC, VaccINTO_status == "pre_inf")
post_inf <- dplyr::filter(tidyVAC, VaccINTO_status == "post_inf")
not_inf <- dplyr::filter(tidyVAC, VaccINTO_status == "not_inf") 


tidyVAC$VaccINTO_status <- factor(tidyVAC$VaccINTO_status, levels = c("pre_inf", "post_inf", "not_inf"))


IgGclass <- ggplot(tidyVAC, aes(x = int_Dose2_Ab, y = IgG_level)) + 
  geom_point(position = position_jitter(width = 0.5), color = "black", aes(fill = VaccINTO_status), pch = 21, size = 1.5) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  scale_fill_manual(values = c("#F05960", "#33BEB3", "#77C252")) + 
  ylim(0, 150000) + 
  xlim(0, 8) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  facet_wrap(~ VaccINTO_status, nrow = 1, scales= "free")  

IgGclass 
ggsave(filename = "5) Pre, Post, No (IgG).pdf", plot = IgGclass, width = 7.5, height = 2.20, units = "in")


toTclass <- ggplot(tidyVAC, aes(x = int_Dose2_Ab, y = TotalAntibody)) + 
  geom_point(position = position_jitter(width = 0.5), color = "black", aes(fill = VaccINTO_status), pch = 21, size = 1.5) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  scale_fill_manual(values = c("#F05960", "#33BEB3", "#77C252")) + 
  ylim(0, 200000) +
  xlim(0, 8) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  facet_wrap(~ VaccINTO_status, nrow = 1, scales= "free")  

toTclass 
ggsave(filename = "6) Pre, Post, No (Total).pdf", plot = toTclass, width = 7.5, height = 2.20, units = "in")


#===================== Pre-Infected ============================================================================================================================

# Spearman Correlation Coefficient test (p, r-value) 
pre_inf$int_Dose2_Ab <- as.numeric(pre_inf$int_Dose2_Ab) 
cor.test(pre_inf$IgG_level, pre_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# IgG 
pre_IgG <- ggplot(pre_inf, aes(x= int_Dose2_Ab, y= IgG_level)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#ff595e"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  xlim(0, 8) + 
  ylim(0, 200000) 

pre_IgG 

# Spearman Correlation Coefficient test (p, r-value)  
pre_inf$int_Dose2_Ab <- as.numeric(pre_inf$int_Dose2_Ab) 
cor.test(pre_inf$TotalAntibody, pre_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# Total Ab 
pre_tot <- ggplot(pre_inf, aes(x= int_Dose2_Ab, y= TotalAntibody)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#ff595e"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  ylim(0, 200000) + 
  xlim(0, 8) 

pre_tot 



#===================== Post-Infected ===========================================================================================================================

# Spearman Correlation Coefficient test (p, r-value) 
post_inf$int_Dose2_Ab <- as.numeric(post_inf$int_Dose2_Ab) 
cor.test(post_inf$IgG_level, post_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# IgG 
post_IgG <- ggplot(post_inf, aes(x= int_Dose2_Ab, y= IgG_level)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#2ec4b6"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  ylim(0, 200000) + 
  xlim(0, 8) 

post_IgG 

# Spearman Correlation Coefficient test (p, r-value)  
post_inf$int_Dose2_Ab <- as.numeric(post_inf$int_Dose2_Ab) 
cor.test(post_inf$TotalAntibody, post_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# Total Ab 
post_tot <- ggplot(post_inf, aes(x= int_Dose2_Ab, y= TotalAntibody)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#2ec4b6"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  ylim(0, 200000) + 
  xlim(0, 8) 

post_tot 


# Minus values need to be corrected manually, because curation of "Covid(+) Date > Ab count date.  


#===================== Not-Infected ===========================================================================================================================

# Spearman Correlation Coefficient test (p, r-value) 
not_inf$int_Dose2_Ab <- as.numeric(not_inf$int_Dose2_Ab) 
cor.test(not_inf$IgG_level, not_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# IgG 
not_IgG <- ggplot(not_inf, aes(x= int_Dose2_Ab, y= IgG_level)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#72cc50"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  ylim(0, 200000) + 
  xlim(0, 8) 

not_IgG 

# Spearman Correlation Coefficient test (p, r-value)  
not_inf$int_Dose2_Ab <- as.numeric(not_inf$int_Dose2_Ab) 
cor.test(not_inf$TotalAntibody, not_inf$int_Dose2_Ab, method = "spearman", use = "na.or.complete") 

# Total Ab 
not_tot <- ggplot(not_inf, aes(x= int_Dose2_Ab, y= TotalAntibody)) + 
  geom_point(position=position_jitter(width = 0.5), color = "black", fill = c("#72cc50"), pch=21, size=2) + 
  stat_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  ylim(0, 200000) + 
  xlim(0, 8) 

not_tot 

#===================== Violin boxplot (For Median) =============================================================================================================

library(FSA) 

# IgG 
tidyVAC %>% kruskal_test(IgG_level ~ VaccINTO_status)
dunnTest(tidyVAC$IgG_level, tidyVAC$VaccINTO_status, method = "bonferroni") 

pIgG <- ggplot(tidyVAC, aes(x = reorder(VaccINTO_status, IgG_level, FUN= median), y = IgG_level, fill = VaccINTO_status)) + 
  geom_violin(trim = FALSE, alpha = 0.5, draw_quantiles = c(0.5), position = position_dodge(1)) + 
  stat_boxplot(geom = 'errorbar', width = 0.1) + 
  geom_boxplot(width = 0.1, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  scale_fill_manual(values=c("#F05960", "#33BEB3", "#77C252")) +
  theme_bw() 
pIgG
ggsave(filename = "7) Group Comparison (IgG).pdf", plot = pIgG, width = 5, height = 2) 


# Total antibody 
tidyVAC %>% kruskal_test(TotalAntibody ~ VaccINTO_status)
dunnTest(tidyVAC$TotalAntibody, tidyVAC$VaccINTO_status, method = "bonferroni") 

pTot <- ggplot(tidyVAC, aes(x = reorder(VaccINTO_status, TotalAntibody, FUN= median), y = TotalAntibody, fill = VaccINTO_status)) + 
  geom_violin(trim = FALSE, alpha = 0.5, draw_quantiles = c(0.5), position = position_dodge(1)) + 
  stat_boxplot(geom = 'errorbar', width = 0.1) + 
  geom_boxplot(width = 0.1, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  scale_fill_manual(values=c("#F05960", "#33BEB3", "#77C252")) + 
  theme_bw() 
pTot
ggsave(filename = "8) Group Comparison (Total).pdf", plot = pTot, width = 5, height = 2) 


#===================== Descriptive Statistics ==================================================================================================================

library(Rmisc)
summary(vacc_sep$Age)
CI(na.omit(vacc_sep$Age), ci=0.95) 

# Pre 
pre_inf 
summary(pre_inf$int_Dose2_Ab)
CI(na.omit(pre_inf$int_Dose2_Ab), ci=0.95) 

summary(pre_inf$IgG_level)
CI(na.omit(pre_inf$IgG_level), ci=0.95)
summary(pre_inf$TotalAntibody)
CI(na.omit(pre_inf$TotalAntibody), ci=0.95) 

# Post 
post_inf
summary(post_inf$int_Dose2_Ab)
CI(na.omit(post_inf$int_Dose2_Ab), ci=0.95) 

summary(post_inf$IgG_level)
CI(na.omit(post_inf$IgG_level), ci=0.95)
summary(post_inf$TotalAntibody)
CI(na.omit(post_inf$TotalAntibody), ci=0.95) 

# Not 
not_inf 
summary(not_inf$int_Dose2_Ab)
CI(na.omit(not_inf$int_Dose2_Ab), ci=0.95) 

summary(not_inf$IgG_level)
CI(na.omit(not_inf$IgG_level), ci=0.95)
summary(not_inf$TotalAntibody)
CI(na.omit(not_inf$TotalAntibody), ci=0.95) 


# Median, IQR, 95% CI IgG and TAb 
Zero <- vacc_sep %>% filter(TimeClass == "Zero")  
One <- vacc_sep %>% filter(TimeClass == "One")
Two <- vacc_sep %>% filter(TimeClass == "Two")
Three <- vacc_sep %>% filter(TimeClass == "Three")
Four <- vacc_sep %>% filter(TimeClass == "Four")
Five <- vacc_sep %>% filter(TimeClass == "Five")
Six <- vacc_sep %>% filter(TimeClass == "Six")
Seven <- vacc_sep %>% filter(TimeClass == "Seven")
Eight <- vacc_sep %>% filter(TimeClass == "Eight")
Nine <- vacc_sep %>% filter(TimeClass == "Nine")
Twelve <- vacc_sep %>% filter(TimeClass == "Twelve")


# IgG  

summary(Zero$IgG_level)
CI(na.omit(Zero$IgG_level), ci=0.95) 

summary(One$IgG_level)
CI(na.omit(One$IgG_level), ci=0.95) 

summary(Two$IgG_level)
CI(na.omit(Two$IgG_level), ci=0.95) 

summary(Three$IgG_level)
CI(na.omit(Three$IgG_level), ci=0.95)

summary(Four$IgG_level)
CI(na.omit(Four$IgG_level), ci=0.95)

summary(Five$IgG_level)
CI(na.omit(Five$IgG_level), ci=0.95)

summary(Six$IgG_level)
CI(na.omit(Six$IgG_level), ci=0.95)

summary(Seven$IgG_level)
CI(na.omit(Seven$IgG_level), ci=0.95)

summary(Eight$IgG_level)
CI(na.omit(Eight$IgG_level), ci=0.95)

summary(Nine$IgG_level)
CI(na.omit(Nine$IgG_level), ci=0.95)

summary(Twelve$IgG_level)
CI(na.omit(Twelve$IgG_level), ci=0.95)

# Total antibody 

summary(Zero$TotalAntibody)
CI(na.omit(Zero$TotalAntibody), ci=0.95) 

summary(One$TotalAntibody)
CI(na.omit(One$TotalAntibody), ci=0.95) 

summary(Two$TotalAntibody)
CI(na.omit(Two$TotalAntibody), ci=0.95) 

summary(Three$TotalAntibody)
CI(na.omit(Three$TotalAntibody), ci=0.95)

summary(Four$TotalAntibody)
CI(na.omit(Four$TotalAntibody), ci=0.95)

summary(Five$TotalAntibody)
CI(na.omit(Five$TotalAntibody), ci=0.95)

summary(Six$TotalAntibody)
CI(na.omit(Six$TotalAntibody), ci=0.95)

summary(Seven$TotalAntibody)
CI(na.omit(Seven$TotalAntibody), ci=0.95)

summary(Eight$TotalAntibody)
CI(na.omit(Eight$TotalAntibody), ci=0.95)

summary(Nine$TotalAntibody)
CI(na.omit(Nine$TotalAntibody), ci=0.95)

summary(Twelve$TotalAntibody)
CI(na.omit(Twelve$TotalAntibody), ci=0.95) 
