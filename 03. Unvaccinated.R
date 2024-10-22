setwd("E:\\Mousumi Ma'am (Project-1)\\3) Unvaccinated - Review") 

library("readxl") 
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(ggpubr) 
library(rstatix)
library(FSA) 
library(forcats)


#===================== Main Data & Classifying it into 3 seperate groups ===================================================================================================

all <- read_excel("Sheet1.xlsx") 

# Data tidying for Boxplot with Jitter (Vacc, Unvacc, Nat_infected)  

tidydata <- all %>% 
  mutate(Vacc_status = if_else(Vaccinated == "Yes", "vaccinated",
                       if_else(Vaccinated == "No" & Covid_event_1 == "No" & Covid_event_2 == "No", "unvaccinated", 
                       if_else(Vaccinated == "No" & Covid_event_1 == "Yes" | Vaccinated == "No" & Covid_event_2 == "Yes", "nat_infected", NULL)))) %>%  
  filter(Vacc_status %in% c("vaccinated", "unvaccinated", "nat_infected")) 

tidydata <- tidydata %>% dplyr::filter(IgG_level >= 1000 & IgG_level <= 140000 & TotalAntibody >= 1000) 

vaccinated <- dplyr::filter(tidydata, Vacc_status == "unvaccinated") 


#====================== Function =========================================================================================================================================== 

# Split Violin Plot 

alpha <- scales::alpha

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

dodge <- position_dodge(width=.5)  


# Raincloud Plot 
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            data %>%
              group_by(group) %>%
              mutate(
                ymin = min(y),
                ymax = max(y),
                xmin = x,
                xmax = x + width / 2
              )
          },
          
          draw_group = function(data, panel_scales, coord) {
            data <- transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)
            )
            newdata <- rbind(
              plyr::arrange(transform(data, x = xminv), y),
              plyr::arrange(transform(data, x = xmaxv), -y)
            )
            newdata <- rbind(newdata, newdata[1, ])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(
            weight = 1, colour = "grey20", fill = "white", size = 0.5,
            alpha = NA, linetype = "solid"
          ),
          
          required_aes = c("x", "y")
  )

#==================================== Total Gender groups (Male, Female) ===================================================================================================

com_gender <- list(c("Male", "Female")) 


# Wilcoxon signed rank test- (Unpaired due to missing Age information) 
wilcox.test(vaccinated$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

IgGwill_M <- vaccinated %>% filter(Gender== "Male")
wilcox.test(IgGwill_M$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

IgGwill_F <- vaccinated %>% filter(Gender== "Female")
wilcox.test(IgGwill_F$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test/ Wilcoxon sum rank test for IgG (Comparison between 2 groups) 
vaccinated %>% wilcox_test(IgG_level ~ Gender) 


# IgG (Male, Female) 

IgG_gen <- ggplot(vaccinated, aes(x= Gender, y= IgG_level, fill= Gender)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fb9300", "#4cbacc")) + 
  ylim(0, 20000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 
  
IgG_gen 

ggsave(filename = "1) Male, Female (IgG).pdf", plot = IgG_gen, width = 3.5, height = 3) 


# Wilcoxon signed rank test- (Unpaired due to missing Age information) 
wilcox.test(vaccinated$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

totwill_M <- vaccinated %>% filter(Gender== "Male")
wilcox.test(totwill_M$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

torwill_F <- vaccinated %>% filter(Gender== "Female")
wilcox.test(torwill_F$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
vaccinated %>% wilcox_test(TotalAntibody ~ Gender) 


# Total Ab 

tot_gen <- ggplot(vaccinated, aes(x= Gender, y= TotalAntibody, fill= Gender))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fb9300", "#4cbacc"))+ 
  ylim(0, 50000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_gen 

ggsave(filename = "2) Male, Female (Total).pdf", plot = tot_gen, width = 3.5, height = 3) 



#==================================== Age Groups (<60, >=60) ===============================================================================================================

ageALL <- vaccinated %>% 
  mutate(Age_Status = if_else(Age >=60, "up60",
                      if_else(Age <60, "below60", NULL))) %>% 
  filter(Age_Status %in% c("up60", "below60")) 


# Spearman Correlation Coefficient test (p, r-value) 
cor.test(ageALL$IgG_level, ageALL$Age, method = "spearman", use = "na.or.complete") 

b60IgG <- ageALL %>% filter(Age_Status== "below60") 
cor.test(b60IgG$IgG_level, b60IgG$Age, method = "spearman", use = "na.or.complete")

u60IgG <- ageALL %>% filter(Age_Status== "up60") 
cor.test(u60IgG$IgG_level, u60IgG$Age, method = "spearman", use = "na.or.complete") 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
ageALL %>% wilcox_test(IgG_level ~ Age_Status) 


# IgG (<60, >=60) 

IgG_age <- ggplot(ageALL, aes(x= Age_Status, y= IgG_level, fill= Age_Status))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+ 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fc4949", "#6cb440"))+ 
  ylim(0, 20000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_age 

ggsave(filename = "3) Below50, Up60 (IgG).pdf", plot = IgG_age, width = 3.5, height = 3) 


# Spearman Correlation Coefficient test (p, r-value) 
cor.test(ageALL$TotalAntibody, ageALL$Age, method = "spearman", use = "na.or.complete") 

b60tot <- ageALL %>% filter(Age_Status== "below60") 
cor.test(b60tot$TotalAntibody, b60tot$Age, method = "spearman", use = "na.or.complete")

u60tot <- ageALL %>% filter(Age_Status== "up60") 
cor.test(u60tot$TotalAntibody, u60tot$Age, method = "spearman", use = "na.or.complete") 

# Mann Whiteney U Test for IgG (Comparison between 2 groups) 
ageALL %>% wilcox_test(TotalAntibody ~ Age_Status) 


# Total Ab (<60, >=60) 
tot_age <- ggplot(ageALL, aes(x= Age_Status, y= TotalAntibody, fill= Age_Status))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21)+
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#fc4949", "#6cb440"))+ 
  ylim(0, 50000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_age   

ggsave(filename = "4) Below50, Up60 (Total).pdf", plot = tot_age, width = 3.5, height = 3) 



#==================================== Comorbidities ========================================================================================================================


vaccinated_copy <- vaccinated 

# Sum of All comorbidities & Classify patients  
library(stringr) 

vaccinated_copy$Diabetes = str_replace(vaccinated_copy$Diabetes, "Diabetes", "1") 
vaccinated_copy$Diabetes = str_replace(vaccinated_copy$Diabetes, "No", "0")
vaccinated_copy$Diabetes <- as.numeric(vaccinated_copy$Diabetes)
class(vaccinated_copy$Diabetes) 

vaccinated_copy$Hypertension = str_replace(vaccinated_copy$Hypertension, "Hypertension", "1") 
vaccinated_copy$Hypertension = str_replace(vaccinated_copy$Hypertension, "No", "0")
vaccinated_copy$Hypertension <- as.numeric(vaccinated_copy$Hypertension)
class(vaccinated_copy$Hypertension) 

vaccinated_copy$Heart_Disease = str_replace(vaccinated_copy$Heart_Disease, "Heart Disease", "1") 
vaccinated_copy$Heart_Disease = str_replace(vaccinated_copy$Heart_Disease, "No", "0")
vaccinated_copy$Heart_Disease <- as.numeric(vaccinated_copy$Heart_Disease)
class(vaccinated_copy$Heart_Disease) 

vaccinated_copy$Kidney_Disease = str_replace(vaccinated_copy$Kidney_Disease, "Kidney Disease", "1") 
vaccinated_copy$Kidney_Disease = str_replace(vaccinated_copy$Kidney_Disease, "No", "0")
vaccinated_copy$Kidney_Disease <- as.numeric(vaccinated_copy$Kidney_Disease)
class(vaccinated_copy$Kidney_Disease) 

vaccinated_copy$Cancer = str_replace(vaccinated_copy$Cancer, "Cancer", "1") 
vaccinated_copy$Cancer = str_replace(vaccinated_copy$Cancer, "No", "0")
vaccinated_copy$Cancer <- as.numeric(vaccinated_copy$Cancer)
class(vaccinated_copy$Cancer) 

vaccinated_copy$Others = str_replace(vaccinated_copy$Others, "Others", "1") 
vaccinated_copy$Others = str_replace(vaccinated_copy$Others, "No", "0")
vaccinated_copy$Others <- as.numeric(vaccinated_copy$Others)
class(vaccinated_copy$Others) 

tidy_comor <- vaccinated_copy %>%  
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

# Wilcoxon signed rank test IgG (Individually) - Unpaired due to missing information 
oneC <- tidy_comor %>% filter(Comor_name == "one")
wilcox.test(oneC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

twoC <- tidy_comor %>% filter(Comor_name == "two")
wilcox.test(twoC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

threeC <- tidy_comor %>% filter(Comor_name == "three")
wilcox.test(threeC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

fourC <- tidy_comor %>% filter(Comor_name == "four")
wilcox.test(fourC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE)

fiveC <- tidy_comor %>% filter(Comor_name == "five")
wilcox.test(fiveC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE)

noneC <- tidy_comor %>% filter(Comor_name == "none")
wilcox.test(noneC$IgG_level, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 


# IgG (NO, 1, 2, 3, 4, 5, 6) 

tidy_comor <- reorder_levels(tidy_comor, "Comor_name", order =  c("one", "two", "three", "four", "five", "six", "none")) 

IgG_com_allANDno <- ggplot(tidy_comor, aes(x= reorder(Comor_name, IgG_level, FUN=median), y= IgG_level, fill= Comor_name)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) + 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 20000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_com_allANDno 

ggsave(filename = "5) Comor 0-6 (IgG).pdf", plot = IgG_com_allANDno, width = 5.5, height = 3) 


# Kruskal-Wallis Test for Total Ab (NO, 1, 2, 3, 4, 5, 6) 
tidy_comor <- reorder_levels(tidy_comor, "Comor_name", order =  c("one", "two", "three", "four", "five", "six", "none")) 
tidy_comor %>% kruskal_test(TotalAntibody ~ Comor_name)

# Multiple pairwise-comparisons for Total Ab (Dunn's Test) (NO, 1, 2, 3, 4, 5, 6)  
dunnTest(tidy_comor$TotalAntibody, tidy_comor$Comor_name, method = "bonferroni") 


# Wilcoxon signed rank test IgG (Individually) - Unpaired due to missing information 
oneD <- tidy_comor %>% filter(Comor_name == "one")
wilcox.test(oneD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

twoD <- tidy_comor %>% filter(Comor_name == "two")
wilcox.test(twoD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

threeD <- tidy_comor %>% filter(Comor_name == "three")
wilcox.test(threeD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

fourD <- tidy_comor %>% filter(Comor_name == "four")
wilcox.test(fourD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

fiveD <- tidy_comor %>% filter(Comor_name == "five")
wilcox.test(fiveD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE) 

noneD <- tidy_comor %>% filter(Comor_name == "none")
wilcox.test(noneD$TotalAntibody, na.rm=TRUE, paired=FALSE, conf.int=TRUE)  


# Total Ab (NO, 1, 2, 3, 4, 5, 6) 

tidy_comor <- reorder_levels(tidy_comor, "Comor_name", order =  c("one", "two", "three", "four", "five", "six", "none")) 

tot_com_allANDno <- ggplot(tidy_comor, aes(x= reorder(Comor_name, TotalAntibody, FUN= median), y= TotalAntibody, fill= Comor_name))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) + 
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() +  
  ylim(0, 50000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_com_allANDno 

ggsave(filename = "6) Comor 0-6 (Total).pdf", plot = tot_com_allANDno, width = 5.5, height = 3) 



# Comorbidity (Yes, No)  

tidy_comor_YN <- vaccinated_copy %>%  
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
  ylim(0, 20000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_com_YN 

ggsave(filename = "7) Comor Yes-No (IgG).pdf", plot = IgG_com_YN, width = 3.5, height = 3) 



# Total antibody 

tot_com_YN <- ggplot(tidy_comor_YN, aes(x= Comor_name, y= TotalAntibody, fill= Comor_name))+ 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  scale_fill_manual(values=c("#99B9FF", "#FF94DB"))+ 
  ylim(0, 60000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_com_YN 

ggsave(filename = "8) Comor Yes-No (Total).pdf", plot = tot_com_YN, width = 3.5, height = 3) 



# Data tidying (All comorbidities in One Column) 

No_vac <- dplyr::filter(vaccinated, Diabetes== "No" & Hypertension== "No" & Heart_Disease== "No" & Kidney_Disease== "No" & Cancer== "No" & Others== "No") %>% 
  mutate(NoVac = Diabetes) %>%  
  select(-c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)) 

Diabetes_vac <- dplyr::select(vaccinated, -c(Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others)) %>% 
  filter(Diabetes == "Diabetes") 

Hypertension_vac <- dplyr::select(vaccinated, -c(Diabetes, Heart_Disease, Kidney_Disease, Cancer, Others)) %>% 
  filter(Hypertension == "Hypertension")   

Heart_Disease_vac <- dplyr::select(vaccinated, -c(Diabetes, Hypertension, Kidney_Disease, Cancer, Others)) %>% 
  filter(Heart_Disease == "Heart Disease")  

Kidney_Disease_vac <- dplyr::select(vaccinated, -c(Diabetes, Hypertension, Heart_Disease, Cancer, Others)) %>% 
  filter(Kidney_Disease == "Kidney Disease")  

Cancer_vac <- dplyr::select(vaccinated, -c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Others)) %>% 
  filter(Cancer == "Cancer") 

Others_vac <- dplyr::select(vaccinated, -c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer)) %>% 
  filter(Others == "Others") 

comorALL_vac <- bind_rows(Diabetes_vac, Hypertension_vac) %>% 
  bind_rows(Heart_Disease_vac) %>%  
  bind_rows(Kidney_Disease_vac) %>% 
  bind_rows(Cancer_vac) %>% 
  bind_rows(Others_vac) %>% 
  bind_rows(No_vac) %>% 
  mutate(Age_Status = if_else(Age >=60, "up60",
                              if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60")) 

library(tidyr)
comorInONE_vac <- comorALL_vac %>%  
  unite("Comorbidities_All", c(Diabetes, Hypertension, Heart_Disease, Kidney_Disease, Cancer, Others, NoVac), na.rm = TRUE)

# comorInONE_vac <- comorInONE_vac %>% dplyr::filter(!Comorbidities_All == "No")  



# Kruskal-Wallis Test for IgG 
comorInONE_vac %>% kruskal_test(IgG_level ~ Comorbidities_All)

# Multiple pairwise-comparisons for IgG (Dunn's Test)  
saveIgG <- dunnTest(comorInONE_vac$IgG_level, comorInONE_vac$Comorbidities_All, method = "bonferroni") 
saveIgG[["res"]] 
write.csv(saveIgG[["res"]], file = "Comorbid IgG p-value.csv") 

# IgG 

comorInONE_vac <- reorder_levels(comorInONE_vac, "Comorbidities_All", order =  c("Diabetes", "Hypertension", "Heart Disease", "Kidney Disease", "Cancer", "Others", "No")) 

IgG_all6_vac <- ggplot(comorInONE_vac, aes(x= reorder(Comorbidities_All, IgG_level, FUN=median), y= IgG_level, fill = Comorbidities_All)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 40000) + 
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

IgG_all6_vac 

ggsave(filename = "9) Comor ALL Names (IgG).pdf", plot = IgG_all6_vac, width = 7.5, height = 3) 


# Kruskal-Wallis Test for Total Ab 
comorInONE_vac %>% kruskal_test(TotalAntibody ~ Comorbidities_All)

# Multiple pairwise-comparisons for Total Ab 
saveTot <- dunnTest(comorInONE_vac$TotalAntibody, comorInONE_vac$Comorbidities_All, method = "bonferroni") 
saveTot[["res"]] 
write.csv(saveTot[["res"]], file = "Comorbid TAb p-value.csv")

# Total Ab 

comorInONE_vac <- reorder_levels(comorInONE_vac, "Comorbidities_All", order =  c("Diabetes", "Hypertension", "Heart Disease", "Kidney Disease", "Cancer", "Others", "No")) 

tot_all6_vac <- ggplot(comorInONE_vac, aes(x= reorder(Comorbidities_All, TotalAntibody, FUN=median), y= TotalAntibody, fill = Comorbidities_All)) + 
  geom_point(position=position_jitter(width = 0.05), alpha=0.05, size=2.5, pch = 21) +
  stat_boxplot(geom = 'errorbar', width = 0.2) + 
  geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) + 
  theme_bw() + 
  ylim(0, 60000) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black", size = 0.5) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) 

tot_all6_vac 

ggsave(filename = "10) Comor ALL Names (Total).pdf", plot = tot_all6_vac, width = 7.5, height = 3) 



#==================================== Data tidying (All comorbidities, seprately) ==========================================================================================


Diabetes2_vac <- dplyr::filter(vaccinated, Diabetes == "Diabetes") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60"))  

Hypertension2_vac <- dplyr::filter(vaccinated, Hypertension == "Hypertension") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60")) 

Heart_Disease2_vac <- dplyr::filter(vaccinated, Heart_Disease == "Heart Disease") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>% 
  filter(Age_Status %in% c("up60", "below60")) 

Kidney_Disease2_vac <- dplyr::filter(vaccinated, Kidney_Disease == "Kidney Disease") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60")) 

Cancer2_vac <- dplyr::filter(vaccinated, Cancer == "Cancer") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60"))

Others2_vac <- dplyr::filter(vaccinated, Others == "Others") %>% 
  mutate(Age_Status = if_else(Age >=60, "up60", 
                      if_else(Age <60, "below60", NULL))) %>%  
  filter(Age_Status %in% c("up60", "below60")) 


# IgG - Split Violin Plots (Separate)  

IgG_Dia_vac <- ggplot(Diabetes2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8))
  
IgG_Dia_vac 

IgG_Hyp_vac <- ggplot(Hypertension2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) + 
  ylim(-100000, 250000)
  
IgG_Hyp_vac 

IgG_Hrt_vac <- ggplot(Heart_Disease2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) + 
  ylim(-10000, 77000)
  
IgG_Hrt_vac  

IgG_Kid_vac <- ggplot(Kidney_Disease2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) + 
  ylim(-100000, 250000)

IgG_Kid_vac 

IgG_Can_vac <- ggplot(Cancer2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) + 
  ylim(-100000, 250000)

IgG_Can_vac 

IgG_Oth_vac <- ggplot(Others2_vac, aes(x= Age_Status, y= IgG_level, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) + 
  ylim(-100000, 250000) 

IgG_Oth_vac  



# Total Ab - Split Violin (Separate)  

tot_Dia_vac <- ggplot(Diabetes2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 

tot_Dia_vac 

tot_Hyp_vac <- ggplot(Hypertension2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 

tot_Hyp_vac 

tot_Hrt_vac <- ggplot(Heart_Disease2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 

tot_Hrt_vac  

tot_Kid_vac <- ggplot(Kidney_Disease2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 

tot_Kid_vac 

tot_Can_vac <- ggplot(Cancer2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 

tot_Can_vac  

tot_Oth_vac <- ggplot(Others2_vac, aes(x= Age_Status, y= TotalAntibody, fill = Gender)) + 
  geom_split_violin(trim = F, draw_quantiles = 0.5) + 
  geom_point(shape=16, position=dodge) + 
  theme_bw() + 
  scale_fill_manual(values=alpha(c("#1AA19F","#FCB248"),0.8)) 
  
tot_Oth_vac  



#==================================== Descriptive Statistics ===============================================================================================================

library(Rmisc)

# Gender 
vaccinated %>% count("Gender") 

male <- vaccinated %>%  
  filter(Gender == "Male")

female <- vaccinated %>%  
  filter(Gender == "Female") 

summary(male$IgG_level)
CI(male$IgG_level, ci=0.95)

summary(male$TotalAntibody)
CI(male$TotalAntibody, ci=0.95)

summary(female$IgG_level)
CI(female$IgG_level, ci=0.95)

summary(female$TotalAntibody)
CI(female$TotalAntibody, ci=0.95) 

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

# Comor (Dia, Hyp etc) 
comorInONE_vac %>% count("Comorbidities_All") 

summary(Diabetes_vac$IgG_level)
CI(Diabetes_vac$IgG_level, ci=0.95)

summary(Diabetes_vac$TotalAntibody)
CI(Diabetes_vac$TotalAntibody, ci=0.95)


summary(Hypertension_vac$IgG_level)
CI(Hypertension_vac$IgG_level, ci=0.95)

summary(Hypertension_vac$TotalAntibody)
CI(Hypertension_vac$TotalAntibody, ci=0.95) 


summary(Heart_Disease_vac$IgG_level)
CI(Heart_Disease_vac$IgG_level, ci=0.95)

summary(Heart_Disease_vac$TotalAntibody)
CI(Heart_Disease_vac$TotalAntibody, ci=0.95)


summary(Kidney_Disease_vac$IgG_level)
CI(Kidney_Disease_vac$IgG_level, ci=0.95)

summary(Kidney_Disease_vac$TotalAntibody)
CI(Kidney_Disease_vac$TotalAntibody, ci=0.95) 


summary(Cancer_vac$IgG_level)
CI(Cancer_vac$IgG_level, ci=0.95)

summary(Cancer_vac$TotalAntibody)
CI(Cancer_vac$TotalAntibody, ci=0.95) 


summary(Others_vac$IgG_level)
CI(Others_vac$IgG_level, ci=0.95)

summary(Others_vac$TotalAntibody)
CI(Others_vac$TotalAntibody, ci=0.95) 


summary(No_vac$IgG_level)
CI(No_vac$IgG_level, ci=0.95)

summary(No_vac$TotalAntibody)
CI(No_vac$TotalAntibody, ci=0.95) 


Diabetes_vac 
Hypertension_vac 
Heart_Disease_vac 
Kidney_Disease_vac 
Cancer_vac 
Others_vac 

# Comor (1, 2, 3 etc)  
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
