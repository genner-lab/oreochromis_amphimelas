##Set working directory

##Load packages

library(dplyr)
library(dunn.test)

##Import initial dataframe

Amphimelas_Meristic <- read.table("Meristic_Data.csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)

##Summary statistics

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Number_of_Lateral_Line_Scales, na.rm=TRUE), Std=sd(Number_of_Lateral_Line_Scales, na.rm=TRUE), Max=max(Number_of_Lateral_Line_Scales, na.rm=TRUE), Min=min(Number_of_Lateral_Line_Scales, na.rm=TRUE), Median=median(Number_of_Lateral_Line_Scales, na.rm=TRUE))

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Number_of_Anal_Fin_Rays, na.rm=TRUE), Std=sd(Number_of_Anal_Fin_Rays, na.rm=TRUE), Max=max(Number_of_Anal_Fin_Rays, na.rm=TRUE), Min=min(Number_of_Anal_Fin_Rays, na.rm=TRUE), Median=median(Number_of_Anal_Fin_Rays, na.rm=TRUE))

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Number_of_Dorsal_Fin_Spines, na.rm=TRUE), Std=sd(Number_of_Dorsal_Fin_Spines, na.rm=TRUE), Max=max(Number_of_Dorsal_Fin_Spines, na.rm=TRUE), Min=min(Number_of_Dorsal_Fin_Spines, na.rm=TRUE), Median=median(Number_of_Dorsal_Fin_Spines, na.rm=TRUE))

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Number_of_Dorsal_Fin_Rays, na.rm=TRUE), Std=sd(Number_of_Dorsal_Fin_Rays, na.rm=TRUE), Max=max(Number_of_Dorsal_Fin_Rays, na.rm=TRUE), Min=min(Number_of_Dorsal_Fin_Rays, na.rm=TRUE), Median=median(Number_of_Dorsal_Fin_Rays, na.rm=TRUE))

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Number_of_Pectoral_Fin_Rays, na.rm=TRUE), Std=sd(Number_of_Pectoral_Fin_Rays, na.rm=TRUE), Max=max(Number_of_Pectoral_Fin_Rays, na.rm=TRUE), Min=min(Number_of_Pectoral_Fin_Rays, na.rm=TRUE), Median=median(Number_of_Pectoral_Fin_Rays, na.rm=TRUE))

Amphimelas_Meristic %>%
  group_by(Locality) %>% 
  summarise(Mean=mean(Standard_Length_in_mm, na.rm=TRUE), Std=sd(Standard_Length_in_mm, na.rm=TRUE), Max=max(Standard_Length_in_mm, na.rm=TRUE), Min=min(Standard_Length_in_mm, na.rm=TRUE), Median=median(Standard_Length_in_mm, na.rm=TRUE))

##Kruskal-Wallis tests for differences among populations, with post-hoc Dunn's tests, for five variable meristics

dunn.test(Amphimelas_Meristic$Number_of_Lateral_Line_Scales, Amphimelas_Meristic$Locality, method="bonferroni", list=TRUE)

dunn.test(Amphimelas_Meristic$Number_of_Dorsal_Fin_Spines, Amphimelas_Meristic$Locality, method="bonferroni", list=TRUE)

dunn.test(Amphimelas_Meristic$Number_of_Dorsal_Fin_Rays, Amphimelas_Meristic$Locality, method="bonferroni", list=TRUE)

dunn.test(Amphimelas_Meristic$Number_of_Anal_Fin_Rays, Amphimelas_Meristic$Locality, method="bonferroni", list=TRUE)

dunn.test(Amphimelas_Meristic$Number_of_Pectoral_Fin_Rays, Amphimelas_Meristic$Locality, method="bonferroni", list=TRUE)

##Tests for differences between sexes, using Wilcoxon ranked sum, for five variable meristics

wilcox.test(Number_of_Lateral_Line_Scales ~ Sex, data=Amphimelas_Meristic, exact=FALSE) 

wilcox.test(Number_of_Dorsal_Fin_Spines ~ Sex, data=Amphimelas_Meristic, exact=FALSE) 

wilcox.test(Number_of_Dorsal_Fin_Rays ~ Sex, data=Amphimelas_Meristic, exact=FALSE) 

wilcox.test(Number_of_Anal_Fin_Rays ~ Sex, data=Amphimelas_Meristic, exact=FALSE) 

wilcox.test(Number_of_Pectoral_Fin_Rays ~ Sex, data=Amphimelas_Meristic, exact=FALSE) 

