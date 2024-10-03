#### Code Information ####
#Code Goal: Workshop for Gruner Lab on Oct 4, 2024 that displays a wide range of coding skills, show data analysis tools and uses, and visualize data. In this script we will create graphs of our data to show patterns and directionality of changes.
#Author: KJB

#### Load Libraries ####
install.packages("tidyverse") #if not already installed on your computer
library(tidyverse)

install.packages("here") #if not already installed on your computer
library(here) 

install.packages("ggplot2")
library(ggplot2)

#### Read in Data ####
#Arthropod_ID_Data_AllYears.csv was created in 01_GrunerLab_RWorkshop_DataCleaning.R
ID_Data<-read.csv(here("Output","Arthropod_CommunityMetrics.csv"))


#### Graphs: Question 1 ####

##### Shannon's Diversity Graph ####

##### Richness Graph ####

##### Evenness Graph ####

#### Graphs: Question 2 ####

##### Shannon's Diversity Graph ####
#### Figure 1B. Humidity ####
HumidityGraph <- ggplot(Abiotics,aes(x=week_num, y=humidity_Mean,group=treatment,color=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=15)+
  geom_line(aes(color=treatment,linetype=treatment),size=4)+
  geom_errorbar(aes(ymin=humidity_Mean-humidity_St_Error,ymax=humidity_Mean+humidity_St_Error),width=0.2,size=4)+
  scale_linetype_manual(values=c("solid","longdash","twodash","dashed"))+
  scale_shape_manual(values=c(15,16,17,18))+
  scale_color_manual(values=c("#76AFE8","#E6E291","#88A76E","#CA7E77"))+
  xlab("Week Number")+
  ylab("Humidity (%)")+
  annotate("text", x=1.6, y=100, label = "B. Humidity", size=20)+
  #add in rectangle around heatwave
  annotate('rect', xmin = c("3","19"), xmax = c("4","20"),ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())

##### Richness Graph ####

##### Evenness Graph ####




