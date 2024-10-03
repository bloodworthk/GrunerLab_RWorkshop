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

install.packages("patchwork")
library(patchwork) #for creating graph with multiple panels

#### Read in Data ####
#Arthropod_ID_Data_AllYears.csv was created in 01_GrunerLab_RWorkshop_DataCleaning.R
CommunityMetrics_Graphs<-read.csv(here("Output","Arthropod_CommunityMetrics.csv")) %>% 
  #create a new column with graph labels exactly how we want them
  mutate(Graph_Labels=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Low Impact Grazing",ifelse(Grazing_Treatment=="NG","Rest from Grazing",Grazing_Treatment))))


#### Set ggplot base ####
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title = element_text(size=30, vjust=2), panel.grid.major=element_blank(),panel.grid.minor=element_blank(), legend.title=element_blank(),legend.text=element_text(size=40))

#### Graphs: Question 1 ####

##### Shannon's Diversity Graph ####
Shannon_Q1<-ggplot(CommunityMetrics_Graphs,aes(x=Graph_Labels,y=Shannon,color=Graph_Labels))+ #make a graph where the xaxis is the grazing treatment (graph labels) and the y axis is Shannon's Diversity
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2 
  xlab("Grazing  Regime")+ #Label the x-axis "Grazing Regime"
  ylab("Shannon Diversity")+  #Label the y-axis "Shannon Diversity"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+ #for the x axis, wrap text after 8 characters
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.6, y=2.35, label="a)",size=20,color="black") + #label the graph with a letter
  theme(axis.text.x=element_blank(),axis.title.x=element_blank()) #remove label and title from x axis 
  
##### Richness Graph ####
Richness_Q1<-ggplot(CommunityMetrics_Graphs,aes(x=Graph_Labels,y=richness,color=Graph_Labels))+ #make a graph where the xaxis is the grazing treatment (graph labels) and the y axis is Richness
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2 
  xlab("Grazing  Regime")+ #Label the x-axis "Grazing Regime"
  ylab("Family Richness")+  #Label the y-axis "Family Richness"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+ #for the x axis, wrap text after 8 characters
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.6, y=15, label="b)",size=20,color="black") + #label the graph with a letter
  theme(axis.text.x=element_blank(),axis.title.x=element_blank()) #remove label and title from x axis 

##### Evenness Graph ####
Evar_Q1<-ggplot(CommunityMetrics_Graphs,aes(x=Graph_Labels,y=Evar,color=Graph_Labels))+ #make a graph where the xaxis is the grazing treatment (graph labels) and the y axis is Richness
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2
  xlab("Grazing  Regime")+ #Label the x-axis "Grazing Regime"
  ylab("Evenness")+  #Label the y-axis "Evenness"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+ #for the x axis, wrap text after 8 characters
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.58, y=0.98, label="c)",size=20,color="black") #label the graph with a letter

#### Create 1 figure with Diversity, Richness, Evenness Figure ####
Shannon_Q1+
  Richness_Q1+
  Evar_Q1+
  plot_layout(ncol = 1,nrow = 3)
#Save using Export in Plot panel, Save as Image, and then renaming it and saving it in location you prefer. Save at size 1000x1200

#### Graphs: Question 2 ####

##### Shannon's Diversity Graph ####
Shannon_Q2<-ggplot(CommunityMetrics_Graphs,aes(x=as.factor(Year),y=Shannon,color=Graph_Labels))+ #make a graph where the xaxis is the Year, then within each year there are three grazing treatment (graph labels) and the y axis is Shannon's Diversity
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2
  xlab("Year") + #Label the x-axis "Year"
  ylab("Shannon Diversity") +  #Label the y-axis "Shannon Diversity"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.5, y=2.35, label="a)",size=15,color="black") + #label the graph with a letter
  theme(axis.text.x=element_blank(),axis.title.x=element_blank()) #remove label and title from x axis 

##### Richness Graph ####
Richness_Q2<-ggplot(CommunityMetrics_Graphs,aes(x=as.factor(Year),y=richness,color=Graph_Labels))+ #make a graph where the xaxis is the Year, then within each year there are three grazing treatment (graph labels) and the y axis is Shannon's Diversity
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2
  xlab("Year") + #Label the x-axis "Year"
  ylab("Family Richness") +  #Label the y-axis "Shannon Diversity"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.5, y=15, label="b)",size=15,color="black") + #label the graph with a letter
  theme(axis.text.x=element_blank(),axis.title.x=element_blank()) #remove label and title from x axis 

##### Evenness Graph ####
Evar_Q2<-ggplot(CommunityMetrics_Graphs,aes(x=as.factor(Year),y=Evar,color=Graph_Labels))+ #make a graph where the xaxis is the Year, then within each year there are three grazing treatment (graph labels) and the y axis is Shannon's Diversity
  geom_boxplot(lwd=2)+ #graph data in boxplot format with the linewidth being 2
  xlab("Year") + #Label the x-axis "Year"
  ylab("Evenness") +  #Label the y-axis "Shannon Diversity"
  scale_color_manual(values=c("#9D858D","#ABDEFF","#6D882B"),breaks=c("Rest from Grazing","High Impact Grazing", "Low Impact Grazing"))+ #choose colors for each grazing treatment
  theme(legend.position="none")+ #remove the legend from the graph
  geom_text(x=0.5, y=0.98, label="c)",size=15,color="black") #label the graph with a letter

#### Create 1 figure with Diversity, Richness, Evenness Figure ####
Shannon_Q2+
  Richness_Q2+
  Evar_Q2+
  plot_layout(ncol = 1,nrow = 3)
#Save using Export in Plot panel, Save as Image, and then renaming it and saving it in location you prefer. Save at size 1000x1200


