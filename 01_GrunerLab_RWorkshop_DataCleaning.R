#### Code Information ####
#Code Goal: Workshop for Gruner Lab on Oct 4, 2024 that displays a wide range of coding skills, show data analysis tools and uses, and visualize data. In this script we will read in and clean our data before combining it into one dataframe
#Author: KJB

#### Load Libraries ####
###### USEFUL TIP #####
#I like to organize my code into sections that I can click through for easier access. If you use at least 4 # signs on either side of text, it will become a tab in your script that you can easily access by clicking on the index in the lower left hand corner of your console. Putting more # signs you put in front of the text indents the text more in the index 

#Here we are installing and loading packages into our Rstudio session. If you have never used a package before, you must first install it onto your computer. Then everytime after that, you can simply load the library into the session

install.packages("tidyverse") #installs package onto your computer
library(tidyverse) #loads package into session - tidyverse is a great package for transforming, combining, and cleaning data

install.packages("here")
library(here) #easy file referencing for an R project

#### Read in Data ####
#These data are provided in the project folder. It is arthropod data that I collected during my graduate degree. It's currently unpublished (but we're working on it!). Please don't share these data with others - this is just for learning!

#2020
ID_Data_20<-read.csv(here("Data/2020_InsectData.csv")) #here() tells R that the starting point for looking for the file is in the current working directory. We add "Data/" because I've put the data into a folder titled data within the current working directory

#2021
ID_Data_21<-read.csv(here("Data/2021_InsectData.csv")) 

#2022
ID_Data_22<-read.csv(here("Data/2022_InsectData.csv")) 


#### View data and determine errors from data entry, etc ####
#for data analysis and visualization, I want all three files to be in one combined dataframe. So I first want to check on data structure, then merge the dataframes together

#Let's look at the column names first to make sure they match, otherwise we would have to change them to match for merging later. 
colnames(ID_Data_20)
colnames(ID_Data_21) 
colnames(ID_Data_22) 

#Let's look at each column of the ID data and make sure nothing looks odd and that our different dataframes match. As I notice issues, I will add it to the cleaned files below. The unique() function displays all of the unique values for a column, the sort function orders them alphabetically or numerically
unique(ID_Data_20$Year)
unique(ID_Data_21$Year)
unique(ID_Data_22$Year)

unique(sort(ID_Data_20$Grazing_Treatment))
unique(sort(ID_Data_21$Grazing_Treatment))
unique(sort(ID_Data_22$Grazing_Treatment))

unique(sort(ID_Data_20$Block))
unique(sort(ID_Data_21$Block))
unique(sort(ID_Data_22$Block))

unique(sort(ID_Data_20$Plot))
unique(sort(ID_Data_21$Plot))
unique(sort(ID_Data_22$Plot))

unique(sort(ID_Data_20$Order))
unique(sort(ID_Data_21$Order))
unique(sort(ID_Data_22$Order))

unique(sort(ID_Data_20$Family))
unique(sort(ID_Data_21$Family))
unique(sort(ID_Data_22$Family))


#Now let's look at the summary of the data in each dataframe. This will allow us to make sure our data structure is the same for each dataframe before we merge
summary(ID_Data_20)
summary(ID_Data_21)
summary(ID_Data_22) #our Sample category here is numeric instead of a character. We'll change that below

#### Make dataframes match for combining later ####

#We have many of the same columns but they're in different orders and we don't need all of these columns. Let's select for just the columns we want/need and fix some spelling or data entry issues
###### USEFUL TIP #####
#ifelse() statements are one way of many to fix errors or inconsistencies within your data. If else statements work by creating a new column where you search an old column for certain criteria, then if R finds that criteria, it places the new replacement into the new column. See detailed example below. This is good for single or specific replacements
#Another great way to fix errors or inconsistencies is using gsub(). gsub() allows you to replace all occurrences of certain text within a given column (or string) in R. Think of this like "Find and Replace" in word or excel


ID_Data_20_Clean<-ID_Data_20 %>% 
  select(Year,Grazing_Treatment,Block,Plot,Sample,Order,Family) %>% 
  #Change block to be consistent - here we're saying "Create a new column called "Block". To fill this column with data, look into the old column "Block" and if you find "B1" put a 1 in the new column, if you find "B2" put a 2 in the new column, if you find "B3" put a 3 in the new column, after that, fill any missing rows in the new column with whatever was already in the old column. 
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Fix spelling mistakes
  mutate(Family=ifelse(Family=="Salticide","Salticidae",ifelse(Family=="Lygacidae","Lygaeidae",ifelse(Family=="Staphylindae","Staphylinidae",ifelse(Family=="unknown","Unknown",Family)))))

ID_Data_21_Clean<-ID_Data_21 %>% 
  select(Year,Grazing_Treatment,Block,Plot,Sample,Order,Family) %>% 
  #fix "LG " to "LG"
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment))%>%
  #Change block to be consistent
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Fix spelling mistakes -- BTW I feel sure there's a better way to fix spelling errors in scientific names but I've never taken the time to figure it out. So take this as a messy and long but accurate way to do it!
  mutate(Family=ifelse(Family=="Acridiae", "Acrididae",ifelse(Family=="Agramyzidae", "Agromyzidae",ifelse(Family=="curculionidae","Curculionidae",ifelse(Family=="Formicide","Formicidae",ifelse(Family=="Ligidae","Lygaeidae",ifelse(Family=="Scuttelleridae","Scutelleridae",ifelse(Family=="thomisidae","Thomisidae",ifelse(Family=="Thomsidae", "Thomisidae",ifelse(Family=="Formicide","Formicidae",ifelse(Family=="unknown","Unknown",Family)))))))))))

#This tells R to look into the column "Order" and if there are any spaces, to replace them with nothing (i.e. taking the spaces out)
ID_Data_21_Clean$Order<-gsub(" ","",ID_Data_21_Clean$Order)

ID_Data_22_Clean<-ID_Data_22 %>% 
  select(Year,Grazing_Treatment,Block,Plot,Sample,Order,Family) %>%
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  #correct misspellings and inconsistencies in order data
  mutate(Order=ifelse(Order=="araneae","Araneae",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="lepidoptera","Lepidoptera",ifelse(Order=="neuroptera","Neuroptera",ifelse(Order=="orthoptera","Orthoptera", ifelse(Order=="thysanoptera","Thysanoptera",ifelse(Order=="unknown","Unknown",Order))))))))))) %>% 
  #correct misspellings and inconsistencies in family data
  mutate(Family=ifelse(Family=="chloropidae","Chloropidae",ifelse(Family=="Cicadellidea","Cicadellidae",ifelse(Family=="Euiophidae","Eulophidae",ifelse(Family=="eupelmidae","Eupelmidae",ifelse(Family=="ichneumonidae","Ichneumonidae",ifelse(Family=="latridiidae","Latridiidae",ifelse(Family=="lycosidae","Lycosidae",ifelse(Family=="muscidae","Muscidae",ifelse(Family=="myrmeleontidae","Myrmeleontidae",ifelse(Family=="nabidae","Nabidae",ifelse(Family=="pentatomidae","Pentatomidae",ifelse(Family=="perilampidae","Perilampidae",ifelse(Family=="platygastridae","Platygastridae",ifelse(Family=="scarabaeidae","Scarabaeidae",ifelse(Family=="Scarabacidae","Scarabaeidae",ifelse(Family=="sepsidae","Sepsidae",ifelse(Family=="Thripinae","Thripidae",ifelse(Family=="Thrips","Thripidae",ifelse(Family=="Tiombiculidae","Trombiculidae",ifelse(Family=="tingidae","Tingidae",ifelse(Family=="trichoceridae","Trichoceridae",ifelse(Family=="Trichoceridea","Trichoceridae",ifelse(Family=="unknown","Unknown",ifelse(Family=="",NA,ifelse(Family=="N/A",NA,ifelse(Family=="n/a",NA,Family))))))))))))))))))))))))))) 

ID_Data_22_Clean$Sample<-as.character(ID_Data_22_Clean$Sample)


#### Merge Dataframes ####

#looks like they're all the same class of data with the same column names. Now we want to merge the dataframes. Becuase our data have the same columns, in the same order, we can use rbind which just binds the rows directly together 
ID_Data<-rbind(ID_Data_20_Clean,ID_Data_21_Clean,ID_Data_22_Clean) %>% 
  #remove unknowns
  filter(Family!="Unknown")


#### Save Dataframe as CSV ####
#now that we've created one dataframe with all 3 years of our cleaned data, we can save that as a CSV for use in our data analysis
write.csv(ID_Data,here("Output","Arthropod_ID_Data_AllYears.csv"))
