#### Code Information ####
#Code Goal: Workshop for Gruner Lab on Oct 4, 2024 that displays a wide range of coding skills, show data analysis tools and uses, and visualize data. In this script we will calculate community metrics, test the assumptions of parametric models, and answer two questions using multiple statistical tests
#Author: KJB

#### Load Libraries ####

install.packages("tidyverse") #if not already installed on your computer
library(tidyverse)

install.packages("here") #if not already installed on your computer
library(here) 

install.packages("codyn")
library(codyn) #for calculating community metrics

install.packages("car")
library(car) #for homoscedacitiy testing

install.packages("olsrr")
library(olsrr) #for normality testing

install.packages("lme4")
library(lme4) #for lmer test

install.packages("multcomp")
library(multcomp) #for post-hoc test


#### Read in Data ####
#Arthropod_ID_Data_AllYears.csv was created in 01_GrunerLab_RWorkshop_DataCleaning.R
ID_Data<-read.csv(here("Output","Arthropod_ID_Data_AllYears.csv")) #here() tells R that the starting point for looking for the file is in the current working directory. We add "Output" because I've put the data into a folder titled Output within the current working directory


#### Calculate Community Metrics ####

###### Absolute and Relative Abundance by Family ####
#Goal: calculate absolute and total abundance
Abundance <- ID_Data %>% 
  group_by(Year,Grazing_Treatment,Block,Plot) %>% #tells R that whatever command comes after this should occur within these groups 
  mutate(Plot_Abundance=length(Sample)) %>% #calculate total abundance of all arthropods in a given plot
  ungroup() %>% #ungroup data (if you don't do this is could mess up later code)
  group_by(Year,Grazing_Treatment,Block,Plot,Order,Family,Plot_Abundance) %>%
  summarise(Absolute_Abundance=length(Sample)) %>% #count up total abundance of each family of arthropods within a given plot
  ungroup() %>% 
  mutate(Relative_Abundance=Absolute_Abundance/Plot_Abundance) #calculate relative abundance of each family of arthropods within a given plot

#let's take a peak out our data and see what the structure is and if everything is looking correct
summary(Abundance) 
head(Abundance)

###### Plot Shannon's Diversity by Family ####

Shannon_Diversity <- community_diversity(df = Abundance,
                                        time.var = "Year",
                                        replicate.var = c("Block","Grazing_Treatment","Plot"),
                                        abundance.var = "Relative_Abundance") #codyn package has community_diversity 


###### Plot Evenness & Richness by Family ####
Evar_Richness <- community_structure(df = Abundance,
                            time.var = "Year",
                            replicate.var = c("Grazing_Treatment","Block","Plot"),
                            abundance.var = "Relative_Abundance",
                            metric = "Evar")

#join the datasets
CommunityMetrics <- Shannon_Diversity %>%
  full_join(Evar_Richness) 

summary(CommunityMetrics)

# Save Community Metrics for use in creating graphs
#write.csv(CommunityMetrics ,here("Output","Arthropod_CommunityMetrics.csv"))


#### Check that data meet the assumptions of parametric models ####
#Shannon's Diversity
Shannon_Assumptions <- lm(Shannon ~ Grazing_Treatment,
                            data = CommunityMetrics)
ols_plot_resid_hist(Shannon_Assumptions) 
ols_test_normality(Shannon_Assumptions) #normal (not all 4 show normality but 3/4 do so it's typically considered "good enough" for ecological data)
#check for homoscedasticity 
leveneTest(data = CommunityMetrics,Shannon ~ Grazing_Treatment) #P value is not significant and therefore the assumption of homoscedasticity is met

#Richness
Richness_Assumptions <- lm(richness ~ Grazing_Treatment,
                           data = CommunityMetrics)
ols_plot_resid_hist(Richness_Assumptions) 
ols_test_normality(Richness_Assumptions) #not normal so we need to transform the data
#check for homoscedasticity 
leveneTest(data = CommunityMetrics,richness ~ Grazing_Treatment) #Pvalue is not significant and therefore the assumption of homoscedasticity is met

Richness_Assumptions_Trans <- lm(sqrt(richness) ~ Grazing_Treatment,
                          data = CommunityMetrics)
ols_plot_resid_hist(Richness_Assumptions_Trans) 
ols_test_normality(Richness_Assumptions_Trans) #not normal so we add do the sqrt of richness to normalize data (still not perfect but "good enough" for ecological data) 
#check for homoscedasticity 
leveneTest(data = CommunityMetrics,sqrt(richness) ~ Grazing_Treatment) #Pvalue is not significant and therefore the assumption of homoscedasticity is met

#Evenness
Evar_Assumptions <- lm(Evar ~ Grazing_Treatment,
                       data = CommunityMetrics)
ols_plot_resid_hist(Evar_Assumptions) 
ols_test_normality(Evar_Assumptions) #not normal so we need to transform the data
leveneTest(data = CommunityMetrics,Evar ~ Grazing_Treatment) #Pvalue is not significant and therefore the assumption of homoscedasticity is met

Evar_Assumptions_Trans <- lm(exp(Evar) ~ Grazing_Treatment,
                          data = CommunityMetrics)
ols_plot_resid_hist(Evar_Assumptions_Trans) 
ols_test_normality(Evar_Assumptions_Trans) #not normal so we add do the exp of evar to normalize data
#check for homoscedasticity 
leveneTest(data = CommunityMetrics,exp(Evar) ~ Grazing_Treatment) #Pvalue is not significant and therefore the assumption of homoscedasticity is met

#### Statistical Tests: Question 1 ####
#How does the arthropod community change with grazing intensity?
#Now that we know our data meets the assumptions of parametric tests, we can run our statistical tests. Reminder: you need to use your transformed data if you had to transform the data in order to meet the assumptions. 

###### Shannon's Diversity ####
#Run our most basic test first: an anova with no random effect
Diversity_ANOVA<-aov(Shannon ~ Grazing_Treatment,
                       data = CommunityMetrics)
summary(Diversity_ANOVA) #output shows you if grazing treatments are different (amongst each other)

#Run a linear regression with Block as a random effect
Diversity_Reg<-lmer(Shannon ~ Grazing_Treatment + (1|Block),
                    data = CommunityMetrics) #Fit a linear mixed-effects model to data where block is a random effect (we're accounting for variability that might exist because our plots are in different blocks - i.e. all plots in block 1 might be more similar than all plots in block 2 regardless of the grazing treatment) 
anova(Diversity_Reg) #running a maximum likelihood test on the mixed model - tells us if the overall group is different. If it is, we will have to run a post-hoc test to determine the differences 
summary(Diversity_Reg)

#Run a linear regression with Block and Year as a random effect
Diversity_Reg2<-lmer(Shannon ~ Grazing_Treatment + (1|Year) + (1|Block),
                    data = CommunityMetrics) #Fit a linear mixed-effects model to data where block and year are random effects - an alternitive to this would be to run each year seperately 
anova(Diversity_Reg2) #running a maximum likelihood test on the mixed model - tells us if the overall group is different. If it is, we will have to run a post-hoc test to determine the differences 
summary(Diversity_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Diversity_ANOVA,Diversity_Reg,Diversity_Reg2) #running this shows that the AIC scores are very similar. Lower AIC scores indicate a better fit model. the LMER with only the block as a random effect is not as good of a model fit as the anova or the LMER with year and block. Not surprising given that block does not account for any variation in our model. 


###### Richness ####
#Run our most basic test first: an anova with no random effect
Richness_ANOVA<-aov(sqrt(richness) ~ Grazing_Treatment,
                     data = CommunityMetrics)
summary(Richness_ANOVA)

#Run a linear regression with Block as a random effect
Richness_Reg<-lmer(sqrt(richness) ~ Grazing_Treatment + (1|Block),
                    data = CommunityMetrics)
anova(Richness_Reg)
summary(Richness_Reg)

#Run a linear regression with Year and Block as a random effect
Richness_Reg2<-lmer(sqrt(richness) ~ Grazing_Treatment + (1|Year) + (1|Block),
                     data = CommunityMetrics)
anova(Richness_Reg2) 
summary(Richness_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Richness_ANOVA,Richness_Reg,Richness_Reg2) #here our second LMER is a better model fit


###### Evar ####
#Run our most basic test first: an anova with no explanatory variables
Evar_ANOVA<-aov(exp(Evar) ~ Grazing_Treatment,
                    data = CommunityMetrics)
summary(Evar_ANOVA)

#Run a linear regression with explanatory variables
Evar_Reg<-lmer(exp(Evar) ~ Grazing_Treatment + (1|Block),
                   data = CommunityMetrics) 
anova(Evar_Reg)
summary(Evar_Reg)

#Run a linear regression with Year and Block as a random effect
Evar_Reg2<-lmer(exp(Evar)~ Grazing_Treatment + (1|Year) + (1|Block),
                     data = CommunityMetrics)
anova(Evar_Reg2) 
summary(Evar_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Evar_ANOVA,Evar_Reg,Evar_Reg2) #here our second LMER is a better model fit

#### Statistical Tests: Question 2 ####
#Does the arthropod community change from year to year with grazing intensity?

###### Shannon's Diversity ####
#Run an anova
Diversity_Year_Reg<-aov(Shannon ~ Grazing_Treatment* as.factor(Year),
                    data = CommunityMetrics) 
summary(Diversity_Year_Reg)

#Run a linear regression with Block as a random effect
Diversity_Year_Reg2<-lmer(Shannon ~ Grazing_Treatment* as.factor(Year) + (1|Block),
                         data = CommunityMetrics) 
anova(Diversity_Year_Reg2) 
summary(Diversity_Year_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Diversity_Year_Reg,Diversity_Year_Reg2) #the anova is a better model than the lmer with a random effect 


###### Richness ####
#Run an anova
Richness_Year_Reg<-aov(sqrt(richness) ~ Grazing_Treatment * as.factor(Year),
                        data = CommunityMetrics)  #this has us looking
summary(Richness_Year_Reg) #year is significant


#Run a linear regression with Block as a random effect
Richness_Year_Reg2<-lmer(sqrt(richness) ~ Grazing_Treatment * as.factor(Year) + (1|Block),
                          data = CommunityMetrics) 
anova(Richness_Year_Reg2) 
summary(Richness_Year_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Richness_Year_Reg,Richness_Year_Reg2) #the anova is a better model than the lmer with a random effect.

###### Evar ####
#Run an anova
Evar_Year_Reg<-aov(exp(Evar) ~ Grazing_Treatment * as.factor(Year),
                       data = CommunityMetrics)  #this has us looking
summary(Evar_Year_Reg) #year is significant


#Run a linear regression with Block as a random effect
Evar_Year_Reg2<-lmer(exp(Evar) ~ Grazing_Treatment * as.factor(Year) + (1|Block),
                         data = CommunityMetrics) 
anova(Evar_Year_Reg2) 
summary(Evar_Year_Reg2)

## Now let's compare these three models and choose which one fits our data the best
AIC(Evar_Year_Reg,Evar_Year_Reg2) #the anova is a better model than the lmer with a random effect.

