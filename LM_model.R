###Nicki Shumway
###13_Nov_2017, updated Nov 2018
####Code for linear regression



##Load packages

library("Hmisc")
library("MASS")
library("AER")
library("ggplot2")
library("tidyverse")
library(ggplot2)
library(RColorBrewer)

## set wd

#setwd("C:/Users/uqnshumw/OneDrive - The University of Queensland/PhD Research/Ch. 1a Marine NNL Policy Review")

### Load Data ###

Policy_Analysis <- read.csv("C:/Users/uqnshumw/OneDrive - The University of Queensland/PhD Research/Ch. 1a Marine NNL Policy Review/Policy_Analysis.csv")
View(Policy_Analysis)

##All_data <- read.csv("C:\Users\uqnshumw\OneDrive - The University of Queensland\PhD Research\Ch. 1a Marine NNL Policy Review\Policy_Analysis.csv")
##View(All_data)


##Policy_Analysis$Governance[Policy_Analysis$Governance == ""] <-NA
##View(Policy_Analysis)

#WGI_analysis <- read.csv("C:/Users/uqnshumw/OneDrive - The University of Queensland/PhD Research/Ch. 1a Marine NNL Policy Review/WGI_analysis.csv")
##View(WGI_Analysis)

##########################################
##### Correlation Matrix - ALL DATA #####
#########################################
head(Policy_Analysis)
Policy_data <- Policy_Analysis[,4:length(Policy_Analysis)] ## makes a new dataframe from policy analysis, subsets everything to the right of presence/absence
View(Policy_data)


round(cor(Policy_data, use = "na.or.complete"),2) ##rounds the date two decimal places for clarity
cor_matrix <- cor(Policy_data, use="pairwise.complete.obs", method = c("spearman"))   
View(cor_matrix)                                        


### Make Generalized linear model of all data against presence/absence of policy ####

All_Model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$Richness + Policy_Analysis$mam_bird + Policy_Analysis$EEZ_Protected
                    + Policy_Analysis$Polity2 + Policy_Analysis$GDP +Policy_Analysis$ + Policy_Analysis$Rents + Policy_Analysis$OHI, family = "binomial", data = Policy_Analysis)
summary(All_Model)

#plot(All_Model)


#####################################
#####BIODIVERSITY VALUES#############
#####################################


### GLM of Marine variables#####
Marine_model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$mam_bird +Policy_Analysis$EEZ_Protected +Policy_Analysis$Richness+Policy_Analysis$OHI, family = "binomial", data = Policy_Analysis)
summary(Marine_model)

## GLM of response to threat###
Threat_model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$mam_bird, family = "binomial", data = Policy_Analysis )
summary(Threat_model)

###GLM of Response to protection####
Protection_model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$EEZ_Protected, family = "binomial", data = Policy_Analysis)
summary(Protection_model)


###GLM of Response to presence of biodiversity 
Biodiversity_model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$Richness + Policy_Analysis$OHI, family = "binomial", data = Policy_Analysis)
summary(Biodiversity_model)




#####################################
#####SOCIO-POLITICAL VALUES##########
#####################################

### All Variables ###

Policy_model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2 + log(Policy_Analysis$GDP) + Policy_Analysis$Rents, family = "binomial", data = Policy_Analysis)
summary(Policy_model)

### getting a warning message: "glm.fit: fitted probabilities numerically 0 or 1 occurred" - log transformed GDP, no longer getting fit error

## Government Capacity###
Capacity_Model <- glm(Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2 + log(Policy_Analysis$GDP), family = "binomial", data = Policy_Analysis)
summary(Capacity_Model)

######WGI VARIABLES #########
### GLM of Response to World Governance Indicators ##
#WGI_model <- glm(WGI_Analysis$Presence_Absence ~ WGI_Analysis$Control.of.Corruption + WGI_Analysis$Government.Effectiveness + WGI_Analysis$Political.Stability.Absense.of.Violence + WGI_Analysis$Regulatory.Quality + WGI_Analysis$Rule.of.Law + WGI_Analysis$Voice.Accountability, family = "binomial", data = WGI_Analysis)
#summary(WGI_model)

#WGI_Analysis$STAGE = factor(WGI_Analysis$STAGE, levels = c("None", "Potential", "Preliminary", "In Progress", "Established") )


#WGI_Stage <- polr(WGI_Analysis$STAGE ~  WGI_Analysis$Control.of.Corruption + WGI_Analysis$Government.Effectiveness +
                   # WGI_Analysis$Political.Stability.Absense.of.Violence + WGI_Analysis$Regulatory.Quality +
                   # WGI_Analysis$Rule.of.Law + WGI_Analysis$Voice.Accountability, data = WGI_Analysis, Hess = TRUE) 
#summary(WGI_Stage) 
#coeftest(WGI_Stage)

################################################################
################################
###Ordinal Logist Regression#### To describe or predict variables. OLR can id which variables predict the level 
################################ of involvement in offsets policy
################################################################

#Dividing data into training and test set
#Random sampling
#samplesize = .60*nrow(Policy_data)
#index = sample(seq_len(nrow(Policy_data)), size = samplesize)
#Creating training and test set 
#datatrain <- Policy_Analysis[index,]
#datatest <- Policy_Analysis[-index,]


##################################
## Build ordinal logistic regression model
##################################

### Stage of Policy vs. ALL values ###

Policy_Analysis$STAGE2 = factor(Policy_Analysis$STAGE2, levels = c("0", "1", "2", "3", "4") )

All_Stage <- polr(Policy_Analysis$STAGE2 ~  Policy_Analysis$mam_bird +Policy_Analysis$EEZ_Protected +
                      Policy_Analysis$OHI + log(Policy_Analysis$GDP) + 
                      Policy_Analysis$Polity2 + Policy_Analysis$Richness, data = Policy_Analysis, Hess = TRUE) ##DRopped Richness and NRR not significant
summary(All_Stage) 
coeftest(All_Stage)

#confidence intervals
ci_All <- confint(All_Stage)


#After building the model and interpreting the model, the next step is to evaluate it. 
  #The evaluation of the model is conducted on the test dataset. A basic evaluation approach is to compute the confusion matrix and the misclassification error. 

#Compute confusion table and misclassification error - model predicts 0,1 bette
predictstage = predict(All_Stage,Policy_Analysis)
table(Policy_Analysis$STAGE2, predictstage)
mean(as.character(Policy_Analysis$STAGE2) != 
       as.character(predictstage))





##############################################
### Stage of Policy vs.  politcal values ###
##############################################

#For predicting
#Dividing data into training and test set
#Random sampling
##samplesize = .60*nrow(Policy_Analysis)
##index = sample(seq_len(nrow(Policy_Analysis)), size = samplesize)
#Creating training and test set 
##datatrain <- Policy_Analysis[index,]
##datatest <- Policy_Analysis[-index,]

Policy_Analysis$STAGE2 = factor(Policy_Analysis$STAGE2, levels = c("0", "1", "2", "3", "4") )

Policy_Stage <- polr(STAGE2 ~  log(GDP) + Polity2, data = Policy_Analysis, Hess = TRUE)

summary(Policy_Stage)
coeftest(Policy_Stage)

#After building the model and interpreting the model, the next step is to evaluate it. 
#The evaluation of the model is conducted on the test dataset. A basic evaluation approach is to compute the confusion matrix and the misclassification error. 

#For predicting
#Compute confusion table and misclassification error - model predicts lo
#predictstage2 = predict(Policy_Stage,Policy_Analysis)
#table(Policy_Analysis$STAGE2, predictstage2)
#mean(as.character(Policy_Analysis$STAGE2) != as.character(predictstage2))



#################

#Plotting the effects ## this isn't working - getting log(GDP) predictor is not in the model?
library("effects")
Effect(focal.predictors = "log(GDP)",Policy_Stage)
plot(Effect(focal.predictors = "log(GDP)",Policy_Stage))
plot(Effect(focal.predictors = c("log(GDP)", "Polity2)",Policy_Stage)))

###
boxplot(Policy_Analysis$STAGE ~ Policy_Analysis$EEZ_Protected + Policy_Analysis$Polity2 + Policy_Analysis$GDP + Policy_Analysis$OHI, data = Policy_Analysis, main = "Stage vs Variables", xlab="Variables", ylab= "Stage")


##plot
boxplot(Policy_Analysis$Polity2 ~ Policy_Analysis$STAGE, data = Policy_Analysis, id.n=TRUE, main = "Polity2 Index vs Stage", xlab="Stage", ylab= "Polity2 score")


############
### GLM of OHI ### Why signigican in Policy model but not here?
OHI_Model <- glm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$OHI, family = binomial, data = Polity_Analysis)
summary (OHI_Model)
plot(OHI_Model)
boxplot(Policy_Analysis$OHI ~ Policy_Analysis$STAGE, data = Policy_Analysis, main = "Stage vs OHI", xlab="stage", ylab= "OHI")

##########
Model2 <- lm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2 + Policy_Analysis$GDP + Policy_Analysis$NRR + Policy_Analysis$OHI + Policy_Analysis$Land_Protected, data = Policy_Analysis)
summary(Model2)

######
Polity_model <- lm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2, data = Policy_Analysis)
summary (Polity_model)
plot(Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2, data = Policy_Analysis)
abline(Polity_model)

### lm of country vs. % EEZ Protected ####
fit2 <- lm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$EEZ_Protected, data = Policy_Analysis)
summary (fit2)
plot(Policy_Analysis$Presence_Absence ~ Policy_Analysis$EEZ_Protected, data = Policy_Analysis)
abline(fit2)

### plotting interactions ######
Int1 <- lm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2*Policy_Analysis$OHI, data = Policy_Analysis)
summary(Int1)

Int2 <- lm(formula = Policy_Analysis$Presence_Absence ~ Policy_Analysis$Polity2*Policy_Analysis$Species_risk, data = Policy_Analysis)
summary(Int2)
interaction.plot(Policy_Analysis$Presence_Absence, Policy_Analysis$Polity2, Policy_Analysis$Species_risk)


##################################################
############Marine Policies############# none including organisational
##################################################

## Marine Specific Policies ####

SubN_Pol <- Policy_Analysis %>%   ## Subsets marine specific subnational policies
  filter(Mar_subN == 3)
Nat_Pol <- Policy_Analysis %>%   ##subsets marine specific national policies
  filter(Mar_Nat ==3)
Mar_Pol <- full_join(Nat_Pol, SubN_Pol)  #### joins the two together

### Policies that are not marine specific, but include marine explicitly ####
Sub_exp <-Policy_Analysis %>%   ## Subsets marine explicit subnational policies
  filter(Mar_subN == 2)
Nat_exp <-Policy_Analysis %>%   ## Subsets marine explicit subnational policies
  filter(Mar_Nat == 2)

Mar_Exp <- full_join(Sub_exp, Nat_exp)

## Policies that include marine implicitly ###
Sub_imp <-Policy_Analysis %>%   ## Subsets marine implicit subnational policies
  filter(Mar_subN == 1)
Nat_imp <-Policy_Analysis %>%   ## Subsets marine implicit subnational policies
  filter(Mar_Nat == 1)
Supr_imp <- Policy_Analysis %>%   ## Subsets marine implicit subnational policies
  filter(Mar_supraN == 1)

Mar_Imp <- full_join (Sub_imp, Nat_imp)
Mar_Imp2 <- full_join (Mar_Imp, Supr_imp)

Mar_Inc <- full_join(Mar_Imp2, Mar_Exp)  ### Includes marine considered explicitly and implicitly

All_mar <- full_join(Mar_Inc, MarinePol) ## Include all marine policies, no matter how included

## Policies that don't include marine at all

No_mar <- Policy_Analysis %>%   ## Subsets no policies
  filter(Presence_Absence == 0)

##################################################
#########################
### Plots ###
#########################
##################################################


########Plot marine specific policies vs OHI #################
ggplot(data = Mar_Pol, aes (x=OHI, y = log(GDP)))+
  
  geom_point(data = Mar_Exp, aes(x=OHI, y = log(GDP)), size = 2, shape = 19, color = "green") +
  
  
  geom_point(data = Mar_Imp2, aes(x=OHI, y = log(GDP)), size = 2, color = "red") +
  
  geom_point(data = No_mar, aes(x=OHI, y = log(GDP)), size = 2, color = "grey50") +
  
  geom_point(data = Mar_Pol,aes(x=OHI, y = log(GDP)), size = 2, color = "blue") +
  geom_text(aes(label= COUNTRY),size = 2.5, color="black", hjust =1, vjust = .5) +
 
  
  
  xlab("Ocean Health Index") +
  ylab("logGDP") +
  
  scale_color_manual("Policy Type", limits = c("Marine Explicit", "Marine Implicit", "No Policy", "Marine Specific"), 
                     values = c("green", "red", "grey50", "blue")) +
  guides(colour = guide_legend(override.aes = list(pch = c(16,21), fill = c("green", "red", "grey50", "blue"))))
      
  

  #guides(color = guide_legend("Policy Type"), label = TRUE) +
  theme_classic()
          
#--------------------------

ggplot(data = Mar_Pol, aes (x=OHI, y = log(GDP)))+
  geom_point(data = Mar_Pol,aes(x=OHI, y = log(GDP)), size = 2, color = "blue") +
  geom_text(aes(label= COUNTRY),size = 2.5, color="black") +
  
  geom_point(data = Mar_Imp2, aes(x=OHI, y = log(GDP)), size = 2, color = "pink") +
  geom_text(aes(label= COUNTRY),size = 2.5, color="black") +
  
  geom_point(data = Mar_Exp, aes(x=OHI, y = log(GDP)), size = 2, color = "green") +
  geom_text(aes(label= COUNTRY),size = 2.5, color="black") +
  
  geom_point(data = No_mar, aes(x=OHI, y = log(GDP)), size = 2, color = "black") +
  geom_text(aes(label= COUNTRY),size = 2.5, color="black") +
  
  xlab("Ocean Health Index") +
  ylab("logGDP")+
  
  guides(color = guide_legend(title = "Policy Type")) +
  theme_classic()



#### Boxplot of Presence/Absence vs. Polity 2 - Y axis first then x (response)
boxplot(Policy_Analysis$Polity2 ~ Policy_Analysis$Presence_Absence, data = Policy_Analysis, main = "Polity2 Index", xlab="Policy", ylab= "Polity2 score")

boxplot(log(Policy_Analysis$GDP) ~ Policy_Analysis$Presence_Absence, data = Policy_Analysis, main = "Presence vs GDP", xlab="Presence", ylab= "log(GDP)")

boxplot(log(Policy_Analysis$EEZ_Protected) ~ Polity_Analysis$Presence_Absence, data = Policy_Analysis, main = "Presence vs EEZ", xlab="Presence", ylab= "log(EEZ)")

boxplot(Policy_Analysis$Richness ~ Policy_Analysis$STAGE, ylim= c(0,1), data = Policy_Analysis, main = "STAGE vs Richness", xlab="Stage", ylab= "Richness")


### plotting stage vs GDP - cut into quartiles
plot(Policy_Analysis$STAGE ~ log(Policy_Analysis$GDP), data = Policy_Analysis)
grid(2,2)
identify(log(Policy_Analysis$GDP), Policy_Analysis$STAGE, labels = row.names (Policy_Analysis$GI), plot = TRUE, tolerance = .25) #not working


plot(Policy_Analysis$Polity2~ log(Policy_Analysis$GDP), data = Policy_Analysis)
grid(2,2)

##ggplot
library("ggplot2")
library("scales")

#Policy_Analysis$GDP = as.numeric(levels(Policy_Analysis$GDP))[Policy_Analysis$GDP]


### POLITY 2 VS logGDP
ggplot(data=Policy_Analysis, aes(x=log(GDP), y=Polity2, color=Presence_Absence)) + 
  geom_point(size = .5) +
  geom_text(aes(label= COUNTRY),size = 2.5) +
  xlab("logGDP") +
  ylab("Polity 2 Score") +
  theme_classic() + xlim(20,31) +
  guides(color = guide_legend(title = "Policy Presence"))
  geom_hline(data = Policy_Analysis, aes(yintercept = as.numeric(0)), color= "red") + # code for quartile not working
  geom_vline(data = Policy_Analysis, aes(xintercept = as.numeric(median(log(Policy_Analysis$GDP))), color = "red")) #code for quartile not working
##+ panel.grid.major = element_line(colour = "firebrick", size = 4) - not working

###########################################################  
### GDP vs Richness, with NRR as size and Stage as color ### 
###########################################################  
  
  
ggplot(data = Policy_Analysis, aes (x=log(Policy_Analysis$Richness), y = log(GDP), color = Policy_Analysis$STAGE2))+
  geom_point(size = Policy_Analysis$Rents) +
  geom_text(aes(label= COUNTRY),size = 2.5) +
  xlab("Species Richness") +
  ylab("logGDP")+
  guides(color = guide_legend(title = "Policy Stage"))



  
  
  
  
  
  
#geom_vline(xintercept = as.numeric(median(log(Policy_Analysis$GDP))), color = "red"))

#3d Scatter plot

library(scatterplot3d)
attach(Policy_Analysis)
scatterplot3d(Policy_Analysis$STAGE, Policy_Analysis$GDP, Policy_Analysis$Polity2, main = "3D Scatterplot")
