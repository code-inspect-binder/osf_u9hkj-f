#clear environment
rm(list = ls())

#load packages
library (tidyverse)
library (car)
library (dplyr)
library (psych)
library (rcompanion)
library (lmtest)
library (sandwich)
library (ggplot2)
library (cowplot)
library (here)
library(performance)
library (lm.beta)
library(stargazer)
library(corrplot)

#set working space
here()

#Load the survey data
expertsample<-read.table("expertsurvey_CDRCCS.txt", header=TRUE, sep="\t")
head(expertsample)

#Total number participants
nrow(expertsample)
#Number of DACCS experts
sum(expertsample$DACCS_cost1>=1.0, na.rm=TRUE)
#Number of BECCS experts
sum(expertsample$BECCS_cost1>=1.0, na.rm=TRUE)
#Number of experts on both BECCS and DACCS
nrow(filter(expertsample, DACCS_cost1>=1.0 & BECCS_cost1>=1.0))

#Transform academic background and job category variables from character to factor
class(expertsample$acadgroup)
expertsample$acadgroup<-as.factor(expertsample$acadgroup)

class(expertsample$job_cat)
expertsample$job_cat <- as.factor(expertsample$job_cat)

#Reversed items
expertsample <- mutate(expertsample, riskgeostor_1=6+1-riskgeostor_1, 
                       DACCS_biodiv2=6+1-DACCS_biodiv2, BECCS_biodiv2=6+1-BECCS_biodiv2, 
                       DACCS_tamp1=6+1-DACCS_tamp1, BECCS_tamp1=6+1-BECCS_tamp1, DACCS_nec2=6+1-DACCS_nec2, 
                       BECCS_nec2=6+1-BECCS_nec2)

#Check items for correlation 
subset_cor <- subset(expertsample, select=c(support_solbio, support_liqbio, 
                                            support_gasbio, riskgeostor_1, riskgeostor_2, 
                                            riskgeostor_3, techopt1, techopt2, techopt3, 
                                            techopt4, DACCS_cost1, DACCS_cost2, DACCS_biodiv1, 
                                            DACCS_biodiv2, DACCS_mor1, DACCS_mor2, DACCS_mor3, 
                                            DACCS_tamp1, DACCS_tamp2, DACCS_tamp3, DACCS_tamp4, 
                                            DACCS_feas1, DACCS_feas2, DACCS_nec1, DACCS_nec2, 
                                            DACCS_support_reas, DACCS_support_depl, BECCS_cost1, 
                                            BECCS_cost2, BECCS_biodiv1, BECCS_biodiv2, BECCS_mor1, 
                                            BECCS_mor2, BECCS_mor3, BECCS_tamp1, BECCS_tamp2, 
                                            BECCS_tamp3, BECCS_tamp4, BECCS_feas1, BECCS_feas2, 
                                            BECCS_nec1, BECCS_nec2, BECCS_support_reas, BECCS_support_depl))
cor <-cor(subset_cor, use="complete.obs")
cor

corrplot<-corrplot(cor, method="number") #second approach

###Reliability analysis
#support for bioenergy
psych::alpha(subset(expertsample, select=c(support_solbio, support_liqbio, support_gasbio)))
#risk geological storage
psych::alpha(subset(expertsample, select=c(riskgeostor_1, riskgeostor_2, riskgeostor_3)))
#technological optimism 
psych::alpha(subset(expertsample, select=c(techopt1, techopt2, techopt3, techopt4)))

#DACCS costs
psych::alpha(subset(expertsample, select=c(DACCS_cost1, DACCS_cost2)), check.keys=TRUE)
#DACCS biodiversity
psych::alpha(subset(expertsample, select=c(DACCS_biodiv1, DACCS_biodiv2)), check.keys=TRUE)
#DACCS moral hazard
psych::alpha(subset(expertsample, select=c(DACCS_mor1, DACCS_mor2, DACCS_mor3)), check.keys=TRUE)
#DACCS tampering with nature
psych::alpha(subset(expertsample, select=c(DACCS_tamp1, DACCS_tamp2, DACCS_tamp3, DACCS_tamp4)), check.keys=TRUE)
#DACCS feasibility
psych::alpha(subset(expertsample, select=c(DACCS_feas1, DACCS_feas2)), check.keys=TRUE)
#DACCS necessity
psych::alpha(subset(expertsample, select=c(DACCS_nec1, DACCS_nec2)), check.keys=TRUE)
#DACCS support 
psych::alpha(subset(expertsample, select=c(DACCS_support_reas, DACCS_support_depl)), check.keys=TRUE)

#BECCS costs
psych::alpha(subset(expertsample, select=c(BECCS_cost1, BECCS_cost2)), check.keys=TRUE)
#BECCS biodiversity
psych::alpha(subset(expertsample, select=c(BECCS_biodiv1, BECCS_biodiv2)), check.keys=TRUE)
#BECCS moral hazard
psych::alpha(subset(expertsample, select=c(BECCS_mor1, BECCS_mor2, BECCS_mor3)), check.keys=TRUE)
#BECCS tampering with nature
psych::alpha(subset(expertsample, select=c(BECCS_tamp1, BECCS_tamp2, BECCS_tamp3, BECCS_tamp4)), check.keys=TRUE)
#BECCS feasibility
psych::alpha(subset(expertsample, select=c(BECCS_feas1, BECCS_feas2)), check.keys=TRUE)
#BECCS necessity
psych::alpha(subset(expertsample, select=c(BECCS_nec1, BECCS_nec2)), check.keys=TRUE)
#BECCS support 
psych::alpha(subset(expertsample, select=c(BECCS_support_reas, BECCS_support_depl)), check.keys=TRUE)

###Create factors
#support for bioenergy 
expertsample$support_bio <- rowMeans(subset(expertsample, select=c(support_solbio, support_liqbio, support_gasbio)),na.rm=TRUE)
#risk geological storage
expertsample$riskgeostor <- rowMeans(subset(expertsample, select=c(riskgeostor_1, riskgeostor_2, riskgeostor_3)),na.rm=TRUE)
#technological optimism
expertsample$techopt <- rowMeans(subset(expertsample, select=c(techopt1, techopt2, techopt3, techopt4)),na.rm=TRUE)
#DACCS costs
expertsample$DACCS_cost <- rowMeans(subset(expertsample, select=c(DACCS_cost1, DACCS_cost2)),na.rm=TRUE)
#DACCS biodiversity
expertsample$DACCS_biodiv <- rowMeans(subset(expertsample, select=c(DACCS_biodiv1)),na.rm=TRUE)
#DACCS moral hazard
expertsample$DACCS_mor <- rowMeans(subset(expertsample, select=c(DACCS_mor1, DACCS_mor2, DACCS_mor3)),na.rm=TRUE)
#DACCS tampering with nature
expertsample$DACCS_tamp <- rowMeans(subset(expertsample, select=c(DACCS_tamp1, DACCS_tamp2, DACCS_tamp3, DACCS_tamp4)),na.rm=TRUE)
#DACCS feasibility
expertsample$DACCS_feas <- rowMeans(subset(expertsample, select=c(DACCS_feas1, DACCS_feas2)),na.rm=TRUE)
#DACCS necessity
expertsample$DACCS_nec <- rowMeans(subset(expertsample, select=c(DACCS_nec1, DACCS_nec2)),na.rm=TRUE)
#DACCS support
expertsample$DACCS_support <- rowMeans(subset(expertsample, select=c(DACCS_support_reas, DACCS_support_depl)),na.rm=TRUE)

#BECCS costs
expertsample$BECCS_cost <- rowMeans(subset(expertsample, select=c(BECCS_cost1, BECCS_cost2)),na.rm=TRUE)
#BECCS biodiversity
expertsample$BECCS_biodiv <- rowMeans(subset(expertsample, select=c(BECCS_biodiv1)),na.rm=TRUE)
#BECCS moral hazard
expertsample$BECCS_mor <- rowMeans(subset(expertsample, select=c(BECCS_mor1, BECCS_mor2, BECCS_mor3)),na.rm=TRUE)
#BECCS tampering with nature
expertsample$BECCS_tamp <- rowMeans(subset(expertsample, select=c(BECCS_tamp1, BECCS_tamp2, BECCS_tamp3, BECCS_tamp4)),na.rm=TRUE)
#BECCS feasibility
expertsample$BECCS_feas <- rowMeans(subset(expertsample, select=c(BECCS_feas1, BECCS_feas2)),na.rm=TRUE)
#BECCS necessity
expertsample$BECCS_nec <- rowMeans(subset(expertsample, select=c(BECCS_nec1, BECCS_nec2)),na.rm=TRUE)
#BECCS support
expertsample$BECCS_support <- rowMeans(subset(expertsample, select=c(BECCS_support_reas, BECCS_support_depl)),na.rm=TRUE)

#create sample for the wilcoxon tests (exclude experts who were experts on only one technology)
bothexpert <- filter(expertsample, DACCS_cost>=1.0 & BECCS_cost>=1.0)

##graphical check for normal distribution of differences 
#support (mean)
diff_support <- bothexpert$BECCS_support-bothexpert$DACCS_support
hist(diff_support)
#support deployment
diff_support_depl <- bothexpert$BECCS_support_depl-bothexpert$DACCS_support_depl
hist(diff_support_depl)
#support research
diff_support_reas <- bothexpert$BECCS_support_reas-bothexpert$DACCS_support_reas
hist(diff_support_reas)
#necessity
diff_nec <- bothexpert$BECCS_nec-bothexpert$DACCS_nec
hist(diff_nec)
#feasibility
diff_feas <- bothexpert$BECCS_feas-bothexpert$DACCS_feas
hist(diff_feas)
#tampering with nature
diff_tamp <- bothexpert$BECCS_tamp-bothexpert$DACCS_tamp
hist(diff_tamp)
#moral hazard
diff_mor <- bothexpert$BECCS_mor-bothexpert$DACCS_mor
hist(diff_mor)
#cost-efficiency
diff_cost <- bothexpert$BECCS_cost-bothexpert$DACCS_cost
hist(diff_cost)
#biodiversity-friendliness
diff_biodiv <- bothexpert$BECCS_biodiv-bothexpert$DACCS_biodiv
hist(diff_biodiv)

##Wilcoxon tests
#support (mean)
wilcox.test(bothexpert$BECCS_support, bothexpert$DACCS_support, paired=TRUE)
#support deployment
wilcox.test(bothexpert$BECCS_support_depl, bothexpert$DACCS_support_depl, paired=TRUE)
#support research
wilcox.test(bothexpert$BECCS_support_reas, bothexpert$DACCS_support_reas, paired=TRUE)
#support research
wilcox.test(bothexpert$BECCS_support_reas, bothexpert$DACCS_support_reas, paired=TRUE)
#necessity
wilcox.test(bothexpert$BECCS_nec, bothexpert$DACCS_nec, paired=TRUE)
#feasibility
wilcox.test(bothexpert$BECCS_feas, bothexpert$DACCS_feas, paired=TRUE)
#tampering with nature
wilcox.test(bothexpert$BECCS_tamp, bothexpert$DACCS_tamp, paired=TRUE)
#moral hazard 
wilcox.test(bothexpert$BECCS_mor, bothexpert$DACCS_mor, paired=TRUE) #significant
bothexpert_long_mor <- pivot_longer(bothexpert, c (BECCS_mor, DACCS_mor)) #To analyze the effect size the data has to be in long format 
wilcoxonPairedR(x=bothexpert_long_mor$value, g=bothexpert_long_mor$name)
bothexpert %>% summarise(meanBECCS=mean(BECCS_mor), sdBECCS=sd(BECCS_mor), meanDACCS=mean(DACCS_mor), sdDACCS=sd(DACCS_mor))%>%round(2)
#cost-efficiency 
wilcox.test(bothexpert$BECCS_cost, bothexpert$DACCS_cost, paired=TRUE) #significant
bothexpert_long_cost <- pivot_longer(bothexpert, c (BECCS_cost, DACCS_cost)) #To analyze the effect size the data has to be in long format 
wilcoxonPairedR(x=bothexpert_long_cost$value, g=bothexpert_long_cost$name)
bothexpert %>% summarise(meanBECCS=mean(BECCS_cost), sdBECCS=sd(BECCS_cost), meanDACCS=mean(DACCS_cost), sdDACCS=sd(DACCS_cost))%>%round(2)
#biodiversity-friendliness 
wilcox.test(bothexpert$BECCS_biodiv, bothexpert$DACCS_biodiv, paired=TRUE) #significant
bothexpert_long_biodiv <- pivot_longer(bothexpert, c (BECCS_biodiv, DACCS_biodiv)) #To analyze the effect size the data has to be in long format 
wilcoxonPairedR(x=bothexpert_long_biodiv$value, g=bothexpert_long_biodiv$name)
bothexpert %>% summarise(meanBECCS=mean(BECCS_biodiv), sdBECCS=sd(BECCS_biodiv), meanDACCS=mean(DACCS_biodiv), sdDACCS=sd(DACCS_biodiv))%>%round(2)

##create summary table with mean and sd

stargazer(bothexpert, digits= 2,nobs=FALSE,type="text")

#Create a data frame with all variables relevant for the ANOVA
BECCS_sample <- filter(expertsample, BECCS_biodiv >=1.0 & !is.na (acadgroup)) %>% select(c(ID, acadgroup, BECCS_biodiv, BECCS_cost, BECCS_mor, BECCS_tamp, BECCS_feas, BECCS_nec, BECCS_support, BECCS_support_reas, BECCS_support_depl))
#group sizes of experts regarding BECCS (check whether group sizes still big enough)
table(BECCS_sample$acadgroup)
sum(table(BECCS_sample$acadgroup)) #n = 146

BECCS_long <- pivot_longer(BECCS_sample, c(BECCS_biodiv, BECCS_cost, BECCS_mor, BECCS_tamp, BECCS_feas, BECCS_nec, BECCS_support, BECCS_support_reas, BECCS_support_depl), names_to="variable", values_to= "agreement")

#Reorder the variable for the boxplot
BECCS_long$variable <-  as.factor(BECCS_long$variable)
BECCS_long$variable <- factor(BECCS_long$variable, c("BECCS_biodiv", "BECCS_cost", "BECCS_mor","BECCS_tamp", "BECCS_feas", "BECCS_nec", "BECCS_support_depl", "BECCS_support_reas", "BECCS_support"))

#Boxplot
BECCSplot<- ggplot()+                                                                                                         
  geom_boxplot(data=BECCS_long, aes(x=variable, y=agreement, fill=acadgroup))+coord_flip()+                              
  scale_x_discrete("factors", labels=c(BECCS_tamp="tampering with nature",BECCS_biodiv="biodiversity-friendliness",BECCS_cost="cost-efficiency",BECCS_feas="feasibility",BECCS_mor="moral hazard",BECCS_nec="necessity", BECCS_support_depl="support (deployment)", BECCS_support="support(mean)", BECCS_support_reas="support (research)"))+
  scale_y_continuous("agreement/support",breaks=c(1,2,3,4,5,6))+                                                            
  scale_fill_brewer(palette="Set2", name="academic groups:")+
  theme_light()+                                                                                                    
  theme(text=element_text(size=14), plot.title=element_text(hjust=0.5))+                                                                                         
  labs(title="BECCS")
BECCSplot

ggsave(file="boxplot_BECCS.jpg", width=7, height=8, dpi=600)

#significant
anova_BECCS_biodiv <-aov(BECCS_sample$BECCS_biodiv~ BECCS_sample$acadgroup)
summary(anova_BECCS_biodiv)
plot(anova_BECCS_biodiv, 2)
leveneTest(BECCS_sample$BECCS_biodiv, BECCS_sample$acadgroup)
pairwise.t.test(BECCS_sample$BECCS_biodiv, BECCS_sample$acadgroup, p.adjust.method="bonf") 
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_biodiv), 2), sd=round(sd(BECCS_biodiv), 2))

#significant
anova_BECCS_cost <-aov(BECCS_sample$BECCS_cost~ BECCS_sample$acadgroup)
summary(anova_BECCS_cost)
plot(anova_BECCS_cost, 2)
leveneTest(BECCS_sample$BECCS_cost, BECCS_sample$acadgroup)
pairwise.t.test(BECCS_sample$BECCS_cost, BECCS_sample$acadgroup, p.adjust.method="bonf") 
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_cost), 2), sd=round(sd(BECCS_cost), 2))

#not significant
anova_BECCS_mor <-aov(BECCS_sample$BECCS_mor~ BECCS_sample$acadgroup)
summary(anova_BECCS_mor)
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_mor), 2), sd=round(sd(BECCS_mor), 2))

#not significant
anova_BECCS_tamp <-aov(BECCS_sample$BECCS_tamp~ BECCS_sample$acadgroup)
summary(anova_BECCS_tamp)
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_tamp), 2), sd=round(sd(BECCS_tamp), 2))

#significant
anova_BECCS_feas <-aov(BECCS_sample$BECCS_feas~ BECCS_sample$acadgroup)
summary(anova_BECCS_feas)
plot(anova_BECCS_feas, 2)
leveneTest(BECCS_sample$BECCS_feas, BECCS_sample$acadgroup)
pairwise.t.test(BECCS_sample$BECCS_feas, BECCS_sample$acadgroup, p.adjust.method="bonf")
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_feas), 2), sd=round(sd(BECCS_feas), 2))

#not significant
anova_BECCS_nec <-aov(BECCS_sample$BECCS_nec~ BECCS_sample$acadgroup)
summary(anova_BECCS_nec)
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_nec), 2), sd=round(sd(BECCS_nec), 2))

#not significant
anova_BECCS_support <-aov(BECCS_sample$BECCS_support~BECCS_sample$acadgroup)
summary(anova_BECCS_support)
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_support), 2), sd=round(sd(BECCS_support), 2))

#not significant
anova_BECCS_support_reas <-aov(BECCS_sample$BECCS_support_reas~BECCS_sample$acadgroup)
summary(anova_BECCS_support_reas)
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_support_reas), 2), sd=round(sd(BECCS_support_reas), 2))

#slightly significant
anova_BECCS_support_depl <-aov(BECCS_sample$BECCS_support_depl~BECCS_sample$acadgroup)
summary(anova_BECCS_support_depl)
plot(anova_BECCS_support_depl, 2)
leveneTest(BECCS_sample$BECCS_support_depl, BECCS_sample$acadgroup)
pairwise.t.test(BECCS_sample$BECCS_support_depl, BECCS_sample$acadgroup, p.adjust.method="bonf")
#mean values
BECCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(BECCS_support_depl), 2), sd=round(sd(BECCS_support_depl), 2))

#Create a data frame with all variables relevant for the ANOVA
DACCS_sample <- filter(expertsample, DACCS_biodiv >=1.0 & !is.na(acadgroup)) %>% select(c(ID,acadgroup, DACCS_biodiv, DACCS_cost, DACCS_mor, DACCS_tamp, DACCS_feas, DACCS_nec, DACCS_support, DACCS_support_reas, DACCS_support_depl))
#group sizes of experts regarding DACCS
table(DACCS_sample$acadgroup)

DACCS_long <- pivot_longer(DACCS_sample, c(DACCS_biodiv, DACCS_cost, DACCS_mor, DACCS_tamp, DACCS_feas, DACCS_nec, DACCS_support, DACCS_support_reas, DACCS_support_depl), names_to="variable", values_to= "agreement")

#Reorder the variable for the boxplot
DACCS_long$variable <-  as.factor(DACCS_long$variable)
DACCS_long$variable <- factor(DACCS_long$variable, c("DACCS_biodiv", "DACCS_cost", "DACCS_mor","DACCS_tamp", "DACCS_feas", "DACCS_nec", "DACCS_support_depl", "DACCS_support_reas", "DACCS_support"))

#boxplot
DACCSplot <- ggplot()+                                                                                                         
  geom_boxplot(data=DACCS_long, aes(x=variable, y=agreement, fill=acadgroup))+coord_flip()+                              
  scale_x_discrete("factors", labels=c(DACCS_tamp="tampering with nature",DACCS_biodiv="biodiversity-friendliness",DACCS_cost="cost-efficiency",DACCS_feas="feasibility",DACCS_mor="moral hazard",DACCS_nec="necessity", DACCS_support_depl="support (deployment)", DACCS_support="support(mean)", DACCS_support_reas="support (research)"))+
  scale_y_continuous("agreement/support",breaks=c(1,2,3,4,5,6))+                                                            
  scale_fill_brewer(palette="Set2", name="academic groups:", labels=c("Engineering and Technology", "Natural Sciences", "Social Sciences and Economics"))+
  theme_light()+                                                                                                    
  theme(text=element_text(size=14), plot.title=element_text(hjust=0.5))+                                                                             
  labs(title="DACCS")
DACCSplot

ggsave(file="boxplot_DACCS.jpg", width=7, height=8, dpi=600)

#One way ANOVAs for DACCS
anova_DACCS_biodiv <-aov(DACCS_sample$DACCS_biodiv~ DACCS_sample$acadgroup)
summary(anova_DACCS_biodiv)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_biodiv), 2), sd=round(sd(DACCS_biodiv), 2))

anova_DACCS_cost <-aov(DACCS_sample$DACCS_cost~ DACCS_sample$acadgroup)
summary(anova_DACCS_cost)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_cost), 2), sd=round(sd(DACCS_cost), 2))

anova_DACCS_mor <-aov(DACCS_sample$DACCS_mor~ DACCS_sample$acadgroup)
summary(anova_DACCS_mor)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_mor), 2), sd=round(sd(DACCS_mor), 2))

anova_DACCS_tamp <-aov(DACCS_sample$DACCS_tamp~ DACCS_sample$acadgroup)
summary(anova_DACCS_tamp)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_tamp), 2), sd=round(sd(DACCS_tamp), 2))

anova_DACCS_feas <-aov(DACCS_sample$DACCS_feas~ DACCS_sample$acadgroup)
summary(anova_DACCS_feas)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_feas), 2), sd=round(sd(DACCS_feas), 2))

anova_DACCS_nec <-aov(DACCS_sample$DACCS_nec~ DACCS_sample$acadgroup)
summary(anova_DACCS_nec)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_nec), 2), sd=round(sd(DACCS_nec), 2))

anova_DACCS_support <-aov(DACCS_sample$DACCS_support~DACCS_sample$acadgroup)
summary(anova_DACCS_support)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_support), 2), sd=round(sd(DACCS_support), 2))

anova_DACCS_support_reas <-aov(DACCS_sample$DACCS_support_reas~DACCS_sample$acadgroup)
summary(anova_DACCS_support_reas)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_support_reas), 2), sd=round(sd(DACCS_support_reas), 2))

anova_DACCS_support_depl <-aov(DACCS_sample$DACCS_support_depl~DACCS_sample$acadgroup)
summary(anova_DACCS_support_depl)
#mean values
DACCS_sample %>% group_by(acadgroup) %>% summarise(mean=round(mean(DACCS_support_depl), 2), sd=round(sd(DACCS_support_depl), 2))

jointfac <- filter(expertsample, !is.na (acadgroup))
jointfac_long <-pivot_longer(jointfac, c(support_bio, riskgeostor, techopt), names_to="variable", values_to= "agreement")

#group sizes of experts regarding DACCS
table(jointfac$acadgroup)

#Reorder the variables for the boxplot
jointfac_long$variable <-  as.factor(jointfac_long$variable)
jointfac_long$variable <- factor(jointfac_long$variable, c("riskgeostor", "techopt", "support_bio"))

#boxplot
Jointplot <- ggplot()+                                                                                                         
  geom_boxplot(data=jointfac_long, aes(x=variable, y=agreement, fill=acadgroup))+coord_flip()+                              
  scale_x_discrete("factors", labels=c(riskgeostor="risk of geological storage", techopt="technological optimism", support_bio="support for bioenergy"))+
  scale_y_continuous("agreement/support",breaks=c(1,2,3,4,5,6))+                                                            
  scale_fill_brewer(palette="Set2", name="academic groups:")+
  theme_light()+                                                                                                    
  theme(text=element_text(size=14),plot.title=element_text(hjust=0.5))+                                                                                         
  labs(title="Technology-independent factors")
Jointplot

ggsave(file="boxplot_joint.jpg", width=10, height=4, dpi=600)

#significant
anova_techopt <-aov(jointfac$techopt~ jointfac$acadgroup)
summary(anova_techopt)
plot(anova_techopt, 2)
leveneTest(jointfac$techopt, jointfac$acadgroup)
pairwise.t.test(jointfac$techopt, jointfac$acadgroup, p.adjust.method="bonf")
#mean values
jointfac %>% group_by(acadgroup) %>% summarise(mean=round(mean(techopt),2), sd=round(sd(techopt),2))


#significant
anova_support_bio <-aov(jointfac$support_bio ~ jointfac$acadgroup)
summary(anova_support_bio)
plot(anova_support_bio, 2)
leveneTest(jointfac$support_bio, jointfac$acadgroup)
pairwise.t.test(jointfac$support_bio, jointfac$acadgroup, p.adjust.method="bonf")
#mean values
jointfac %>% group_by(acadgroup) %>% summarise(mean=round(mean(support_bio),2), sd=round(sd(support_bio),2))

#not significant
anova_riskgeostor <-aov(jointfac$riskgeostor~ jointfac$acadgroup)
summary(anova_riskgeostor)
#mean values
jointfac %>% group_by(acadgroup) %>% summarise(mean=round(mean(riskgeostor),2), sd=round(sd(riskgeostor),2))

combinedplot <- plot_grid(BECCSplot + theme(legend.position="none"), DACCSplot + theme(legend.position="none"))
combinedplot

legend <- get_legend(DACCSplot+theme(legend.position="bottom"))
BECCSDACCSplot <- plot_grid(combinedplot, legend, ncol=1, rel_heights=c(1,.1))
BECCSDACCSplot

ggsave("BECCS&DACCSplot.jpg",width=12, height=8, dpi=600)

###REGRESSION
BECCS_reg <- expertsample %>% filter(BECCS_biodiv >=1.0) %>% 
  rename("tamp"=BECCS_tamp,"biodiv"=BECCS_biodiv,"cost"=BECCS_cost,"feas"=BECCS_feas,"mor"=BECCS_mor,"nec"=BECCS_nec) %>% 
  select (c(ID, job_cat, support_bio, riskgeostor, techopt,biodiv, cost,mor,tamp,feas, nec, BECCS_support))

BECCS_model <- lm(BECCS_support ~ relevel(job_cat, "Prof") + support_bio+techopt+riskgeostor+tamp+biodiv+cost+mor+feas+nec, data=BECCS_reg) 
summary(BECCS_model)
lm.beta(BECCS_model)
nrow(model.frame(BECCS_model))
table(is.na(BECCS_reg))

vif(BECCS_model) # no multicollinearity
mean(vif(BECCS_model))

bptest(BECCS_model) #no heteroscedasticity 
#BECCS_model_robust <- coeftest(BECCS_model, vcov = vcovHC(BECCS_model, type = "HC3"))
#BECCS_model_robust

DACCS_reg <- expertsample %>% filter(DACCS_biodiv >=1.0) %>% 
  rename("tamp"=DACCS_tamp,"biodiv"=DACCS_biodiv,"cost"=DACCS_cost,"feas"=DACCS_feas,"mor"=DACCS_mor,"nec"=DACCS_nec) %>% 
  select (c(ID, job_cat, support_bio, riskgeostor, techopt,biodiv, cost,mor,tamp,feas, nec, DACCS_support))

DACCS_model <- lm(DACCS_support ~ relevel(job_cat, "Prof") + support_bio+techopt+riskgeostor+cost+biodiv+mor+tamp+feas+nec, data=DACCS_reg)
summary(DACCS_model)
lm.beta(DACCS_model)
nrow(model.frame(DACCS_model))
table(is.na(DACCS_reg))

vif(DACCS_model) # no multicollinearity
mean(vif(DACCS_model))

bptest(DACCS_model) #heteroscedasticity
DACCS_model_robust <- coeftest(DACCS_model, vcov = vcovHC(DACCS_model, type = "HC3"))
DACCS_model_robust
