setwd("C:/coding/r/dissertation")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(simplevis)
library(dplyr)
library(explore)
library(crank)

SurveyData <- read.csv("SurveyAnalysis_Subset_15SEP2021.csv",header=T,sep=",")

#Remove NAs
SurveyData <- na.omit(SurveyData)
attach(SurveyData) 

#Non-parametric tests 

#Mann-Whitney U
wilcox.test(S_tot ~ Vis_max, data=SurveyData)

#Friedman

#medians

median(S1_tot)
median(S2_tot)
median(S3_tot)

scenarios <- matrix(c(S1_tot,S2_tot,S3_tot),ncol=3)
friedman.test(scenarios)

# Parametric tests 

SurveyData %>% 
  explore(Vis_max)

SurveyData %>% 
  explore(S_tot)

#ANOVA 

# weightlost == S_tot (estimated marginal means)
# Diet == scenarios
# gender == Vis_max

#Tests of Between-Subjects Effects (working)

anova_bse <- aov(S_tot ~ Vis_max)
summary(anova_bse)

bxp <- ggboxplot(SurveyData, x = "Vis_max", y = "S_tot", add = "point")
bxp

ggqqplot(SurveyData, "S_tot", facet.by = "Vis_max")


#Tests of Within-Subjects Effects (broken)
res.aov <- anova_test(
  data = SurveyData, dv = scenarios, wid = Vis_max,
  within = S_tot
)

get_anova_table(res.aov)

#Split Vis_max is Yes or No

SurveyData2$Vis_max[SurveyData2$Vis_max == 0] <- "No"
SurveyData2$Vis_max[SurveyData2$Vis_max == 1] <- "Yes"
SurveyData2$VMfactor <- as.factor(SurveyData2$Vis_max)
levels(SurveyData2$VMfactor)

par(mar=c(1,1,1,1))

hist(SurveyData2$S_tot[SurveyData2$Vis_max=='No'],main='VisMax',xlab='SurveyData2$Vis_max')
hist(SurveyData2$S_tot[SurveyData2$Vis_max=='Yes'],main='VisMax',xlab='SurveyData2$Vis_max')


