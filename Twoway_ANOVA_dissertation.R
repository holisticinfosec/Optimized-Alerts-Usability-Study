setwd("C:/coding/r/dissertation")

survey <- read.csv("SurveyAnalysis_Subset_15SEP2021.csv",header=T,sep=",")

#Tell R we are using the survey dataset until further notice using attach.
#This means that 'Height' can be used instead of dietR$Height.
# weightlost == S_tot (estimated marginal means)
# Diet == Scenarios
# gender == Vis_max


survey <- na.omit(survey)
attach(survey) 
anova2 <- aov(weightlost~as.factor(gender)*as.factor(Diet),data=survey)

#calculate the weight lost by person (difference in weight before and after the diet) and add to the dataset.
dietR$weightlost<-pre.weight-weight6weeks
#attach again your data and remove the missing values.

attach(dietR)

#Carrying out the two-way ANOVA, telling R than Diet and gender are categorical using as.factor().


#Ask for the residuals (difference between each individual and their group mean).
#save the residuals of the anova in a separate object.
res<-residuals(anova2)

#Checking the normality of the residuals.
hist(res,main="Histogram of residuals",xlab="Residuals")

#The Levene's test for equality of variances is in the car package.  Load the additional library car.
library(car)
#If this command does not work, you will need to go to the Packages --> Install package(s) and select the UK (London)CRAN mirror.
#Then look for the package 'car' and click.  A lot of extra menus will download. Then try library(car) again.

#Carry out Levene's test.
leveneTest(weightlost~as.factor(gender)*as.factor(Diet),data=dietR)

#To see the ANOVA output use summary().
summary(anova2)

#To produce an interaction plot.  First give R the labels for gender.
# The factor command uses variable<-factor(variable,c(category numbers),labels=c(category names)).

gender<-factor(gender,c(0,1),labels=c('Female','Male'))


#There are lots of options for line colour, style and legend placing with an interaction plot.
#Use ?interaction.plot to find out more.

interaction.plot(Diet,gender,weightlost,type="b",col=c(2:3),leg.bty="o",leg.bg="beige",lwd=2,pch=c(18,24),xlab="Diet",ylab="Weight lost",main="Interaction plot")

#If there are significant results in the ANOVA, post hoc test should be carried out.
#To carry out Tukey's post hoc adjustments for the pairwise comparisons.
#If the interaction is NOT significant, interpret the main effects.
#if it is significant interpret the interaction post hoc tests only.
TukeyHSD(anova2)

#-------------------------------------------------------------------.
#If you prefer to split the file and carry out separate ANOVA's by gender follow these instructions.
#Splitting the data set into two subsets.
#Save the female data into a new dataset.
females<-split(dietR,gender)[[1]]
#Save the male data into a new dataset.
males<-split(dietR,gender)[[2]]
#Do two separate ANOVAs.
fitfemale<-aov(weightlost~as.factor(Diet),data=females)
fitmale<-aov(weightlost~as.factor(Diet),data=males)
#Do two separate Tukeys.
TukeyHSD(fitfemale)
TukeyHSD(fitmale)


