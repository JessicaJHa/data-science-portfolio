# Repeated Measure ANOVA in R Activity

# Install packages

## install.packages("fastR2")
install.packages("carData")

## Load Libraries

library("rcompanion")
library("fastR2")
library("car")

# Load in Data

bkfst= read.csv("C:/Users/j_els/OneDrive/Documents/Data Science coursework/Intermediate Statistics/Lesson 5/Data Sets used in L5/breakfast/breakfast.csv")
bkfst.head()

## Determine whether weight changes from baseline to follow up

# Data Wrangling

# subset the data to get rid of unnecessary rows and columns

bkfst1 <- bkfst[1:33,]

keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
bkfst2 <- bkfst1[keeps]
head(bkfst2)

# Baseline Data
# keeping only first5 columns that do not change by timepoint, to get data in right shape for baseline measurement

bkfst3 <- bkfst2[,1:5]

bkfst3$repdat <- bkfst2$Baseline.Body.Mass..kg.
bkfst3$contrasts <- "T1"
head(bkfst3)

# follow up data, getting in right shape for follow up measure

bkfst4 <- bkfst2[,1:5]

bkfst4$repdat <- bkfst2$Follow.Up.Body.Mass..kg.
bkfst4$contrasts <- "T2"
head(bkfst4)

# Then smoosh it all back together with binding

bkfst5 <- rbind(bkfst3, bkfst4)

# Testing for Normality

plotNormalHistogram(bkfst1$Baseline.Body.Mass..kg.)
plotNormalHistogram(bkfst1$Follow.Up.Body.Mass..kg.)

# They look approximately normally distributed, we'll take it and won't do any adjusting.

# Testing for Homogeneity of Variance

leveneTest(repdat ~ Treatment.Group*contrasts, data=bkfst5)

# It was not significant, which means this assumption has been met, in other words, the data passes the assumption of homogeneity of variance.

str(bkfst5)

# data frame contains 66 obs. which passes the assumption of sample size

# Analysis

RManova2 <- aov(repdat~contrasts+Error(Participant.Code), bkfst5)
summary(RManova2)

# looks like there is no significant effect of time on weight







