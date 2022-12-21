
# Install Packages --------------------------------------------------------

install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("multcomp")
install.packages("pastecs")
install.packages("akima")
install.packages("WRS", repos="http://R-Forge.R-project.org")
install.packages("reshape")

# Libraries ---------------------------------------------------------------

library(pastecs) # for stat.desc function
library(car) # for leveneTest function
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(akima)
library(WRS)
library(reshape)
library(rstatix)
library(WRS2)

# Data ---------------------------------------------------------------

# The below sample data indicates the counts of terrorist attacks in Bogota Province and all other provinces in Colombia for the following three groups: FARC, ELN and combined OtherGroup

BogotaProvince <- c(3, 2, 5, 2, 2, 2, 7, 2, 4, 7, 5, 3, 4, 4, 7, 5, 4, 9, 2, 6, 3, 4, 4, 4, 6, 4, 6, 2, 8, 5)
OtherProvince <- c(4, 1, 5, 1, 2, 2, 7, 4, 5, 5, 3, 1, 2, 2, 6, 4, 2, 1, 3, 5, 4, 3, 3, 2, 0, 1, 3, 0, 1, 0)
Group <- c(rep(1, 9),rep(2, 8), rep(3, 13))
Group <- factor(Group, levels = c(1:3), labels = c("FARC", "ELN", "OtherGroup"))
TerroristData <- data.frame(Group, BogotaProvince, OtherProvince)

# Descriptive Statistics --------------------------------------------------

by(TerroristData$BogotaProvince, TerroristData$Group, stat.desc)
by(TerroristData$OtherProvince, TerroristData$Group, stat.desc)

# Assumption Checks -------------------------------------------------------

############
# Variance #
############

leveneTest(TerroristData$BogotaProvince, 
           TerroristData$Group, 
           center = "median") # compute the center of each group. Median is more robust than mean

?leveneTest
# Levene's Test for Homogeneity of Variance (center = median)
# Df F value Pr(>F)
# group  2  0.3256 0.7249
# 27  

# The above output is very insignificant (p-value: 0.7249), so the variances are very similar. Had this output been significant, we could conduct a robust ANOVA test
# A good double check for Levene's Test is to examine the highest and lowest group variances. For TerroristData$BogotaProvince, there are standard deviations of 1.79, 1.46 and 2.12, for FARC, ELN and OtherGroup respectively. When we square these values, we get variances of 3.20, 2.13, and 4.49 for FARC, ELN and OtherGroup respectively. We then take the highest variance and divide it by the smallest (4.49/2.13 = 2.11). The approximate critical value for Hartley's F max test for three variances and with roughly ten people per group is 5. The observed value of 2.11 is less than 5, so we don't need to worry about variances.

#####################################################
# Are Predictor Variable and Covariate independent? #
#####################################################

# OtherProvince is the covariate, so we need to check this variable is roughly equal across levels of the independent variable called Group. In other words, is the mean level of OtherProvince roughly equal across the three trrorist groups? This can be tested through an ANOVA test with OtherProvince as the outcome and Group as the predictor.

checkIndependenceModel<-aov(OtherProvince ~ Group, data = TerroristData)
summary(checkIndependenceModel)

# Df Sum Sq Mean Sq F value Pr(>F)
# dose         2  12.77   6.385   1.979  0.158
# Residuals   27  87.10   3.226  

# In the above output, the main effect of dose is not significant (F, 2, 27) = 1.98, p.value = 0.16, thus indicating that the average levels of OtherProvince count is roughly the same across three terrorist groups. This means it is appropriate to use OtherProvince as covariate in analysis

summary.lm(checkIndependenceModel)

# Fitting an ANCOVA Model -------------------------------------------------

# We use the aov() function

TerroristModel <- aov(BogotaProvince ~ OtherProvince + Group, data = TerroristData)
# It is important to put the variate and covariate before the variable Group. This order is necessary as we want the covariate OtherProvince to be evaluated after the predictor BogotaProvince, but before the depedendent variable called Group
summary(TerroristModel)
# The resulting aov table is as follows:

# Df Sum Sq Mean Sq F value Pr(>F)  
# Group          2  16.84   8.422   2.770 0.0812 .
# OtherProvince  1  15.08  15.076   4.959 0.0348 *
#   Residuals     26  79.05   3.040                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# An alternative is to use type 111 sums of squares, as follows:

Anova(TerroristModel, type = "III")

# Contrasts ---------------------------------------------------------------

# After completing Anova tests, it is neccessary to carry out further analysis to determine the differences in means between the different groups or levels in the categorical variables.
# This is where contrasts come in.
# Because we are using type III sum of squares, we must set orthagonal contrasts, which can be achieved through using helmet contrasts or by setting our own contrasts.
# There are several rules regarding contrast weightings, including:
# Groups coded with positive weights are compared against groups with negative weights. One chunk of variation
# has positive weight, while the other has negative weight. The sum of the weights should be zero, Where a group is not included in the variation, it must be assigned a weight of zero.
# For any give contrast, the sum of the weights assigned to the group(s) assigned in one chunk of variation must equal the number of groups in the opposite chunk of variation.
# Therefore, the weights are determined as follows:
# The first chunk of variation comprises ELN and OtherGroup, while the second chunk comprises FARC. One chunk must be positive and the other negative, whilst it doesn't matter the order. Let's assign chunk1 positive weights and chunk two negative weights. Furthermore, the weights we assign to chunk1 groups must be equivalent to the groups in chunk2. As there is only FARC in group2, then we assign each group in chunk1, a value of one. Additionally, FARC is assigned a weight of two, as there is ELN and OtherGroup in the contrasting chunk. Finally, we combine the sign of the weights (positive or negative) with the magnitude to give us the final weights of -2 (FARC), 1 (ELN) and 1 (OtherGroup)

contrasts(TerroristData$Group) <- cbind(c(-2, 1, 1), c(0, -1, 1))
# Alternatively, helmert contrasts can be used: contrasts(TerroristData$Group) <- contr.helmert(3)
# We can use these contrasts to run the ANCOVA
TerroristModel <- aov(BogotaProvince ~ OtherProvince + Group, data = TerroristData)
Anova(TerroristModel, type = "III")
# Anova Table (Type III tests)

# Response: BogotaProvince
# Sum Sq Df F value    Pr(>F)    
# (Intercept)   76.069  1 25.0205 3.342e-05 ***
#  OtherProvince 15.076  1  4.9587   0.03483 *  
#   Group         25.185  2  4.1419   0.02745 *  
#   Residuals     79.047 26                      
---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# In the above output, we see that the covariate OtherProvince significantly predicts the dependent variable.  # (BogotaProvince). Therefore, attacks in BogotaProvince is influenced attacks in OtherProvince, Additionally, when the effect of OtherProvince is removed, the effect of group type is significant (0.02745) 
  
# Line 56 documents the group means. It is not possible to interpret these means, as they have not been adjusted for the effects of the covariate. The original results fail to inform us about the group mean differences in the significant ANCOVA for the variable "Group". To obtain the adjusted means, we need the effect() function in the effects package. This produces a summary table of means for a specified effect in a model created by lm() or aov(), but is adjusted for other variables in the model (these are called marginal means). 
  
rstatix::emmeans_test(TerroristData, BogotaProvince ~ OtherProvince, 
                      p.adjust.method = "bonferroni", 
                      detailed = TRUE)


# Planned Contrasts -------------------------------------------------------

summary.lm(TerroristModel)

# Call:
# v(formula = BogotaProvince ~ OtherProvince + Group, data = TerroristData)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2622 -0.7899 -0.3230  0.8811  4.5699 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       1.7892     0.8671   2.063   0.0492 *
#   OtherProvince     0.4160     0.1868   2.227   0.0348 *
#   GroupELN          1.7857     0.8494   2.102   0.0454 *
#   GroupOtherGroup   2.2249     0.8028   2.771   0.0102 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.744 on 26 degrees of freedom
# Multiple R-squared:  0.2876,	Adjusted R-squared:  0.2055 
# F-statistic:   3.5 on 3 and 26 DF,  p-value: 0.02954

# Here, the first dummyvariable, called GroupELN, compares GroupELN with the average of the adjusted means for GroupOtherGroup and GroupFARC. As it is significant, then, it means that GroupELN is significantly differenr than the combined means of the groups in the Group variable in terms of terrorist attacks in Bogota Province
# The second dummy variable, called "GroupOtherGroup", compares GroupELN and GroupOtherGroup. As it is significant, then it means that GroupOtherGroup did produce signficantly more terrorist attacks in Bogota Province than GroupELN.
# The estimate for OtherProvince covariate, which is 0.4160, which is positive, indicates that as a terrorist attack count increases by one, then terrorist attack in BogotaProvince increases by half a unit.

# Testing for Homogeneity of regression slopes -------------------------------------------------

hoRS <- aov(BogotaProvince ~ OtherProvince + Group + Group:OtherProvince, TerroristData)
hoRS
hoRS <- aov(BogotaProvince ~ OtherProvince*Group, TerroristData)
hoRS
# Both the above formulas create the same output
Anova(hoRS, type = "III")

# Anova Table (Type III tests)
# 
# Response: BogotaProvince
# Sum Sq Df F value   Pr(>F)   
# (Intercept)          0.771  1  0.3157 0.579405   
# OtherProvince       19.922  1  8.1565 0.008715 **
#   Group               36.558  2  7.4836 0.002980 **
#   OtherProvince:Group 20.427  2  4.1815 0.027667 * 
#   Residuals           58.621 24                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# In the above output, the homegeneity of regression slopes assumption has been broken as the interaction between OtherProvince:Group is significant. Therefore, the analysis is called into question. This assumption checks that there is no significant interaction between the covariate and the grouping variable
# It can also be tested by using anova_test() function
TerroristData %>% anova_test(BogotaProvince ~ OtherProvince*Group)

# Robust ANCOVA -----------------------------------------------------------

# We need to split the Group variable. We want to create two sub variables, namely FARC and ELN, as robust ANCOVA implemented for 2 groups only
# ANCOVA takes the following form:

# ancova(covGrp1, dvGrp1, covGrp2, dvGrp2, tr. = 2)
# the cov refers to covariates and dv refers to dependent variables

setwd("C:/R Portfolio")
terrorism_data <- read.csv("Terrorism_data.csv")

FARC <- subset(terrorism_data, Group == 1)
ELN <- subset(terrorism_data, Group == 2)
covGrp1 <- FARC$BogotaProvince
dvGrp1 <- FARC$OtherProvince
covGrp2 <- ELN$BogotaProvince
dvGrp2 <- ELN$OtherProvince
ancova(covGrp1, dvGrp1, covGrp2, dvGrp2, tr. = 2)

# [1] "NOTE: Confidence intervals are adjusted to control the probability"
# [1] "of at least one Type I error."
# [1] "But p-values are not"
# $output
# X n1 n2       DIF     TEST        se    ci.low       ci.hi     p.value crit.val
# [1,] 2 17 21 -1.405594 1.888193 0.7444124 -3.484572  0.67338339 0.072261241 2.792777
# [2,] 4 26 31 -1.733553 3.130180 0.5538189 -3.240109 -0.22699584 0.003719873 2.720306
# [3,] 5 26 32 -1.012500 1.676646 0.6038842 -2.664430  0.63942982 0.104360221 2.735508
# [4,] 5 26 32 -1.012500 1.676646 0.6038842 -2.664430  0.63942982 0.104360221 2.735508
# [5,] 7 17 24 -1.375000 2.614530 0.5259071 -2.829079  0.07907858 0.015021056 2.764896

ancboot(covGrp1, dvGrp1, covGrp2, dvGrp2, nboot = 2000)

# [1] "Note: confidence intervals are adjusted to control FWE"
# [1] "But p-values are not adjusted to control FWE"
# [1] "Taking bootstrap samples. Please wait."
# $output
# X n1 n2       DIF      TEST    ci.low      ci.hi p.value
# [1,] 2 17 21 -1.405594 -1.888193 -3.558132  0.7469435  0.0835
# [2,] 4 26 31 -1.733553 -3.130180 -3.334972 -0.1321336  0.0060
# [3,] 5 26 32 -1.012500 -1.676646 -2.758688  0.7336876  0.1055
# [4,] 5 26 32 -1.012500 -1.676646 -2.758688  0.7336876  0.1020
# [5,] 7 17 24 -1.375000 -2.614530 -2.895710  0.1457095  0.0210
# 
# $crit
# [1] 2.891593

# Both the ancova and ancboot outputs can be interpreted in the same way. The x column indicates the five values for the OtherProvince covariate for which the relationship between baseline BogotaProvince and OtherProvince are comparable in the two covgrps. At these five points, we are informed as to the number of cases in the data for the two groups (n1 and n2) that have a covariate value close to x. Based on these two samples, trimmed means (20 by default), are computed and the difference between them is tested. The difference is stored in the DIF column and its estimate standard error in the se column. The test statistic comparing the difference is in the TEST column (is the difference divided by the standard error). The confidence interval of the difference between the trimmed means is included (these are corrected to control for the fact that we have conducted five tests). Lastly, we are informed of the p-values for the tests of the difference between the trimmed means. Where they are significant, it means that there is a significant difference between the trimmed means when adjusting for the covariate. 
# These two outputs indicate significant differences between trimmed means for two of the five design points. In other words, in least cases, the groups differ significantly in their mean level of terrorist attack across the two terrorist groups. In the ancova output, we didn't get significant results for the first, third and fourth design point. This therefore suggests that ELN attacks increased in provinces with low attack counts or where attack counts were already high, but not for average attack rates.