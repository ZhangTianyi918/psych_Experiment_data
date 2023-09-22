# Method Tutorial for Experiments
# Tianyi Zhang, 01/13/2023

# The dataset for this tutorial: 
# Dufner, M. (2021). Dataset for: Our Versus Their Narcissist: How People 
# View Narcissistic Persons From Their Ingroup and From a Competing Outgroup 
# [Data set]. PsychArchives. https://doi.org/10.23668/PSYCHARCHIVES.5219

# I added in "treatment" column manually for the convenience of this tutorial 
# 1 represents that the person is rating an in-group member (person form the same team)
# 0 represents that the person is rating an out-group member (person from a different team)

# =============================================================================================

# Install and load packages used for this tutorial 

install.packages("car")
install.packages("dplyr")
install.packages("psych")
install.packages("irr")
install.packages("ltm")
install.packages("sjPlot")
install.packages("cobalt")
install.packages("MatchIt")
install.packages("ggplot2")
library(MatchIt)
library(cobalt)
library(sjPlot)
library(irr)
library(car)
library(dplyr)
library(psych)
library(ltm)
library(ggplot2)
# =============================================================================================

# reading in data

group <- read.csv("group.csv")
# "group" file has the participants ratings for one another

individual <- read.csv("individual.csv")
# "individual" file has the narcissism rating of participants
# Half of it is self-report, half of it is reported by trained observers 

# Quick introduction of the experimental design: 

# This is an experimental testing if people perceive narcissism differently when the narcissistic person
# is a in-group member vs. an out-group member 
# Team membership is randomly assigned, 2 teams that are debating are of same gender
# to eliminate potential biases brought by gender differences 
# The two teams were asked to debate each other on controversial topics
# Likability of group one members were rated after first impression and round three of the debates
# In this sample data, we have 2 groups of 4 teams 

# =============================================================================================

# First thing to do is to check our data, since the behavior part was rated by observers
# I want to first test the inter rater consistency of the data to make sure they are consistent 

icc(individual[,27:30])
icc(individual[,31:34])
icc(individual[,35:38])
icc(individual[,39:42])


# The ICC needs to be at least .5 to be acceptable and .75 to be good
# and all the value we get from this is smaller than .5 
# Thus, in this tutorial I will not be using the observing data 
# In reality it is unlikely that the inter rater consistency is so awfully low, 
# if the observers are well trained and communicated well beforehand
# but always make sure to check the value before data-analysis 

# =============================================================================================

# Next, for the survey data, we want to test the cronbach alpha to ensure internal consistency
# There could be situations that some items are not well taken by the participants
# and we might want to eliminate some items in specific cases 

cronbach.alpha(individual[,9:17])
# For the narcissism admiration scale, we have a very good alpha (.837)

cronbach.alpha(individual[,18:26])
# For the narcissism rivalry scale, we have a very good alpha value too (.888)
# So the analysis forward is going to be based off self-report narcissism rating only 

# Aggregate the scale scores for grandiose narcissism 
individual$nar.mean <-apply(individual[,9:26],1,mean)

# Similarly, testing the likability scale's consistency 
cronbach.alpha(group[,6:7])
cronbach.alpha(group[,12:13])

# Aggregate the likability scales 
group$t1like <- apply(group[,6:7],1,mean)
group$t2like <- apply(group[,12:13],1,mean)


# Both values are good (.76 and .8), now we merge the two data sets by target id 
df = merge(individual, group, by.x = "id", by.y = "target_id")

# ============================================================================================
# Balance checking for control vs. treatment group
# In this case, since we randomized group membership
# We consider being in-group as the treatment group, and out-group as the control group
# Thus, the balance testing is to see if the in-group and out-group are balanced in terms
# of some demographic information, as well as our variables of interest 

# Create covariates subset
covs <- subset(df, select = c(nar.mean, gen, age))

# balance table 
bal.tab(covs, df$treatment)

# The "Diff.Un" column is the (standardized) difference in means between the two groups 
# prior to adjusting, closer to 0 means a better balance 
# As we can see we have 95 in-group members, and 105 out-group members 
# For gender, age, and narcissism, we have a decent balance 


# However, this might not happen in the real-life always
# If you have a significantly unbalanced data set, or it's hard to collect balanced data
# We can use the matchit package to 'trim off' part of the data for a better balance 
# Use G-Power to calculate what's the smallest sample size for your experiment design
# Then if you have a bigger dataset than that, you can use MatchIt to modify your data for a better
# balance 

# Checking out balance for the current dataset
m.out0 <- matchit(treatment ~ age + gen + nar.mean, # variables of interest
                  data = df, # dataset 
                method = NULL, # matching method, see ?matchit for more 
                distance ="glm" # distance measure to be used, here we have glm for 
                                # propensity score
                )
summary(m.out0)
# Similar to the balance table result 

# An example of matching dataset with the "nearest" method 
# m.out1 <- matchit(treatment ~ age + gen + nar.mean, data = df, 
#                  method = "nearest", distance ="glm")
# summary(m.out1)

# =============================================================================================

# With the data we have, we can try to test these hypotheses:
# H1: Narcissistic people are perceived less likable for both first impression (h2.1) and after 
# interactions (h2.2)

# H2: People perceive in-group members more likable than out group members for both first
# impression (h2.1) and after interactions (h2.2)

# H3: For first impression (h3.1), narcissistic people tend to be perceived less likable by in-group 
# members than out-group members; after interactions (h3.2), narcissistic people tend to be perceived 
# more likable by in-group members than out-group members 

# Null model write out 
h0.1 <- lm(t1like ~ 1, data = df)
h0.2 <- lm(t2like ~ 1, data = df)

# =============================================================================================

# Testing for hypothesis 1.1
h1.1 <- lm(t1like ~ nar.mean, data = df)
summary(h1.1)

# Narcissism does not predict the overall first-impression likability significantly
# There's only a insignificant negative relationship

# Testing for h1.2
h1.2 <- lm(t2like ~ nar.mean, data = df)
summary(h1.2)

# Narcissism does significantly influence overall likability after 3 rounds of interaction
# Overall, after 3 rounds of interaction, people tend to have a more positive review of 
# narcissistic individuals 

# visualize the negative relationship
plot(df$nar.mean, df$t2like)
abline(lm(t2like~nar.mean, data = df))

# =============================================================================================

# Testing for h2.1

h2.1 <- lm(t1like ~ nar.mean + treatment, data = df)
summary(h2.1)
# Group membership does not predict first-impression likability 

# Testing for h2.2
h2.2 <- lm(t2like ~ nar.mean + treatment, data = df)
summary(h2.2)

# Controlled for narcissism
# Group membership does not likability after interaction either 


# =============================================================================================
# Testing for h3.1
h3.1 <- lm(t1like ~ nar.mean * group.y, data = df)
summary(h3.1)

# Testing for h3.2
h3.2 <- lm(t2like ~ nar.mean * group.y, data = df)
summary(h3.2)


# The interplay of group membership and narcissism does not have a significant prediction of likability 
# which suggests that people tend to perceive narcissism the same regardless of group membership 

# =============================================================================================

# Summary: Overall, narcissism has positive relationship with likability after three rounds only. 
# Only Hypothesis 1 was partially supported. Group membership or the interaction of group membership
# and narcissism does not have influence over one's likability. 

# =============================================================================================

# Unit test 
# Coefficient summary for h3.2
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        1.7320     1.4852   1.166    0.245
# nar.mean           0.5564     0.4137   1.345    0.180
# group.y            0.4957     1.0564   0.469    0.639
# nar.mean:group.y  -0.1487     0.2930  -0.507    0.613

coef(h3.2)

# if these two aligns with each other more or less it's good 

