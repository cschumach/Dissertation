#packages
library(lavaan)
library(tidyverse)
library(psych)
library(dplyr)
library(here)
library(semTools)
library(semPlot)

#Loading the main file as a Data Frame

df <- read.csv(here('rawdata', 'wave1COVIDdata.csv'))

#intial data clearing
summary(df)
is.na(df)
#moving variables of intrest into a new DF
studydf <- df %>% select(pid, W1_Paranoia1, W1_Paranoia2 , W1_Paranoia3, W1_Paranoia4, W1_Paranoia5, 
                         W1_Paranoia_Total, W1_Conspiracy_1,W1_Conspiracy_2,W1_Conspiracy_3, W1_Conspiracy_4, 
                         W1_Conspiracy_5, W1_Conspiracy_Total, W1_Religion_binary, W1_ReligiousBelief_Total,
                         W1_ReligiousBelief1,  W1_ReligiousBelief2,  W1_ReligiousBelief3,  W1_ReligiousBelief4,  W1_ReligiousBelief5,
                         W1_ReligiousBelief6, W1_ReligiousBelief7, W1_ReligiousBelief8, W1_ReligiousBelief2_R, W1_ReligiousBelief4_R, W1_ReligiousBelief6_R, W1_ReligiousBelief8_R,
                         W1_Political_Scale, W1_Nationalism1, W1_Nationalism2, W1_Nationalism_Total, W1_Death_Anxiety_Total,
                         W1_DAI1, W1_DAI2,  W1_DAI3, W1_DAI4, W1_DAI5, W1_DAI6,  
                         W1_DAI7,  W1_DAI8,  W1_DAI9,  W1_DAI10,  W1_DAI11,  W1_DAI12,  W1_DAI13, W1_DAI14,  W1_DAI15,  W1_DAI16,  W1_DAI17,
                         W1_CRT1, W1_CRT2, W1_CRT3, W1_CRT4, W1_CRT5)

#removing NA's
is.na.data.frame(studydf)
studydf <- na.omit(studydf)

#normallity testing
res <-resid(studydf)
MCLC <- studydf[c,('W1_Paranoia1, W1_Paranoia2 , W1_Paranoia3, W1_Paranoia4, W1_Paranoia5, 
                         W1_Paranoia_Total, W1_Conspiracy_1,W1_Conspiracy_2,W1_Conspiracy_3, W1_Conspiracy_4, 
                         W1_Conspiracy_5, W1_Conspiracy_Total, W1_Religion_binary, W1_ReligiousBelief_Total,
                         W1_ReligiousBelief1,  W1_ReligiousBelief2,  W1_ReligiousBelief3,  W1_ReligiousBelief4,  W1_ReligiousBelief5,
                         W1_ReligiousBelief6, W1_ReligiousBelief7, W1_ReligiousBelief8, W1_ReligiousBelief2_R, W1_ReligiousBelief4_R, W1_ReligiousBelief6_R, W1_ReligiousBelief8_R,
                         W1_Political_Scale, W1_Nationalism1, W1_Nationalism2,
                         W1_DAI1, W1_DAI2,  W1_DAI3, W1_DAI4, W1_DAI5, W1_DAI6,  
                         W1_DAI7,  W1_DAI8,  W1_DAI9,  W1_DAI10,  W1_DAI11,  W1_DAI12,  W1_DAI13, W1_DAI14,  W1_DAI15,  W1_DAI16,  W1_DAI17,
                         W1_CRT1, W1_CRT2, W1_CRT3, W1_CRT4, W1_CRT5')]
testdata <- studydf[,2:50]
cor(testdata)

qqnorm(studydf$W1_Paranoia1-studydf$W1_Paranoia5)
qqnorm(studydf$W1_Conspiracy_1-studydf$W1_Conspiracy_5)
qqnorm(studydf$W1_ReligiousBelief1-studydf$W1_ReligiousBelief8)
qqnorm(studydf$W1_Nationalism1 & studydf$W1_Nationalism2)
qqnorm(studydf$W1_DAI1-studydf$W1_DAI17)
qqnorm(studydf$W1_CRT1-studydf$W1_CRT5)


#CFA

path1 <- '
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1 +  W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
 W1_ReligiousBelief7 +  W1_ReligiousBelief8_R
Nationalism =~ W1_Nationalism1 + W1_Nationalism2

'
cfamodel <- cfa(path1, data = studydf)
summary(cfamodel, standardized = TRUE, fit.measures = TRUE)
#base bifactor model
path0 <-' 
General =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5 + W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 + W1_ReligiousBelief1 + 
  W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
  W1_ReligiousBelief7 +  W1_ReligiousBelief8_R +  W1_Nationalism1 + W1_Nationalism2 
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1 +  W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
  W1_ReligiousBelief7 +  W1_ReligiousBelief8_R
Nationalism =~ W1_Nationalism1 + W1_Nationalism2
General ~~ 1 * General
Paranoia ~~ 1* Paranoia + 0* General 
Conspiracy ~~ 1* Conspiracy + 0* General + 0 * Paranoia + 0* Religion + 0* Nationalism
Religion ~~ 1* Religion + 0* General 
Nationalism ~~ 1* Nationalism + 0* General
'
bifactormodelbase <- cfa(path0, data = studydf)
summary(bifactormodelbase, standardized = TRUE, fit.measures = TRUE)
semPaths(bifactormodelbase, "std","est", layout = 'tree3', bifactor = "General")

#bifactor model THE SUGGESTED MODEL

path2 <- '
General =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5 + W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 + W1_ReligiousBelief1 + 
W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
 W1_ReligiousBelief7 +  W1_ReligiousBelief8_R +  W1_Nationalism1 + W1_Nationalism2 +  W1_DAI1 + W1_DAI2 + W1_DAI3 + W1_DAI4 + W1_DAI5 + W1_DAI6 +  
  W1_DAI7 + W1_DAI8 + W1_DAI9 +  W1_DAI10 + W1_DAI11 +  W1_DAI12 +  W1_DAI13 + W1_DAI14 +  W1_DAI15 + W1_DAI16 +  W1_DAI17 +   W1_CRT1 +  W1_CRT2 + W1_CRT3 + W1_CRT4 + W1_CRT5
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1 +  W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
 W1_ReligiousBelief7 +  W1_ReligiousBelief8_R
Nationalism =~ W1_Nationalism1 + W1_Nationalism2
Death Anxiety =~  General + W1_DAI1 + W1_DAI2 + W1_DAI3 + W1_DAI4 + W1_DAI5 + W1_DAI6 +  
  W1_DAI7 + W1_DAI8 + W1_DAI9 +  W1_DAI10 + W1_DAI11 +  W1_DAI12 +  W1_DAI13 + W1_DAI14 +  W1_DAI15 + W1_DAI16 +  W1_DAI17
Analytical Ability =~ General+ W1_CRT1 +  W1_CRT2 + W1_CRT3 + W1_CRT4 + W1_CRT5
General ~~ 1* General
Paranoia ~~ 1* Paranoia + 0 * Conspiracy + 0* Religion
Conspiracy ~~ 1* Conspiracy + 0 * Nationalism
Religion ~~ 1* Religion + 0 * Nationalism 
Nationalism ~~ 1* Nationalism 
Death Anxiety ~~ 1*Death Anxiety + 0* Paranoia + 0* Conspiracy + 0*Religion + 0* Nationalism
Analytical Ability ~~ 1* Analytical Ability + 0* Paranoia + 0* Conspiracy + 0*Religion + 0* Nationalism

'

bifactormodel1 <- cfa(path2, data = studydf)
summary(bifactormodel1, standardized = TRUE, fit.measures = TRUE)
semPaths(bifactormodel1, "std", "est", layout = 'tree3', bifactor = "General")
modindices(bifactormodel1)

#ALL SECOND ORDER BIFACTORS!


#second-order bifactor test 2: includes the general factor being correlated to the constructs e.g. paranoia
path3 <- '
General =~Paranoia + Conspiracy + Religion + Political + Nationalism
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1 +  W1_ReligiousBelief2 +  W1_ReligiousBelief3 +  W1_ReligiousBelief4 +  W1_ReligiousBelief5 +  W1_ReligiousBelief6 +
 W1_ReligiousBelief7 +  W1_ReligiousBelief8
Political =~ W1_Political_Scale
Nationalism =~ W1_Nationalism1 + W1_Nationalism2


'
bifactormodeltest2 <- cfa(path3, data = studydf)
summary(bifactormodeltest2, standardized = TRUE, fit.measures = TRUE)
semPaths(bifactormodeltest2)

#bi-factor test with general loading on death anxiety and CRT measures specifically A BIT CURSED LMAO
path4 <- '

General =~Paranoia + Conspiracy + Religion + Political + Nationalism 
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1 +  W1_ReligiousBelief2_R +  W1_ReligiousBelief3 +  W1_ReligiousBelief4_R +  W1_ReligiousBelief5 +  W1_ReligiousBelief6_R +
 W1_ReligiousBelief7 +  W1_ReligiousBelief8_R
Nationalism =~ W1_Nationalism1 + W1_Nationalism2
Death Anxiety =~ General+  W1_DAI1 + W1_DAI2 + W1_DAI3 + W1_DAI4 + W1_DAI5 + W1_DAI6 +  
  W1_DAI7 + W1_DAI8 + W1_DAI9 +  W1_DAI10 + W1_DAI11 +  W1_DAI12 +  W1_DAI13 + W1_DAI14 +  W1_DAI15 + W1_DAI16 +  W1_DAI17
Analytical Ability =~ General + W1_CRT1 +  W1_CRT2 + W1_CRT3 + W1_CRT4 + W1_CRT5

'
bifactormodeltest3 <- sem(path4, data = studydf)
summary(bifactormodeltest3, standardized = TRUE, fit.measures = TRUE)

semPaths(bifactormodeltest3, "std", "est", layout = "tree2")
modindices(bifactormodeltest3)
#no path from 4 latent variables to general
