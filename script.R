
#packages
library(lavaan)
library(tidyverse)
library(psych)
library(dplyr)
library(here)
library(semTools)
library(semPlot)
library(GPArotation)

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
                         W1_CRT1, W1_CRT2, W1_CRT3, W1_CRT4, W1_CRT5, W1_Gender, W1_Age_year, W1_Age_categories)

#removing NA's
is.na.data.frame(studydf)
studydf <- na.omit(studydf)

#testing structure of data
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
#Normality testing
testdata <- studydf[,2:50]
cor(testdata)

qqnorm(studydf$W1_Paranoia1-studydf$W1_Paranoia5)
qqnorm(studydf$W1_Conspiracy_1-studydf$W1_Conspiracy_5)
qqnorm(studydf$W1_ReligiousBelief1-studydf$W1_ReligiousBelief8)
qqnorm(studydf$W1_Nationalism1 & studydf$W1_Nationalism2)
qqnorm(studydf$W1_DAI1-studydf$W1_DAI17)
qqnorm(studydf$W1_CRT1-studydf$W1_CRT5)
qqnorm(studydf$W1_Death_Anxiety_Total)
qqnorm(studydf$W1_TotalCRT)
#take religious belief out 2,4,6,8
#CFA

path1 <- '
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1_R +  W1_ReligiousBelief3_R +  W1_ReligiousBelief5_R +
             W1_ReligiousBelief7_R 
Nationalism =~ W1_Nationalism1 + W1_Nationalism2

Conspiracy ~~ 0* Paranoia + 0* Religion + 0* Nationalism
Religion ~~ 0* Paranoia + 0* Nationalism
Nationalism ~~ 1* Nationalism + 0 * Paranoia

'
cfamodel <- cfa(path1, data = studydf)
summary(cfamodel, standardized = TRUE, fit.measures = TRUE, rsquare = T)
#path
semPaths(cfamodel, "std", layout = 'tree3', bifactor = "General", edge.color = "black", nCharNodes = 0)
#std Factor Loadings
inspect(cfamodel, what = "std", list.by.group = T)$lambda

#the first bifactor model
path0 <-' 
General =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5 + W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 + 
           W1_ReligiousBelief1_R + W1_ReligiousBelief3_R +  W1_ReligiousBelief5_R + 
           W1_ReligiousBelief7_R 
Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1_R +  W1_ReligiousBelief3_R +  W1_ReligiousBelief5_R + W1_ReligiousBelief7_R 


General ~~ 0*Paranoia + 0*Conspiracy + 0*Religion 
Paranoia ~~ 0*Conspiracy + 0*Religion 
Conspiracy ~~ 0*Religion 


'
bifactormodelbase <- cfa(path0, data = studydf)
summary(bifactormodelbase, standardized = TRUE, fit.measures = TRUE, rsquare = T)
fitmeasures(bifactormodelbase, c('cfi', 'tli','rmsea', 'rmsea.ci.upper', 'bic'))
Bifactordia<- semPaths(bifactormodelbase, "std", layout = 'tree3', bifactor = "General", edge.color = "black", nCharNodes = 0)
savePlot(Bifactordia, )
#std factor loadings
bifactormatrix<-inspect(bifactormodelbase, what = "std", list.by.group = T)$lambda
bifactormatrix<
#reliability
datafactorbase <- predict(bifactormodelbase)
reliability(bifactormodelbase)
compRelSEM(bifactormodelbase, tau.eq = F, ord.scale = T, return.total = T)





# SEM 
model_SEM <- '
General =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5 + W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 + W1_ReligiousBelief1_R + 
           W1_ReligiousBelief3_R +  W1_ReligiousBelief5_R  +
           W1_ReligiousBelief7_R 

Paranoia =~ W1_Paranoia1 + W1_Paranoia2 + W1_Paranoia3 + W1_Paranoia4 + W1_Paranoia5
Conspiracy =~ W1_Conspiracy_1 + W1_Conspiracy_2 + W1_Conspiracy_3 + W1_Conspiracy_4 + W1_Conspiracy_5 
Religion =~  W1_ReligiousBelief1_R + W1_ReligiousBelief3_R + W1_ReligiousBelief5_R + W1_ReligiousBelief7_R 


General ~~ 0*Paranoia + 0*Conspiracy + 0*Religion 
Paranoia ~~ 0*Conspiracy + 0*Religion
Conspiracy ~~ 0*Religion 
W1_Death_Anxiety_Total ~~ 0*W1_TotalCRT

General ~ W1_Death_Anxiety_Total

General ~ W1_TotalCRT


'

Fit_SEM <- sem(model_SEM, data=studydf, estimator='WLSMV', std.lv=TRUE)
model_SEM_fit <- summary(Fit_SEM, fit.measures=TRUE, standardized= TRUE, rsquare = T)
fitmeasures(Fit_SEM, c('cfi', 'tli','rmsea', 'rmsea.ci.upper', 'bic'))
model_SEM_fit
#path diagram
SEMDia <- semPaths(Fit_SEM, "std", layout = 'tree3', bifactor = "General", edge.color = "black", nCharNodes = 1) 

#loading tables
semmatrix<- inspect(Fit_SEM, what = "est", list.by.group = T)$lambda
#Reliability
#omega
reliabilityL2(Fit_SEM, "General" )
reliabilityL2(Fit_SEM, "Paranoia" )
reliabilityL2(Fit_SEM, "Conspiracy" )
reliabilityL2(Fit_SEM, "Religion" )
omegaFromSem(Fit_SEM)
#alpha
compRelSEM(Fit_SEM, obs.var = T, tau.eq = T)
#Add into the SEM  to check the relationships between the variables
#Regressions to run 
#General ~ W1_Death_Anxiety_Total  + W1_TotalCRT + W1_Age_year + W1_Gender

#General ~ Paranoia + Conspiracy + Religion

#W1_Death_Anxiety_Total ~ Paranoia + Conspiracy + Religion 

#W1_TotalCRT ~ Paranoia + Conspiracy + Religion







