# PSY6009_210155431_Bentall
### Codebook
**df** = Base dataset

**studydf** = Final data frame used which consists of the data sets measures of paranoia, conspiracy belief, nationalism, death anxiety, analyitical ability, gender and age.

**testdata** = Converting the dataframe into a matrix so the cor function will work. 

**cfamodel** = This is the cfamodel fitted to the path

**cfaDia** = Path diagram for the CFA

**inspect()$lambda** = prints out the standardized factor loadings into one easy to read table. 

**bifactormodelbase** = the model for the first bifactor model (model 2). 

**bifactorDia** = This is the path diagram for one of the bifactor models (model 2).

**model_SEM** = Path for the second bifactor model (model 3)

**Fit_SEM** = Variable which represents the fitted model 3. 

**model_SEM_fit** = This is the summary of model 3. 

**fitmeasures** = Gives only the CFI, TLI, RMSEA, and  RMSEA upper(for comparing fits quickly).

**semDia** = Path diagram for the final bifactor model (model 3). 

### Information about running regressions
**Regressions were run after the intial model was ran, therefore the code for the analysis needs to be copy pasted into the lavaan path for model 3.**
**The code can be found under the model 3 reliablity tests titled "Reggressions to run". They can be added in or subtracted from depending on the research question.**

### Project Details
**Coded in R version 4.2.1**

**Packages Used**:
* lavaan 0.6-12
* tidyverse 1.3.2
* psych 2.2.5
* dplyr 1.0.9
* here 1.0.1
* semTools .05-6
* semPlot1 1.1.6
* GPArotation 2022.4-1

### Folder Structure
*cleaneddata = inlcudes the data used for final analysis
* rawdata = 1 raw data set of the C19PRC dataset
* 1 script called script
* 1 R project file
