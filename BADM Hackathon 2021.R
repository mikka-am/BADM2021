#clean environment
rm(list = ls())

setwd("~/SFU/BADM Hackathon 2021/2021 Competition")

install.packages("tidyverse")
install.packages("car")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("nnet")
install.packages("randomForest")
install.packages("effects")
install.packages("data.table")
install.packages("corrplot")

library("tidyverse")
library("car")
library("rpart")
library("rpart.plot")
library("nnet")
library("randomForest")
library("effects")
library("data.table")
library("corrplot") 

source('BCA_functions_source_file.R')

#add data
sur <- read_csv("SURVEY_DATASET.CSV")
fsa <- read_csv("FSA_DATASET.CSV")
use <- read_csv("USAGE_DATASET.CSV")

#remove bad identifier
sur$table_id <- NULL
fsa$table_id <- NULL
use$table_id <- NULL

merged.pts1 <- merge(sur, use, by="pt_id")

merged.pts1$fsa <- merged.pts1$clinic_fsa

merged.pts1$clinic_fsa <- NULL

merged <- merge(merged.pts1,fsa,by="fsa")

variable.summary(merged)

view(merged)

#Set variable "pt_id" as row names
rownames(merged) <- merged$pt_id

view(merged)

merged$pt_id <- NULL


#change all character variables into factor variables first
merged$perc_weight <- as.factor(merged$perc_weight)

factors <- c('fsa' ,'income','age','edu','perc_health','bmi_class',
             'repstrain','injstatus','physactivityindicator','perc_mentalHealth','perc_lifstress','perc_workstress','care_language','Sample')

merged[,factors] <- lapply(merged[,factors] , factor)

variable.summary(merged)

#reclassify NA bmi/weight stuff into Prefer not to say

merged$New.perc_weight <- fct_explicit_na(merged$perc_weight, # Factor of interest
                          na_level = "Prefer not to say") # replacement value

merged$New.bmi_class <- fct_explicit_na(merged$bmi_class, # Factor of interest
                                          na_level = "Prefer not to say") # replacement value

view(merged)

merged$perc_weight <- NULL
merged$bmi_class <- NULL

variable.summary(merged)

#remove FSA postcodes
merged$fsa <- NULL

variable.summary(merged)

summary(merged)

#convert appropriate factor variables to numeric
#***First, create new columns with numbers as categories***
merged$perc_health.Num <- fct_collapse(merged$perc_health,
                                        "5" = c("Excellet"),
                                        "4" = c("Very Good"),
                                        "3" = c("Good"),
                                        "2" = c("Fair"),
                                        "1" = c("Poor"))

merged$physactivityindicator.Num <- fct_collapse(merged$physactivityindicator,
                                                  "4" = c("Active"),
                                                  "3" = c("Moderate"),
                                                  "2" = c("Somewhat"),
                                                  "1" = c("None"))

merged$perc_mentalHealth.Num <- fct_collapse(merged$perc_mentalHealth,
                                              "5" = c("Excellet"),
                                              "4" = c("Very Good"),
                                              "3" = c("Good"),
                                              "2" = c("Fair"),
                                              "1" = c("Poor"))

merged$perc_lifstress.Num <- fct_collapse(merged$perc_lifstress,
                                                "5" = c("Extreme"),
                                                "4" = c("Quite"),
                                                "3" = c("A bit"),
                                                "2" = c("Not Very"),
                                                "1" = c("None"))

merged$perc_workstress.Num <- fct_collapse(merged$perc_workstress,
                                               "5" = c("Extreme"),
                                               "4" = c("Quite"),
                                               "3" = c("A bit"),
                                               "2" = c("Not Very"),
                                               "1" = c("None"))

merged$prefer_english <- fct_collapse(merged$care_language,
                                           "1" = c("English"),
                                           "0" = c("Other"))

#**CONVERT NEW CATEGORIES INTO NUMERIC**
merged$perc_health.Num <- as.numeric(merged$perc_health.Num)
merged$physactivityindicator.Num <- as.numeric(merged$physactivityindicator.Num)
merged$perc_mentalHealth.Num <- as.numeric(merged$perc_mentalHealth.Num)
merged$perc_lifstress.Num <- as.numeric(merged$perc_lifstress.Num)
merged$perc_workstress.Num <- as.numeric(merged$perc_workstress.Num)
merged$prefer_english <- as.numeric(merged$prefer_english)

#REMOVE OLD FACTOR VARIABLES
merged$perc_health <- NULL
merged$physactivityindicator <- NULL
merged$perc_mentalHealth <- NULL
merged$perc_lifstress <- NULL
merged$perc_workstress <- NULL
merged$care_language <- NULL

#Check for correlations
corrMatrix <- cor(select_if(merged, is.numeric))

view(corrMatrix)

corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

#***OBSERVED CORRELATIONS:
#*pop_fsa and hhold_fsa
#*pop_fsa and hhold_work_health
#*hhold_fsa and hhold_work_health
#*Need to convert total amounts into averages:
  #*pop_fsa
  #*hhold_work_health
  #*tot_spend_toba_alco
  #*Divide all of the three above variables by hhold_fsa

merged$avg_pop_fsa <- merged$pop_fsa/merged$hhold_fsa
merged$avg_hhold_work_health <- merged$hhold_work_health/merged$hhold_fsa
merged$avg_spend_toba_alco <- merged$tot_spend_toba_alco/merged$hhold_fsa

#REMOVE OLD TOTAL VARIABLES
merged$pop_fsa <- NULL
merged$hhold_work_health <- NULL
merged$tot_spend_toba_alco <- NULL

#***RECHECK CORRELATIONS
corrMatrix2 <- cor(select_if(merged, is.numeric))

view(corrMatrix2)

corrplot(corrMatrix2,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

#**OTHER CORRELATIONS FOUND:
  #*Avg_spend_health & avg_dcost
    #*Removing avg_dcost because it's included in avg_spend_health (i.e., avg_spend_health is always greater)
  #*avg_spend_health & avg_insur_prem
    #*Removing insurance premium because you can infer their insurance premiums if they spend a lot on healthcare

merged$avg_dcost <- NULL
merged$avg_insur_prem <- NULL


#NEED TO WIPE IDENTIFIERS LIKE clinic_id FROM DATA
merged$clinic_id <- NULL

#Correlation recheck THE TRILOGY
corrMatrix3 <- cor(select_if(merged, is.numeric))

view(corrMatrix3)

corrplot(corrMatrix3,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

#***REMOVE TRIVIALLY RELATED VARIABLES***
#Remove freq because trivially related to power_us (target variable)
#Remove median_income_fsa because income already exists as a factor variable (and income is directly related to patient)
#Remove median_age_fsa beacuse age already exists as a factor variable directly to patients
#remove avg_spend_health because spending already exists
  #note that spend_health in data dictionary is supposed to be spending
merged$freq <- NULL
merged$median_income_fsa <- NULL
merged$median_age_fsa <- NULL
merged$avg_spend_health <- NULL

variable.summary(merged)

#NOTE: Not combining factor variables in edu unless it poses a significance problem

#Convert power_us to factor
merged$power_us <- as.factor(merged$power_us)

#FINAL STEP OF CLEANING IS TO SEPARATE INTO SAMPLE AND VALIDATION

merged.train   <- filter(merged,Sample == "Estimation")
merged.test    <- filter(merged,Sample == "Validation")
merged.holdout <- filter(merged,Sample =="Holdout" )

summary(merged)

#now we can start models!!!!!! models to use:
  #*Logistic
  #*Classification tree
  #*Stepwise
  #*Random forest
  #*Neural net
  #*Then model assessment with liftcharts

#get all variable names
paste(names(merged), collapse = " + ")

#Logistic regression
merged.logreg1 <- glm(formula = power_us ~  income + age + edu + 
                        arthritis + highBP + diabetes + stroke + 
                        repstrain + injstatus + gave_birth_last5 + 
                        othercare + spending + hhold_fsa + New.perc_weight + 
                        New.bmi_class + 
                        perc_health.Num + physactivityindicator.Num + 
                        perc_mentalHealth.Num + perc_lifstress.Num + 
                        perc_workstress.Num + prefer_english + avg_pop_fsa + 
                        avg_hhold_work_health + avg_spend_toba_alco, data = merged.train,
                      family = binomial(logit))

summary(merged.logreg1)

#Stepwise
merged.step <- step(merged.logreg1,direction="both")

summary(merged.step)



#Classification tree
merged.classtree <- rpart(formula = power_us ~  income + age + edu + 
                            arthritis + highBP + diabetes + stroke + 
                            repstrain + injstatus + gave_birth_last5 + 
                            othercare + spending + hhold_fsa + New.perc_weight + 
                            New.bmi_class + 
                            perc_health.Num + physactivityindicator.Num + 
                            perc_mentalHealth.Num + perc_lifstress.Num + 
                            perc_workstress.Num + prefer_english + avg_pop_fsa + 
                            avg_hhold_work_health + avg_spend_toba_alco,
                        data = merged.train,
                        cp = 0.0001, #set to 0.0001 to check 
                        model = TRUE)

plotcp(merged.classtree)
printcp(merged.classtree)

rpart.plot(merged.classtree,type=0,fallen.leaves = FALSE,
           uniform=TRUE, yes.text="true",no.text="false",cex=0.6,digits=2)

#Tree version 2
merged.classtree2 <- rpart(formula = power_us ~  income + age + edu + 
                            arthritis + highBP + diabetes + stroke + 
                            repstrain + injstatus + gave_birth_last5 + 
                            othercare + spending + hhold_fsa + New.perc_weight + 
                            New.bmi_class + 
                            perc_health.Num + physactivityindicator.Num + 
                            perc_mentalHealth.Num + perc_lifstress.Num + 
                            perc_workstress.Num + prefer_english + avg_pop_fsa + 
                            avg_hhold_work_health + avg_spend_toba_alco,
                          data = merged.train,
                          cp = 0.003, 
                          model = TRUE)

rpart.plot(merged.classtree2,type=0,fallen.leaves = FALSE,
           uniform=TRUE, yes.text="true",no.text="false",cex=0.6,digits=2)

#Random forest

merged.RF <- randomForest(formula = power_us ~  income + age + edu + arthritis + highBP + diabetes + stroke + 
                            repstrain + injstatus + gave_birth_last5 + 
                            othercare + spending + hhold_fsa + New.perc_weight + 
                            New.bmi_class + 
                            perc_health.Num + physactivityindicator.Num + 
                            perc_mentalHealth.Num + perc_lifstress.Num + 
                            perc_workstress.Num + prefer_english + avg_pop_fsa + 
                            avg_hhold_work_health + avg_spend_toba_alco,
                          data = merged.train,
                              importance = TRUE,
                              ntree = 400, mtry = 2)

varImpPlot(merged.RF,type = 2,
           main="merged.RF", # title
           cex =0.7) # font size

#Neural Network
merged.Nnet <- Nnet(formula = power_us ~  income + age + edu + 
                      arthritis + highBP + diabetes + stroke + 
                      repstrain + injstatus + gave_birth_last5 + 
                      othercare + spending + hhold_fsa + New.perc_weight + 
                      New.bmi_class + 
                      perc_health.Num + physactivityindicator.Num + 
                      perc_mentalHealth.Num + perc_lifstress.Num + 
                      perc_workstress.Num + prefer_english + avg_pop_fsa + 
                      avg_hhold_work_health + avg_spend_toba_alco,
                    data = merged.train,
                      decay = 0.10, # decay parameter
                      size = 4)

merged.Nnet2 <- Nnet(formula = power_us ~  income + age + edu + 
                      arthritis + highBP + diabetes + stroke + 
                      repstrain + injstatus + gave_birth_last5 + 
                      othercare + spending + hhold_fsa + New.perc_weight + 
                      New.bmi_class + 
                      perc_health.Num + physactivityindicator.Num + 
                      perc_mentalHealth.Num + perc_lifstress.Num + 
                      perc_workstress.Num + prefer_english + avg_pop_fsa + 
                      avg_hhold_work_health + avg_spend_toba_alco,
                    data = merged.train,
                    decay = 0.15, # decay parameter
                    size = 4)

summary(merged.Nnet)

#model assessment

#Estimation - Cumulative
lift.chart(modelList = c("merged.logreg1","merged.step","merged.classtree2","merged.RF","merged.Nnet","merged.Nnet2"),
           data = merged.train,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Estimation")

#Validation - Cumulative
lift.chart(modelList = c("merged.logreg1","merged.step","merged.classtree2","merged.RF","merged.Nnet"),
           data = merged.test,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Validation")

#Validation - Cumulative (Just Step)
lift.chart(modelList = c("merged.step"),
           data = merged.test,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Validation")

#Incremental 
lift.chart(modelList = c("merged.step"),
           data = merged.test,
           targLevel = "1",
           trueResp = 0.22,
           type = "incremental", # "incremental" instead of "cumulative" 
           sub = "Validation")

#SCORING
merged$score <- rawProbScore(model = "merged.step",
                                               data = merged,
                                               targLevel = "1")

#BRING BACK PT_ID
merged$pt_id <- row.names(merged)

view(merged)

Submission.step <- merged[merged$Sample == "Holdout",c("pt_id","score")]

write.csv(Submission.step,"Team425 (OLD).csv")

summary(merged.step)

summary(merged)

#Time to try adjusting the neural net!
merged.Nnet4 <- Nnet(formula = power_us ~  income + age + edu + 
                       arthritis + highBP + diabetes + stroke + 
                       repstrain + injstatus + gave_birth_last5 + 
                       othercare + spending + hhold_fsa + New.perc_weight + 
                       New.bmi_class + 
                       perc_health.Num + physactivityindicator.Num + 
                       perc_mentalHealth.Num + perc_lifstress.Num + 
                       perc_workstress.Num + prefer_english + avg_pop_fsa + 
                       avg_hhold_work_health + avg_spend_toba_alco,
                     data = merged.train,
                     decay = 0.2, # decay parameter
                     size = 4)

lift.chart(modelList = c("merged.step","merged.Nnet","merged.Nnet4"),
           data = merged.test,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Validation2")

#  Crosstabs
#
#  Observed counts showing the relation between the variables
Tbl.merge <- xtabs(~merged$income + merged$age, data = merged)
Tbl.merge

Test.merge <-chisq.test(Tbl.merge) #generate the test and the expected counts

## Expected counts if no relation between Spending and Children
Test.merge$expected

prop.table(Tbl.merge, margin = 2)

Test.merge

#Neural net sequel
merged.Nnet5 <- Nnet(formula = power_us ~  income + age + edu + 
                       arthritis + highBP + diabetes + stroke + 
                       repstrain + injstatus + gave_birth_last5 + 
                       othercare + spending + hhold_fsa + New.perc_weight + 
                       New.bmi_class + 
                       perc_health.Num + physactivityindicator.Num + 
                       perc_mentalHealth.Num + perc_lifstress.Num + 
                       perc_workstress.Num + prefer_english + avg_pop_fsa + 
                       avg_hhold_work_health + avg_spend_toba_alco,
                     data = merged.train,
                     decay = 0.2, # decay parameter
                     size = 3)

lift.chart(modelList = c("merged.step","merged.Nnet4","merged.Nnet5"),
           data = merged.test,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Validation3")

#Comparing age and perceived health
merged$perc_health.cat <- as.factor(merged$perc_health.Num)

Tbl.agehealth <- xtabs(~merged$perc_health.cat + merged$age, data = merged)
Tbl.agehealth

Test.agehealth <-chisq.test(Tbl.agehealth) #generate the test and the expected counts

## Expected counts if no relation between Spending and Children
Test.agehealth$expected

prop.table(Tbl.agehealth, margin = 2)

Test.agehealth

#Correlation again
corrMatrix2 <- cor(select_if(merged, is.numeric))

view(corrMatrix2)

corrplot(corrMatrix2,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)


#SCORING ATTEMPT 2
merged$score2 <- rawProbScore(model = "merged.Nnet4",
                             data = merged,
                             targLevel = "1")

Submission.Nnet4 <- merged[merged$Sample == "Holdout",c("pt_id","score2")]

write.csv(Submission.Nnet4,"Team425.csv")


summary(merged)

#****DO NOT USE THE SECOND ATTEMPT!!! IT'S WORSE LOL***
#*
#*#Generate effect plots

plot(allEffects(merged.step),type = "response")

#Partial Dependency Plot for Random Forest

partialPlot(model4.RF,
            pred.data = r.test,
            x.var = TDS,
            sub = "Validation Set", 
            which.class = "1")
partialPlot(model4.RF,
            pred.data = r.test,
            x.var = amort_left_days,
            sub = "Validation Set", 
            which.class = "1")
partialPlot(model4.RF,
            pred.data = r.test,
            x.var = incomexsize,
            sub = "Validation Set", 
            which.class = "1")
