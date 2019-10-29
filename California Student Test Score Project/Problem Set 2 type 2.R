library(gcookbook)
library(psych)
library(readr)
library(readxl)
library(modelr)
library(ggplot2)
library(MASS)
library(tseries)
library(grDevices)
library(reshape2)
library(stats)
library(ggforce)
library(dplyr)
library(modeest)
library(leaps)
library(Ecdat)
library(car)
library(lmtest)
library(car)
library(lmtest)
library(sandwhich)
library(lmSubsets)
library(leaps)
library(HH)

setwd("~/R/Econometrics")
getwd()

#Delete all variables in the memory
rm(list = ls())

TestData<-read.csv("CATestScoreData.csv",header=TRUE)

no_math_data <-TestData
no_math_data$math_scr <- NULL
no_math_data$testscr <- NULL
no_math_data$X <- NULL
no_math_data$X.1 <- NULL
no_math_data$read_scr <- NULL
size = ncol(no_math_data)

for (x in 1:size) {
  y = x
  while (y <= size) {
    new_name <- paste(colnames(no_math_data)[x], "*", colnames(no_math_data)[y])
    no_math_data$bullshit = no_math_data[,x] * no_math_data[,y]
    names(no_math_data)[names(no_math_data) == "bullshit"] <- new_name
    y = y + 1
    #print(head(no_math_data, n=1L))
  }
}
head(no_math_data)
no_math_data$read_scr <- TestData$read_scr


#Regression of Student Teacher Ratio on Reading Score 
LM_Reading_STR <- lm(read_scr ~ str, data=no_math_data)


#NVMAX = 16 
LinReg_all_16 <- regsubsets(
  read_scr ~ ., data = no_math_data, 
  nvmax = 16, really.big= TRUE ,
  method=c("exhaustive")) #We should try the NBEST =1 

#NVMAX = 128 
LinReg_all_128 <- regsubsets(
  read_scr ~ ., data = no_math_data, 
  nvmax = 128, really.big= TRUE ,
  method=c("exhaustive")) #We should try the NBEST =1 


#Best with only squared terms
LinReg_Best_Squared <- lm(read_scr ~ meal_pct + `expn_stu * expn_stu` + avginc + el_pct , data=no_math_data) #### I think this is wrong

#Best Model (I think)
lmNVMAX16 <- lm(read_scr~ `str * el_pct` + `expn_stu * avginc` + `meal_pct * str` + el_pct, data=no_math_data) #### I think this is better


#Best with all interactions, but nvmax = 128
LinReg_Best_128 <- lm(read_scr ~ `meal_pct * str` + `str * el_pct` + expn_stu + el_pct , data=no_math_data)


############################################ OUTPUTS #########################################################


#Get Best Models from NVMAX 16
data.frame(
  BIC = which.min(summary(LinReg_all_16)$bic),
  Adjusted_R2 = which.max(summary(LinReg_all_16)$adjr2),
  R2 = which.max(summary(LinReg_all_16)$rsq),
  RSS = which.min(summary(LinReg_all_16)$rss))

#Get Best Models from NVMAX 128
data.frame(
  BIC = which.min(summary(LinReg_all_128)$bic),
  Adjusted_R2 = which.max(summary(LinReg_all_128)$adjr2),
  R2 = which.max(summary(LinReg_all_128)$rsq),
  RSS = which.min(summary(LinReg_all_128)$rss))



        
###########################   Write Data to Excel   ########################### 

#No Math Data, Raw
write.csv(
  no_math_data,
  file = "no_math_data.csv")

#Header Output
write.csv(
  head(no_math_data),
  file = "No Math Headers.csv") 




#Simple Linear Regression Summary
sink("Summary of Reading Score v. STR Model.txt")
print(summary(LM_Reading_STR))
print("BIC: ")
print(BIC(LM_Reading_STR))
sink()

#Best Model (I think)
sink("Best Model Summary.txt")
print(summary(lmNVMAX16))
print("BIC: ")
print(BIC(lmNVMAX16))
sink()




#Best 2nd Degree Model Summary
sink("Best 2nd Degree Model Summary.txt")
print(summary(LinReg_Best_Squared))
print("BIC: ")
print(BIC(LinReg_Best_Squared))
sink()

#NVMAX 128 Best Model Summary
sink("NVMAX 128 Best Model Summary.txt")
print(summary(LinReg_Best_128))
print(summary(LinReg_Best_128))
print("BIC: ")
print(BIC(LinReg_Best_128))
sink()



#Best Predicted Model from NVMAX 16
write.csv(rbind(
  as.data.frame((summaryHH(LinReg_all_16))[4,])), #***Change [#,] to correct model prior to use***
  file = "Best NVMAX 16 Model.csv")


#Best Predicted Model from NVMAX 128
write.csv(rbind(
  as.data.frame((summaryHH(LinReg_all_128))[4,])),#***Change [#,] to correct model prior to use***
  file = "Best NVMAX 128 Model.csv")




#Excel Outputs From All Generated Models
#All NVMAX 16 Models
write.csv(rbind(
  as.data.frame(summaryHH(LinReg_all_16)[1:16,])), 
  file = "Model Summaries NVMAX 16.csv")

#All NVMAX 128 Models
write.csv(rbind(
  as.data.frame(summaryHH(lm_all_128)[1:128,])), 
  file = "Model Summaries NVMAX 128.csv")




#NVMAX 16 In-or-Out Table
write.csv(as.data.frame(
  summary(LinReg_all_16)$outmat),
  file = "Variables In-or-Out Table NVMAX 16.csv")

#NVMAX 128 In-or-Out Table
write.csv(as.data.frame(
  summary(LinReg_all_128)$outmat),
  file = "Variables In-or-Out Table NVMAX 128.csv")
