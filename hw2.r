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
library(car)
library(lmtest)
library(sandwhich)
library(lmSubsets)
library(leaps)
library(HH)
library(glmulti)
library(rJava)
library(L1pack)


setwd("~/R/Econometrics")
getwd()

#Delete all variables in the memory
rm(list = ls())

TestData<-read.csv("CATestScoreData.csv",header=TRUE)


#Simple Regression Model
Modlm <- lm(read_scr~str, data= no_math_data)
summary(Modlm)

#Simple Regression on only the Intercept
lm_onIntersept <- lm(read_scr~1, data = no_math_data)

#regression without intercept
regression_without_intercept <- lm(Test_Score~Ratio- 1)



#### End Initialize Globals####

######## Initial Scatter Plot ######
ScatterPlot <- ggplot(TestData, aes(x = Ratio, y = Test_Score))

ScatterPlot +
  ggtitle("Figure 1.  Scatterplot of Test Scores v. Student to Teacher Ratio") +

  xlab("Student  - Teacher Ratio") +
  ylab("Student's Reading Scores (%)") +
  ylim(min(75),M=max(90)) +
  xlim(min(Ratio),M=max(Ratio))  +


  #Standard lm Regression, blue line
  geom_point(aes(x = Ratio,y = Test_Score))

######## End Initial Scatter Plot ######


####### Regression of Test_Score v. Ratio #######
ScatterPlot +
  ggtitle("Fig. ")+
  xlab("Student  - Teacher Ratio") +
  ylab("Student's Reading Scores (%) ") +
  ylim(min(75),M=max(90)) +
  xlim(min(Ratio),M=max(Ratio))  +
  #annotate("text", x = 24.99, y = 77.2, label = "R^2==0.03824", parse = TRUE) +
  #annotate("text", x = 25.5, y = 76.5, label = "P-Stat==5.467e-05", parse = TRUE) +
  #annotate("text", x = 25.19, y = 75.7, label = "F-Stat==16.62", parse = TRUE) +
  #annotate("text", x = 25.3, y = 75, label = "Y-int==86.42717", parse = TRUE) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, colour = "blue")


####### "Best Regression" #######






TestData<-read.csv("CATestScoreData.csv",header=TRUE)

#Creates needed Dataset
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
  }
}
no_math_data$read_scr <- TestData$read_scr


#Regression of Student Teacher Ratio on Reading Score
LM_Reading_STR <- lm(read_scr ~ str, data=no_math_data)


#NVMAX = 16
LinReg_all_16 <- regsubsets(
  read_scr ~ ., data = no_math_data,
  nvmax = 16, really.big= TRUE ,
  method=c("exhaustive")) #We should try the NBEST =1

#NVMAX = 512
LinReg_all_512 <- regsubsets(
  read_scr ~ ., data = no_math_data, nbest=1,
  nvmax = 512, really.big= TRUE ,
  method=c("exhaustive"))


#Best Model -- BIC = 1239.756
lmNVMAX16 <- lm(read_scr~ `str * el_pct` + `expn_stu * avginc` + `meal_pct * str` + el_pct, data=no_math_data)

#Verifies NVMAX16, but nvmax = 512 -- BIC = 1239.756
LinReg_all_512 <- lm(read_scr~ `str * el_pct` + `expn_stu * avginc` + `meal_pct * str` + el_pct, data=no_math_data)


####### JB Test #######

resid_lm <- LinReg_all_512$residuals
jarque.bera.test(resid_lm)
#Yes, so use LAD

#Enhanced Scatter Plot
scatterplot(no_math_data$str,no_math_data$read_scr,
            main="Enhanced Scatter Plot",
            xlab="Student to Teach Ratio",
            ylab="STudent Test Score",
            sub="   ",
            legend=TRUE)

#Scatterplot the residuals
plot(resid_lm, main = "Residual Plot from Best Regression",
     xlab = "Fitted Y Values",
     ylab = "Deviation")



#Residuals Plot of the linear model:
Fitted_Residuals_Linear_Model<-lmNVMAX16$residuals;
Fitted_Residuals_Linear_Model_Squared<-(Fitted_Residuals_Linear_Model)^2;
plot(no_math_data$str, Fitted_Residuals_Linear_Model_Squared,
     main="Fitted Residuals of the Linear Model",
     xlab="Student to Teach Ratio",
     ylab="Residuals Squared",
     sub="   ")

#Residuals Plot of the linear model:
Fitted_Residuals_Linear_Model<-LM_Reading_STR$residuals;
Fitted_Residuals_Linear_Model_Squared<-(Fitted_Residuals_Linear_Model)^2;
plot(no_math_data$str, Fitted_Residuals_Linear_Model_Squared,
     main="Fitted Residuals of the Best Fit Model",
     xlab="Student to Teach Ratio",
     ylab="Residuals Squared",
     sub="   ")



#LAD with all interactions, but nvmax = 512 -- BIC = 1245.196
LAD_LinReg_all_512 <- lad(read_scr~ `str * el_pct` + `expn_stu * avginc` + `meal_pct * str` + el_pct, data=no_math_data)
BIC(LAD_LinReg_all_512)
resid_lm <- LAD_LinReg_all_512$residuals
jarque.bera.test(resid_lm)
#Still yes

#Scatterplot the LAD residuals
plot(resid_lm, main = "Residual Plot from Best Regression LAD",
     xlab = "fitted Y values",
     ylab = "Deviation")

###### Detecting Heteroskadicity #####
#BP Test
bptest(read_scr~ `str * el_pct` + `expn_stu * avginc` + `meal_pct * str` + el_pct, data=no_math_data)
#We reject the Null at the 15.12% significance level

resid_lm_sqrd = resid_lm * resid_lm

#White Regression
whiteTerms <-TestData
whiteTerms$math_scr <- NULL
whiteTerms$testscr <- NULL
whiteTerms$X <- NULL
whiteTerms$X.1 <- NULL
whiteTerms$read_scr <- NULL
whiteRegressors <- as.matrix(whiteTerms)
whiteRegressors
poly(whiteRegressors, degree = 2, raw = TRUE)
whiteRegression <- lm(resid_lm_sqrd ~ poly(whiteRegressors, degree = 2, raw = TRUE))

# Test the null of no relationship: use N*R_squared
whiteStat <- nrow(whiteRegressors)*summary(whiteRegression)$r.squared
quantile <- qchisq(.95,length(whiteRegression$coefficients)-1)
Reject <- (whiteStat > quantile)
print(Reject)
#We do reject the Null at the 5% significance Level




############################################ OUTPUTS #########################################################


#Get Best Models from NVMAX 16
data.frame(
  BIC = which.min(summary(LinReg_all_16)$bic),
  Adjusted_R2 = which.max(summary(LinReg_all_16)$adjr2),
  R2 = which.max(summary(LinReg_all_16)$rsq),
  RSS = which.min(summary(LinReg_all_16)$rss))

#Get Best Models from NVMAX 512
data.frame(
  BIC = which.min(summary(LinReg_all_512)$bic),
  Adjusted_R2 = which.max(summary(LinReg_all_512)$adjr2),
  R2 = which.max(summary(LinReg_all_512)$rsq),
  RSS = which.min(summary(LinReg_all_512)$rss))




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

#NVMAX 512 Best Model Summary
sink("NVMAX 512 Best Model Summary.txt")
print(summary(LinReg_all_512))
print(summary(LinReg_all_512))
print("BIC: ")
print(BIC(LinReg_all_512))
sink()



#Best Predicted Model from NVMAX 16
write.csv(rbind(
  as.data.frame((summaryHH(LinReg_all_16))[4,])), #***Change [#,] to correct model prior to use***
  file = "Best NVMAX 16 Model.csv")


#Best Predicted Model from NVMAX 512
#write.csv(rbind(
#  as.data.frame((summaryHH(LinReg_all_512))[4,])),#***Change [#,] to correct model prior to use***
#  file = "Best NVMAX 512 Model.csv")




#Excel Outputs From All Generated Models
#All NVMAX 16 Models
write.csv(rbind(
  as.data.frame(summaryHH(LinReg_all_16)[1:16,])),
  file = "Model Summaries NVMAX 16.csv")

#All NVMAX 512 Models
#write.csv(rbind(
#  as.data.frame(summaryHH(LinReg_all_512)[1:512,])),
#  file = "Model Summaries NVMAX 512.csv")




#NVMAX 16 In-or-Out Table
write.csv(as.data.frame(
  summary(LinReg_all_16)$outmat),
  file = "Variables In-or-Out Table NVMAX 16.csv")

#NVMAX 512 In-or-Out Table
#write.csv(as.data.frame(
#  summary(LinReg_all_512)$outmat),
#  file = "Variables In-or-Out Table NVMAX 512.csv")



################################################################################
############################ MATH REGRESSIONS ##################################
################################################################################

#Import data again
TestData<-read.csv("CATestScoreData.csv",header=TRUE)

#Regression of math_scr on str
basic_model <- lm(math_scr~Test_Score, data = TestData)
summary(basic_model)


#Finding the best linear Model
no_read_data <-TestData
no_read_data$testscr <- NULL
no_read_data$X <- NULL
no_read_data$X.1 <- NULL
no_read_data$read_scr <- NULL
size = ncol(no_read_data)

#NVMAX = 16
LinReg_all_16 <- regsubsets(
  math_scr ~ ., data = no_read_data,
  nvmax = 16, really.big= TRUE ,
  method=c("exhaustive"))

summary(LinReg_all_16)
which.min(summary(LinReg_all_16)$bic)
#Best Linear Model (I think) -- BIC = 1258.1
lmNVMAX16 <- lm(read_scr~ meal_pct + avginc + el_pct, data=no_math_data)
BIC(lmNVMAX16)

#Finding best non-linear model
no_read_data <-TestData
no_read_data$math_scr <- NULL
no_read_data$testscr <- NULL
no_read_data$X <- NULL
no_read_data$X.1 <- NULL
no_read_data$read_scr <- NULL
size = ncol(no_read_data)

for (x in 1:size) {
  y = x
  while (y <= size) {
    new_name <- paste(colnames(no_read_data)[x], "*", colnames(no_read_data)[y])
    no_read_data$bullshit = no_read_data[,x] * no_read_data[,y]
    names(no_read_data)[names(no_read_data) == "bullshit"] <- new_name
    y = y + 1
    #print(head(no_read_data, n=1L))
  }
}
head(no_read_data)
no_read_data$math_scr <- TestData$math_scr

#NVMAX = 16
NonLinReg_all_16 <- regsubsets(
  math_scr ~ ., data = no_read_data,
  nvmax = 16, really.big= TRUE ,
  method=c("exhaustive"))

summary(NonLinReg_all_16)
which.min(summary(NonLinReg_all_16)$bic)

#Best Non-linear Model (I think) -- BIC = 1245.453
nlmNVMAX16 <- lm(read_scr~ `expn_stu * avginc` + `calw_pct * avginc` + meal_pct + el_pct, data=no_math_data)
BIC(nlmNVMAX16)


####### JB Test #######

resid_lm <- nlmNVMAX16$residuals
jarque.bera.test(resid_lm)
#Yes, so use LAD

#Scatterplot the residuals
plot(resid_lm, main = "Residual Plot from Best Regression",
     xlab = "fitted Y values",
     ylab = "Deviation")

#LAD with all interactions, but nvmax = 16 -- BIC = 1245.196
LAD_lmNVMAX16 <- lad(read_scr~ `expn_stu * avginc` + `calw_pct * avginc` + meal_pct + el_pct, data=no_math_data)
BIC(LAD_lmNVMAX16)
resid_lm <- LAD_lmNVMAX16$residuals
jarque.bera.test(resid_lm)
#Still yes

#Scatterplot the LAD residuals
plot(resid_lm, main = "Residual Plot from Best Regression LAD",
     xlab = "fitted Y values",
     ylab = "Deviation")

###### Detecting Heteroskadicity #####
#BP Test
bptest(read_scr~ meal_pct + avginc + el_pct, data=no_math_data)
#We reject the Null at the 11.04% significance level

resid_lm <- nlmNVMAX16$residuals
resid_lm_sqrd = resid_lm * resid_lm

#White Regression
whiteTerms <-TestData
whiteTerms$math_scr <- NULL
whiteTerms$testscr <- NULL
whiteTerms$X <- NULL
whiteTerms$X.1 <- NULL
whiteTerms$read_scr <- NULL
whiteRegressors <- as.matrix(whiteTerms)
whiteRegressors
poly(whiteRegressors, degree = 2, raw = TRUE)
whiteRegression <- lm(resid_lm_sqrd ~ poly(whiteRegressors, degree = 2, raw = TRUE))

# Test the null of no relationship: use N*R_squared
whiteStat <- nrow(whiteRegressors)*summary(whiteRegression)$r.squared
quantile <- qchisq(.95,length(whiteRegression$coefficients)-1)
Reject <- (whiteStat > quantile)
print(Reject)
#We do reject the Null at the 5% significance Level


################################################################################
############################ IV Predictions   ##################################
################################################################################

#Need to use the simple model that predicts on only str. (Modlm)
Modlm <- lm(read_scr~str, data= no_math_data)
#Point Prediciton
#Predicts a point of 83.392
point_prediction <- predict(Modlm, newdata = data.frame(str = c(15)))

#Confidence Interval
#Predicts [82.771, 84.012] centered on 83.392
confidence_interval_prediction <- predict(Modlm, newdata = data.frame(str = c(15)), interval = "confidence")

#Density Prediction
var(Modlm$fitted.values - TestData$read_scr)
#Variance is 5.934
#Density Prediction is N(83.392, 5.934)

################################################################################
############################  V Predictions   ##################################
################################################################################

#We now have all data points, so we can use out best model, 