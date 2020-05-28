            ######################################
            ##    Author Rasha Atshan           ##
            ##    HR Empolyee Attritition       ##
            ##    Northwestren Mutual DSI       ##
            ##        May, 2019                 ##
            ######################################
            
   
      
setwd("C:/Users/atsha/Desktop/Work/Bansal's Work/NMDSI")
rm(list = ls())
options(max.print=10000) 

library(ggplot2)#library for plotting
library(leaps) # library for model selection
library(rockchalk) ## for mcDiagnose
library(boot)##for cv.glm
      
hr.attrit <- read.csv("HR_Employee_Attrition_NMDSI_data .csv", header = T)
hr.attrit = data.frame(hr.attrit)
summary(hr.attrit)
dim(hr.attrit)
colnames(hr.attrit)

#to find the type of data we're dealing with
# 1.usind one command
lapply(hr.attrit, class)
#sapply(hr.attrit, sd) # apply sd function to each variable in the dataset. 

##################    Improtant note before submitting  #################
### NEED to add plot title, change color, prectange, etc
ggplot(data = hr.attrit, aes(x = Attrition)) + geom_bar(stat = "count")
ggplot(data = hr.attrit, aes(x = Attrition)) + geom_bar(stat = "count",
                fill = "blue3", color = "grey40") + theme_bw() + coord_flip() 

#the distribution of the attrition --- we have imbalanced data
##getting the frequency  of each variable
freq.hr = NULL
for ( i in 1 : ncol(hr.attrit)) {
  freq.hr [[i]] <- table (hr.attrit [, i], useNA = "always")
  
}
names(freq.hr) <- colnames(hr.attrit)
#freq.hr

#decalring categorical variables as factor and numerical variables
#as.numeric(levels(f))[f]
hr.attrit$Attrition = factor(hr.attrit$Attrition, levels = c ('Yes', 'No') , labels = c(1, 2) )
hr.attrit$BusinessTravel = as.factor(as.integer(hr.attrit$BusinessTravel))
hr.attrit$Department = factor(as.integer(hr.attrit$Department))
hr.attrit$EducationField = factor(hr.attrit$EducationField,
                             levels = c('Human Resources', 'Life Sciences', 'Marketing', 'Medical', 'Technical Degree', 'Other'),
                             labels = c(1,2,3,4,5,6))
hr.attrit$Gender = factor(hr.attrit$Gender, 
                          levels = c('Female', 'Male'),
                          labels = c(1,2))
hr.attrit$JobRole = factor(hr.attrit$JobRole,
                           levels = c('Healthcare Representative', 'Human Resources', 'Laboratory Technician', 'Manager', 'Manufacturing Director',
                                      'Research Director', 'Research Scientist', 'Sales Executive', 'Sales Representative') ,
                           labels = c(1,2,3,4,5,6,7,8,9))
hr.attrit$MaritalStatus = factor(hr.attrit$MaritalStatus,levels = c('Single', 'Married', 'Divorced'),labels = c(1,2,3))
hr.attrit$OverTime = factor(hr.attrit$OverTime,levels = c('Yes','No'),labels = c(1,2))
hr.attrit$Over18 = as.numeric(hr.attrit$Over18, levels = c('Yes', 'No'),labels = c(1,2))

#full logistic model of all varibles
attrit.fit1 = glm( Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , data = hr.attrit, 
                   family = "binomial")
summary(attrit.fit1)
#to get the number of parameters in the fitted model.
length(coef(attrit.fit1))
# To obtain confidence intervals for the coefficient estimates
confint(attrit.fit1)
#to get standard CI
confint.default(attrit.fit1)
## odds ratios only
exp(coef(attrit.fit1))
## multicollinearity test
mcDiagnose(attrit.fit1)
###########Best Subset Selection#######################
###>>> Best Subset method
bestsubset.fit = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , 
                            data = hr.attrit, nvmax = length(attrit.fit1), nbest = 1)
bestsub.summary= summary(bestsubset.fit)

names(bestsubset.fit) ##maesures for the summary

bestsub.summary$rsq
bestsub.summary$adjr2

par(mfrow = c(2,1))
plot(bestsub.summary$rss, xlab = "Number of Variables",  ylab = "RSS", type = "l")
plot(bestsub.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

#Location of maximum or minimum point of a vector
which.max(bestsub.summary$adjr2)
which.min(bestsub.summary$cp)
which.min(bestsub.summary$bic)

#visual plots to see the vairbles in each statisca technique 
plot(bestsubset.fit, scale = "r2")
plot(bestsubset.fit, scale = "adjr2")
plot(bestsubset.fit, scale = "Cp")
plot(bestsubset.fit, scale = "bic")

#getting coef of a set # of variables
coef(bestsubset.fit, which.min(bestsub.summary$cp))

###>>> best subset using forward and backwards method
#forward
fwd.fit = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , 
data = hr.attrit, method = "forward",nvmax = length(attrit.fit1), nbest = 1)
fwdfit.summary = summary(fwd.fit)
names(fwdfit.summary)
plot(fwdfit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot (fwdfit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

plot(fwd.fit, scale = "Cp")
coef(fwd.fit, which.min(summary(fwd.fit)$cp))

#backward
bwd.fit = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , 
                     data = hr.attrit, method = "backward",nvmax = length(attrit.fit1), nbest = 1)
bwdfit.summary = summary(bwd.fit)
names(bwdfit.summary)
plot(bwdfit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot (bwdfit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
plot(bwd.fit, scale = "Cp")
coef(bwd.fit, which.min(summary(bwd.fit)$cp))

##Cross-Validation
k = 10                 # number of folds
set.seed(17)            # set the random seed so we all get the same results
cv.error = rep(0,20)
for(i in 1:20) {
  # The perform best subset selection on the full dataset, minus the jth fold
  best.fit = regsubsets(Attrition ~  Age + DistanceFromHome + EducationField 
                        + JobSatisfaction + Gender + JobInvolvement + JobRole + MaritalStatus 
                        + NumCompaniesWorked + OverTime + RelationshipSatisfaction + TotalWorkingYears 
                        + WorkLifeBalance + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole
                        + YearsSinceLastPromotion + YearsWithCurrManager,data = hr.attrit, nvmax = 20)
  cv.error[i] = cv.glm (hr.attrit, best.fit, k = 10)$delta[1]
}
cv.error

  
  
    

#We see that cross-validation selects an 20-predictor model. Now let's use best subset 
#selection on the full data set in order to obtain the 20-predictor model.
reg.best = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours, 
                      data = hr.attrit, nvmax = 20 )
coef(reg.best, 20)
reg_summary = summary(reg.best)
#visulaiztion of CV results
par(mfrow=c(2,2))
# Plot RSS
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
# Plot Adjusted R^2, highlight max value
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max = which.max(reg_summary$adjr2)
points(max, reg_summary$adjr2[max], col = "red", cex = 2, pch = 20)
# Plot Cp, highlight min value
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min = which.min(reg_summary$cp)
points(min,reg_summary$cp[min], col = "red", cex = 2, pch = 20)
# Plot BIC, highlight min value
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min = which.min(reg_summary$bic)
points(min, reg_summary$bic[min], col = "red", cex = 2, pch = 20)


##predictation
attrit.probs = predict(best.fit, type = 'response')
attrit.probs[1:20]

### NEED to ad plot titles, and what ever else
#par(mfrow = c(2,2))
# Diagnostic plots are not used for logstic regression plot(attrit.fit1)


#https://github.com/rstudio/keras/issues/626

#https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset
#factor link
#https://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/

# 2. using a loop
#for (i in 1 : ncol(hr.attrit)) {
#  print(names(hr.attrit[i]))
#  names = names(hr.attrit[i])
#  print(class(hr.attrit [,i]))
#}
