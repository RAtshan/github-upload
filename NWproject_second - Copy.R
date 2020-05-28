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
###########Best Subset Selection#######################
###>>> Best Subset method
bestsubset.fit = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , 
                            data = hr.attrit, nvmax = length(attrit.fit1), nbest = 1)
bestsub.summary= summary(bestsubset.fit)

names(bestsubset.fit) ##maesures for the summary

bestsub.summary$rsq
bestsub.summary$adjr2

par(mfrow = c(2,1))
plot(bestsub.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")
plot(bestsub.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")

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
summary(fwd.fit)

plot(fwd.fit, scale = "Cp")
coef(fwd.fit, which.min(summary(fwd.fit)$cp))

#backward
bwd.fit = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours  , 
                     data = hr.attrit, method = "backward",nvmax = length(attrit.fit1), nbest = 1)
summary(bwd.fit)
plot(bwd.fit, scale = "Cp")
coef(bwd.fit, which.min(summary(bwd.fit)$cp))

# To obtain confidence intervals for the coefficient estimates
confint(attrit.fit1)
## odds ratios only
exp(coef(attrit.fit1))
##Writing my owen predict() funcion for regsubset()
predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]]) # Extract the formula used when we called regsubsets()
  mat = model.matrix(form,newdata)    # Build the model matrix
  coefi = coef(object,id=id)          # Extract the coefficiants of the ith model
  xvars = names(coefi)                # Pull out the names of the predictors used in the ith model
  mat[,xvars]%*%coefi               # Make predictions using matrix multiplication
}


##Cross-Validation
k = 10                 # number of folds
set.seed(1)            # set the random seed so we all get the same results

# Assign each observation to a single fold
folds = sample(1:k, nrow(hr.attrit), replace = TRUE) #how we sample data
cv.errors = matrix(NA, k, 20, dimnames = list(NULL, paste(1:20))) #empty matrix to record the error

#all this is explained in intro to Statistical Learrning book P:250-251
#elements of folds = j are in test set
#compute the test errors on the appropriate subset and store them in the appropriate slot in the matrix cv.error
#We wil have 10X30 matrix were (i,j) element crosspond to the test MSE of ith CV fold for the best
#j-variable model

# Outer loop iterates over all folds
for(j in 1:k){
  
  # The perform best subset selection on the full dataset, minus the jth fold
    best.fit = regsubsets(Attrition ~  Age + DistanceFromHome + EducationField 
                          + JobSatisfaction + Gender + JobInvolvement + JobRole + MaritalStatus 
                          + NumCompaniesWorked + OverTime + RelationshipSatisfaction + TotalWorkingYears 
                          + WorkLifeBalance + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole
                          + YearsSinceLastPromotion + YearsWithCurrManager,data = hr.attrit[folds!=j,], nvmax = 20)
    
    # Inner loop iterates over each size i
    for( i in 1:20){
    
      # Predict the values of the current fold from the "best subset" model on i predictors
      pred = predict(best.fit, hr.attrit[folds== j,],id = i)
      
      # Calculate the MSE, store it in the matrix we created above
    cv.errors[j,i] = mean( ( hr.attrit$Attrition[folds==j]-pred)^2)
  }
}


# Take the mean of over all folds for each model size
mean_cv_errors = apply(cv.errors, 2, mean)

# Find the model size with the smallest cross-validation error
min = which.min(mean_cv_errors)

# Plot the cross-validation error for each model size, highlight the min
plot(mean_cv_errors, type='b')
points(min, mean_cv_errors[min][1], col = "red", cex = 2, pch = 20)
#We see that cross-validation selects an 20-predictor model. Now let's use best subset 
#selection on the full data set in order to obtain the 20-predictor model.
reg.best = regsubsets(Attrition ~ . - EmployeeCount -EmployeeNumber - Over18 - StandardHours, 
                      data = hr.attrit, nvmax = 20 )
coef(reg.best, 20)

#visulaiztion of CV results
par(mfrow=c(2,2))
reg_summary = summary(reg.best)
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

#produce diagnostic plots for logistic regression
##################    Improtant note before submitting  #################

### NEED to ad plot titles, and what ever else
par(mfrow = c(2,2))
plot(attrit.fit1)














#str(attrit.fit1)

#to get CI
#confint(attrit.fit1)

#to get standard CI
#confint.default(attrit.fit1)


## multicollinearity test
library(rockchalk)
mcDiagnose(attrit.fit1)

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
