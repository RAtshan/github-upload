          #######################################
          ##  Author Rasha Atshan             ##
          ##  HR Empolyee Attritition         ##
          ## Northwestren Mutual DSI          ##
          ##    Januray, 2019                 ##
          ######################################

          
setwd("C:\Users\atsha\Desktop\Work\Bansal's Work\NMDSI")
rm(list = ls())

library(DescTools)
library(arm)
library(car)
library(rockchalk)
library(visreg)
library(nonnest2)
library(caret) #needed for feature plotlibrary(psych)

          
hr.attrit <- read.csv("HR_Employee_Attrition_NMDSI_data .csv", header = T)
hr.attrit = data.frame(hr.attrit)

summary(hr.attrit)
describe(hr.attrit)
dim(hr.attrit)


##getting the frequency  of each variable
freq.hr = NULL
for ( i in 1 : ncol(hr.attrit)) {
  freq.hr [[i]] <- table (hr.attrit [, i], useNA = "always")
  
}
names(freq.hr) <- colnames(hr.attrit)
freq.hr


#chisq.hr <- chisq.test(hr.attrit$Age, hr.attrit$Attrition, hr.attrit$EnvironmentSatisfaction,
#                        hr.attrit$JobSatisfaction, hr.attrit$JobLevel, hr.attrit$PerformanceRating,
#                         hr.attrit$Education, hr.attrit$WorkLifeBalance)
  
# Our response vairbles are Attrition (No and yes) & PerformanceRating (3 = Excellent & 4 = Outstanding)
hr.attrit$Attrition
freq.hr$Attrition

hr.attrit$PerformanceRating
freq.hr$PerformanceRating

### our regressors/ Indep variables are job envirment and sataificstion, work life balance,
#Age, Education, EnvironmentSatisfaction, Job Role, Job Satisfaction, 

#glm instead of lm 
hr.attrit$new.Attrition = as.factor(hr.attrit$Attrition)  # declaring Attrition as a 2 level factor
new.Attrition = as.numeric(hr.attrit$new.Attrition)     #convert a factor to a numeric vector
colnames(hr.attrit)

hr.attrit$PerformanceRating_factor = as.factor(hr.attrit$PerformanceRating)


#full logistic regression model for attritation
empl.attrit.lm = glm( new.Attrition ~ Age + Education + EnvironmentSatisfaction + JobRole + JobSatisfaction 
          + WorkLifeBalance, , data = hr.attrit, family = binomial(link = "logit") )
summary(empl.attrit.lm)
Anova(empl.attrit.lm, test.statistic = "LR")

#coef(empl.attrit.lm)
exp(coef(empl.attrit.lm)) ##odd ratio
abs(exp(coef(empl.attrit.lm)) - 1) * 100  # the percentage of change

confint(empl.attrit.lm, level = .95)
exp(confint(empl.attrit.lm, level = .95)) ## CI for OR
          
PseudoR2(empl.attrit.lm, "all")


# must finish the graphs
#visreg(empl.attrit.lm)


#full logistic refression model for performance
empl.perform.lm = glm(PerformanceRating_factor ~ Age + Education + EnvironmentSatisfaction + JobRole + JobSatisfaction 
                      + WorkLifeBalance, , data = hr.attrit, family = binomial(link = "logit") )
summary(empl.perform.lm)

exp(coef(empl.perform.lm))
abs(exp(coef(empl.perform.lm))-1)*100 ## the percentage of change

PseudoR2(empl.attrit.lm, "all")


invlogit(coef(empl.attrit.lm)) #[1]required Library(arm)/ it gives proportion of the sample

## multicollinearity test
mcDiagnose(empl.attrit.lm)
mcDiagnose(empl.perform.lm)

# stepwise regression backward elimination/forward selection
empl.attrit.back.elim = step(empl.attrit.lm , direction = "both", trace=FALSE ) 
summary(empl.attrit.back.elim)

empl.perform.back.elim = step(empl.perform.lm, direction = "backward", trace = FALSE)
summary(empl.perform.back.elim)

#some plots
#Need to add a title for all subgraph
par(mfrow = c(2,2))
plot( hr.attrit$Attrition, hr.attrit$Age, col = c("4", "bisque"), main = "Employee Attriation Factors",
      xlab = "Employee Attrition", ylab = "Employee Age")
plot(hr.attrit$Attrition, hr.attrit$Education, col = c("4", "bisque"), main = "Employee Attriation Factors",
     xlab = "Employee Attrition", ylab = "Employee Education")
plot(hr.attrit$Attrition, hr.attrit$EnvironmentSatisfaction, col = c("4", "bisque"), main = "Employee Attriation Factors",
     xlab = "Employee Attrition", ylab = "Employee Satisfaction of Enviroment")

#issues plotting jobrole
#plot( hr.attrit$Attrition , hr.attrit$JobRole, col = c("4", "bisque"), main = "Employee Attriation Factors",
 #     xlab = "Employee Attrition", ylab = "Employee Role")
plot(hr.attrit$Attrition, hr.attrit$ JobSatisfaction, col = c("4", "bisque"), main = "Employee Attriation Factors",
     xlab = "Employee Attrition", ylab = "Employee Satisfaction of the Job")
plot(hr.attrit$Attrition, hr.attrit$WorkLifeBalance, col = c("4", "bisque"), main = "Employee Attriation Factors",
     xlab = "Employee Attrition", ylab = "Employee Blance between Work and Life")



pairs(~ new.Attrition + Age + Education + EnvironmentSatisfaction + JobRole + JobSatisfaction 
      + WorkLifeBalance, , data = hr.attrit, col = "mediumorchid" )
pairs( ~ PerformanceRating_factor + Age + Education + EnvironmentSatisfaction + JobRole + JobSatisfaction 
       + WorkLifeBalance, , data = hr.attrit, col = "blue" )


#produce diagnostic plots for logistic regression 
par(mfrow=c(2,2))
plot(empl.attrit.lm)
plot(empl.perform.lm)


















