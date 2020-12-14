################################ HW3 ##########################################

library(data.table)
library(dataframes2xls)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db') ## override data connect with dbi
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))  ## print outs the label fle. (totaly 2 files)
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

##############################################################################
### Q1 #############
mlb1 <- wpull('mlb1')
##1.1

md1 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc
             +allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(md1)
# Null Hypothesis is B13(catcher) = 0 
# For 5% Significance level the P value for catcher is 0.05432
# The P-value 0.05432 > 0.5 
# The null hypothesis cannot be rejected.
# Catcher and Outfielders earn the same.
# The size is 325
# Salary differential will be 100*[exp(0.2535)-1] = 28.85%

##1.2 
md2 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,data=mlb1)
anova(md2,md1)
# Null Hypothesis H0: years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar = 0
# Beta9=0,Beta10=0,Beta11=0,Beta12=0,Beta13=0
# The P-value = 0.1168 > 0.05 at 5 % significance
# We cannot reject the NUll hypothesis.

##1.3
# part i) and ii) are roughly consistent. 
# Some variables are weakly significant thrdbase and shrtstop with a low t value

#####################################################################

gpa2 <- wpull('gpa2')

##2.1
# colgpa = b0 + b1*hsize + b2*(hsize)^2 + b3*hsperc + b4*sat + b5*female + b6*athelete + u


##2.2
md3 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
summary(md3)

Colgpa = 1.241 - 0.05685*hsize + 0.004675*hsize2 -0.01321*hsperc + 0.001646*sat + 0.1549*female + 0.1693*athlete 
# Coeffecient of athlete is 0.1693
# P value is 6.5e-05 hence it is significant
# GPA difference between athletes and non athletes is 0.16

##2.3
md4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete,data=gpa2)
summary(md4)

# Coefficient of athlete drops when we drop SAT to 0.00544
# P value becomes 0.90318 which is not significant

##2.4




##2.5
# The effect of colgpa on SAT does not differ much by gender.
# 


#################################################################################
##Q3

loanapp <- wpull('loanapp')

#3.1
md7 <- lm(approve~white,data=loanapp)
summary(md7)

# B1 has a positive coefficient 
# discrimination in favor of white population

#3.2

md7 <- lm(approve~white,data=loanapp)
summary(md7)

#The co efficient of white is significant. 
#for every white person, 
#the chances of getting a mortgage loan approval is 20.06% 

#3.3
md8 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
               cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
summary(md8)

# coefficient of white is 0.128820
# chances of approval is 12.88% for white person
# There is still discrimination is favor of white.
# white is highly significant
# the magnitude of discrimination against other races is getting reduced

#3.4
md9 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
               cosign+chist+pubrec+mortlat1+mortlat2+vr+white:obrat,data=loanapp)
summary(md9)

# interaction term white:obrat is significant
# white applicant is penalized less than a nonwhite
# Coefficient is 0.008088 and the t value is 3.531

#3.5


md10 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                cosign+chist+pubrec+mortlat1+
                mortlat2+vr+I(white*(obrat-32)),data=loanapp)
summary(md10)
confint(md10)
# 95% confidence interval is (7.32% , 15.24%)

#############################################################################
#Q4

hprice1 <- wpull('hprice1')

#4.1
md11 <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
summary(md11)
vcov(md11)
vcovHC(md11)
coeftest(md11,vcov=vcovHC)

#price = -21.77 + 0.002068*lotsize + 0.1227*sqrft + 13.85*bdrms
# Lotsize become much less significant
# lot size the standard error increases but the corresponding 
# t and p values make it insignificant.

#4.2 
md12 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(md12)
coeftest(md12,vcov=vcovHC)

# standard errors of lotsize and sqrft has increased.
# lotsize and sqrtft are statistically significant while bdrms is no longer significant.

# 4.3
# using logs, the heteroscedasticity has reduced.
# model would be a better compared to the previous model

###############################################################################
#Q5

gpa1 <- wpull('gpa1')
#5.1
md13 <- lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
summary(md13)
coeftest(md13,vcov=vcovHC)
res <- residuals(md13)
fit <- predict(md13)
#colGPA = 1.356 + 0.41295*hsGPA + 0.01334*ACT - 0.071*skipped + 0.124*PC

#5.2
md14 <- lm(I(res^2)~fit+I(fit^2))
hi <- predict(md14)
summary(hi)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02738 0.08312 0.10521 0.10195 0.11848 0.16480

#5.3
summary(hi)
md15 <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=(1/hi),data=gpa1)
summary(md15)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.401564   0.298430   4.696 6.39e-06 ***
  hsGPA        0.402506   0.083362   4.828 3.65e-06 ***
  ACT          0.013162   0.009827   1.339 0.182698    
skipped     -0.076365   0.022173  -3.444 0.000762 ***
  PC           0.126005   0.056339   2.237 0.026945 *  
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.013 on 136 degrees of freedom
Multiple R-squared:  0.3062,	Adjusted R-squared:  0.2858 
F-statistic: 15.01 on 4 and 136 DF,  p-value: 3.488e-10

#t-value of skipped improved significantly


#5.4
coeftest(md15,vcov=vcovHC)
#standard errors change much from part 3. They are significantly less now.

################################################################################











