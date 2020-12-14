########################## Assignment 4 #########################################
### Question 1 ###############

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

hpriceTable <- wpull('hprice1')
md1 <- lm(price ~ sqrft + I(bdrms^2) + lotsize + I(sqrft^2) + colonial + assess + assess:lotsize, data = hpriceTable)
summary(md1)
AIC(md1)
BIC(md1)
Equation: price = (1.511*e+02) - (1.046*e-1)*sqrft - (9.394*e-1)*bdrms^2 - (2.771*e-3)*lotsize + (2.427*e-5)*sqrft^2 + (2.006*e+1)colonial + (6.692*e-1)*assess 
                  + (1.072*e-5)*lotsize*assess 
R-Squared = 85.74%, AIC = 910.5522, BIC = 932.8483
This model had the lowest AIC and BIC.


##############################################################################################################
#### Question 2
gpa2 <- wpull('gpa2')
md2 <- lm(colgpa ~ sat + I(sat^2) + tothrs + athlete + verbmath + I(hsize^2) + hsize + hsrank + I(hsrank^2) + hsperc + female + black + sat:hsrank, data = gpa2)
summary(md2)
AIC(md2)
BIC(md2)

colgpa = 2.844 - (1.677*e^-3)*sat + (1.566*e^-6)*sat^2 + (1.707*e^-3)*tothrs + 
        (1.336*e^-1)*athlete - (8.645*e^-2)*verbmath + (2.846*e^-4)*hsize^2 + (2.260*e^-2)*hsize - 
        (5.627*e^-4)*hsrank + (5.196*e^-6)*hsrank^2 - (7.993*e^-3)*hsperc + (1.526*e^-1)*female - 
        (3.416*e^-1)black - (2.755*e^-6)*sat*hsrank

R-Squared = 32.71%, AIC = 6675.001, BIC = 6769.917
This model had the lowest AIC and BIC

#############################################################################################################
### Question 3
mlb1 <- wpull('mlb1')
md3 <- lm(log(salary) ~ teamsal + nl + years + games + atbats + runs + hits + doubles + triples + hruns + rbis + bavg + bb + fldperc + frstbase + scndbase + shrtstop + whitepop + gamesyr + hrunsyr + allstar + hispph, data = mlb1)
summary(md3)
AIC(md3)
BIC(md3)

R-Squared = 67.45%, AIC = 712.7723, BIC = 803.9505
This model had the lowest AIC and BIC.

#############################################################################################################
### Question 4
(i)
library('plm')
rental <- wpull('rental')
pctstu = (rental$enroll/rental$pop)*100
pctstu
y90 = ifelse(rental$year == '90',1,0)
y90
md4 <- plm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model = "pooling", data = rental)
summary(md4)

#The rents were higher in 1990. 
#The variable 'y90' states that there is a 26.22% increase 
#in rent for a 10 year period holding the other factors constant.

#There is 0.5% increase in rent for every one point increase in 'pctstu'

(ii)
# The standard errors in part 1 is not valid. We are not differentiating.

(iii) 

log(rent) = 0.3855 + 0.0722*log(pop) + 0.3099*log(avginc) + 0.0112*pctstu
# The co-efficient of pctstu = 0.0112,
# This implies that 1-point increase in pctstu increases the rent by 1.12%
# The t-value = 2.7114 which is statistically significant,
# we conclude that the relative size of student population impacts the rent.

(iv) 
The estimated fixed effects models equation is
log(rent) = 0.3855 + 0.0722*log(pop) + 0.3099*log(avginc) + 0.0112*pctstu
Std Error:     0.0368245   0.0883426         0.0664771           0.0041319
co-efficients and standard errors are similar for both the first differencing model and fixed effects model

##############################################################################################################
### Question 5 
(i)
MURDER <- wpull ('murder')
pmurder <- pdata.frame(MURDER,index = c('state','year'))
pmurder

#The sign of b1 should be negative. 
#It is not possible to determine the sign of b2.

(ii) 
murder <- subset(pmurder, year == '90'| year =='93')
murder
mdmurder <- plm((mrdrte) ~ exec + unem, model = "pooling", data = murder)
summary(mdmurder)
#Estimated pooled OLS equation:
#mrdrte = -4.889 + 0.11491(exec) + 2.2875(unem)
#Co-efficient of exec is B1 = 0.11491, co-efficient of 'unem' is b2 = 2.2875
#Since the co-efficient of 'unem' is positive, there is no deterring effect.

(iii)
pmurder <- subset(MURDER, year == '90'| year =='93')
pmurder1 <- pdata.frame(pmurder,index = c('state','year'))
mdmurder <- plm(mrdrte ~ exec + unem, model = "fd", data = pmurder1)
summary(mdmurder)
# Estimated the equation by using first differences is:
 # mrdrte = 0.413267 - 0.103840*exec - 0.066591*unem
# There is deterring effect but its not strong enough to affect the model

(iv)

coeftest(modelmurder41,vcovHC)
# The Hetero-Skedasticity robust standard errors in the pooled OLS model are:
 Intercept     exec        unem
 5.25713    0.13863   2.28750
 
 (v)
 murder_1993 <- subset(MURDER,year == 93) 
 murder_1993[order(exec)]
 #The state with highest number of executions in 1993 is Texas with exec = 34
 #The state with the second highest is virginia with 11 executions
 
 
(vi)
 murdernotTX <- subset(pmurder[state != 'TX'])
 murdernotTX
 pmurder20 <- pdata.frame(murdernotTX,index = c('state','year'))
 pmurder20
 mdmurder43 <- plm(mrdrte ~ exec + unem, model = "fd", data = pmurder20)
 summary(mdmurder43)
 coeftest(modelmurder43,vcovHC)
 
 Estimated the equation using first differences by dropping Texas from the model and taking years 1990 and 1993
 mrdrte = 0.412523 + -0.067471*exec + -0.070032*unem
 0.211283     0.104913          0.160371
 The heteroskedasticity-robust standard errors:
 mrdrte = 0.412523 + -0.067471*exec + -0.070032*unem
 0.194331      0.076690         0.141755
 The effect is nearly twice as big, but it is insignificant.
 
 (vii)
 mdmurder444 <- plm(mrdrte ~ as.factor(year) + exec + unem, model = "within", data = pmurder)
 summary(mdmurder444)
 
 #mrdrte = 1.55621*year90 + 1.73324*year93 - 0.13832*exec + 0.22132*unem
 #The co-efficient of 'exec' when  3 years are included is -0.138.
 #But the co-efficient when
 #only 2 years is included is 0.1038 based on the fixed effects model.
 #The deterring effect is  significant in a 3-year model,
 #but it is insignificant in case of a 2-year model
 
 
 
 
###########################################################################################################
### Question 6 

(i)
airfare <- wpull('airfare')
pairfare <- pdata.frame(airfare,index=c('id','year'))
yr97 = ifelse(airfare$year == 1997,1,0)
yr97
yr98 = ifelse(airfare$year == 1998,1,0)
yr99 = ifelse(airfare$year == 1999,1,0)
yr00 = ifelse(airfare$year == 2000,1,0)
yr00
concen <- airfare$bmktshr
concen
md_airfarea<- plm(log(fare) ~ yr97 + yr98 + yr99 + yr00 + bmktshr + ldist + I(ldist^2), model = "pooling", data= pairfare)
summary(md_airfarea)
0.1 increase in 'concen' there would be a 3.52% increase in fare

(ii)
distsq <- log(airfare$dist^2)
distsq
md_airfare2 <- lm(log(fare) ~ yr97 + yr98 + yr99 + yr00 + bmktshr + log(dist) + distsq, data = airfare)
summary(md_airfare2)
confint(md_airfare2)
md_airfare3 <- lm(log(fare) ~ yr97 + yr98 + yr99 + yr00 + bmktshr + log(dist) + distsq, data = pairfare)
summary(md_airfare3)
confint(md_airfare3)
coeftest(md_airfare2,vcov. = vcovHC )
confint1 <- 0.31497 + (1.96*0.0323)
confint1
confint2 <- 0.31497 - (1.96*0.0323)
confint2
# The 95% confidence interval = [0.251662,0.378278] 
# the normal confidence interval = [0.26462,0.38316]. 
# This shows that both models have almost the same confindence interval with the other being slightly bigger.

(iii)
#At 79.50 miles, the relationship between 'log(fare)' and 'dist' is positive.

(iv)
md_airfare4 <- plm(log(fare) ~ bmktshr + log(dist) + distsq, model = "within",effect = 'twoways', data = pairfare)
summary(md_airfare4)
md_airfare5 <- plm(log(fare) ~ yr97 + yr98 + yr99 + yr00 + bmktshr + log(dist)+ distsq, model = "within", data= pairfare)
summary(md_airfare5)
#The co-efficient b1 = 0.017. This indicates that a 0.1% increase in concentration leads to a 1.7% increase in fare

(V)
# Avg. passengers per day and fraction market might be the other 2 characteristics that might affect and highly correlate with concentration

(Vi)
#From the model, it is convincing that a higher concentration results in increase in fares.
#Also there is a correlation between concentration and characteristics like route, distance etc.
#The co-efficient of b1 from the fixed affects model is a strong estimate to support the statement.

#############################################################################################################

### Question 7
(i)
loanapp <- wpull('loanapp')
md_loanapp <- glm(approve ~ white,family = binomial,data = loanapp)
summary(md_loanapp)
fitted.values(md_loanapp)
md_loanapp2 <- lm(approve ~ white,data = loanapp)
summary(md_loanapp2)
fitted.values(md_loanapp2)
mean(fitted.values(md_loanapp2))
#Estimated logit model is approve = 0.8847 + 1.4094*white
#The estimated probabilities for whites = 0.9083879 and non-whites = 0.7077922
#The estimated probabilities are same as the linear probability estimates

(ii)
md_loanapp3 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,family=binomial(link=probit),data = loanapp)
summary(md_loanapp3)
#After adding all the given variables, the co-efficient of white = 0.520254
#t-value of white = 5.371 which is statistically significant
#There is evidence of discrimination against the non-whites

###########################################################################################################
## Question 8 

(i) 
rowalcohol <- nrow(alcohol)
rowalcohol
alcoholempl <- subset(alcohol[status==3])
alcoholempl
rowalcoholempl <- nrow(alcoholempl)
rowalcoholempl
alcoholabuse <- subset(alcohol[abuse==1])
alcoholabuse
rowalcoholabuse <- nrow(alcoholabuse)
rowalcoholabuse
fractionempl <- (rowalcoholempl/rowalcohol)*100
fractionempl
fractionabuse <- (rowalcoholabuse/rowalcohol)*100
fractionabuse
#people who are employed at time of interview is 89.82%
#people who had alcohol abuse at time of interview is 9.92%

(ii)
md_alcohol<- lm(employ ~ abuse, data = alcohol)
md_alcohol
summary(md_alcohol)
coeftest(md_alcohol,vcov.=vcovHC)
margins(md_alcohol)
#The estimated OLS model is md_alcohol<- lm(employ ~ abuse, data = alcohol)
#Equation is: employ = 0.9010 - (0.0283) * abuse, standard error= 0.010206
#Normal Standard errors are: 
#  intercept      abuse
#0.003214      0.010206
#Heteroskedasticity standard errors are:   
#  intercept       abuse
#0.0031755    0.0111529
#There is less difference between the normal and robust standard errors. 
# Yes, it is statistically significant.

(iii)
md_alcohol2 <- glm(employ ~ abuse, family = binomial, data = alcohol)
summary(md_alcohol2)
margins(md_alcohol2)
#The estimated logit model is: employ= 2.20832 - 0.28337*abuse, 
#Standard error= 0.010206. No change in sign and it is statistically significant
# margins of linear model = -0.0283 and logit model is -0.02589.
#There is no much difference in average marginal effect.

(iv)
fitted.values(md_alcohol)
fitted.values(md_alcohol2)
#For abuse = 1, value = 0.8726899 and for abuse = 0, value = 0.9009946

(v)
md_alcohol3 <- lm(employ ~ abuse + age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, data = alcohol)
summary(md_alcohol3)
margins(md_alcohol3)
# The co-efficient of abuse = -0.02025 it is insignificant at the 5% significance level

(Vi)
md_alcohol4 <- glm(employ ~ abuse + age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, family = binomial, data = alcohol)
summary(md_alcohol4)
margins(md_alcohol4)
#The co-efficient for abuse = -0.2295500, t-value = -2.145
#The avg marginal effect of abuse for given logit model is -0.01938
#logit model is not identical to that of linear model but it is very close
#significant at the 5% significance level.

(Vii)
md_alcohol5 <- glm(employ ~ abuse + exhealth + vghealth + goodhealth, family=binomial, data=alcohol)
margins(md_alcohol5)
#The estimated effect = -0.01746 which is greater than that of previous model = -0.02025. 
#adding these factors which are statistically significant make the model better.

(Viii)
md_alcohol6 <- lm(abuse ~ age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3 + mothalc + fathalc, data = alcohol)
summary(md_alcohol6)
# the abuse variable is influenced by many factors. It is endogenous.
#The t-value of mothalc = 2.733, p-value = 0.00630 and t-value of fathinc = 5.707 and p-value almost = 0
#They are statistically significant even at 1% level. 
# factors 'mothalc' and 'fathalc' are influencing abuse

###############################################################################################################
### Question 9 

fertil1 <- wpull('fertil1')
(i)
yr74 = ifelse(fertil1$year == 74,1,0)
yr76 = ifelse(fertil1$year == 76,1,0)
yr78 = ifelse(fertil1$year == 78,1,0)
yr80 = ifelse(fertil1$year == 80,1,0)
yr82 = ifelse(fertil1$year == 82,1,0)
yr84 = ifelse(fertil1$year == 84,1,0)
md_fertil <- glm(kids ~ educ + age + I(age^2) + black + east + northcen + west + farm + othrural + town + smcity + yr74 + yr76 + yr78 + yr80 + yr82 + yr84,family = poisson,data = fertil1)
summary(md_fertil)

#kids = -3.0604 - 0.048*educ + 0.2044*age - 0.0022*agesq + 0.3603*black + 0.087*east + 0.1417*northcen + 0.0795*west - 0.0148*farm - 0.0572*othrural + 0.0306*town + 0.0741*smcity + 0.0932*y74 - 0.0287*y76 - 0.0156*y78 - 0.0196*y80 - 0.1926*y82 - 0.2143*y84
#The co-efficient of year82 = -0.1926076. 
# fertility rate decreases by 19.26% from 1972 to 1982

(ii)
estdifference <- exp(0.3603475)-1
estdifference
#Black women had on average 36% more children than non-black women

(iii)
fitted.values(md_fertil)
kids_estimate <- md_fertil%>%predict(type="response")
kids_estimate
correlation_kids_estimate <- cor(fertil1$kids,kids_estimate)
correlation_kids_estimate
rsquared1 <- correlation_kids_estimate^2
rsquared1
model_fertil2<- lm(kids ~ educ + age + I(age^2) + black + east + northcen + west + farm + othrural + town + smcity + year74 + year76 + year78 + year80 + year82 + year84,data = fertil1)
summary(md_fertil2)
#The fitted values of Poisson model computed is:
#The correlation between kids and kids estimate = 34.769%
#R-Squared = (0.34769)^2
#R-Squared = 0.12089 
#R-Squared of linear model = 0.1295
#the R-Squared for linear model is greater than that of the Poisson distribution model
































































 
 


























