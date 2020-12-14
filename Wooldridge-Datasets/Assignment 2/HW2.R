############################################# Home Work 2 ########################################

# Q1
library(data.table)
library(dataframes2xls)
library(ggplot2)
library(dplyr)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db') ## override data connect with dbi
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))  ## print outs the label fle. (totaly 2 files)
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

vote1 <- wpull('vote1')

################################ Question 1  ############################################
# 1.1
vote1 <- wpull('vote1')
model1  <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote1)
summary (model1)
# voteA = 45.087 + 6.08136*log(expendA) -6.6156*log(expendB) + 0.15201(prtystrA)
# B1 = 6.08136
# for every 1 increase in the expenditure A the voteA increases by 6.08136% 
# when the other factors are not considered or they are null

#1.2
# The Null hypothesis is that 1% increase in A expenditures is offset by a 1% increase in B expenditures.
# The Null Hypothesis is true
# Alternative Hypothesis Ha : B1 + B2 not equal to 0


#1.3
# voteA=45.09 + 6.08136log(expendA) - 6.615log(expendB) + 0.15201prtystrA
# ExpenditureA and ExpenditureB both have t- stat that is significant.
# part(ii) analysis is required.

#1.4 
summary(lm(voteA~log(expendA)+I(log(expendA)-log(expendB))+prtystrA,data=vote1))
# null hypothesis equation : voteA= 45.08 - 0.53log(expendA) + 6.6156(log(expendA) - log(expendB)) + 0.15201prtystrA
#t value = -1.002

#1.5
#We cannot reject the null hypothesis


########################################################################################
# Question 2 

lawsch85 <- wpull('lawsch85')

# 2.1
model2 = lm(log(salary)~ LSAT + GPA + log(libvol) + log(cost) + rank, data =lawsch85)
summary(model2)
#The Null Hypothesis: no change in median starting salary compared to rank of law school.
#T stat for Rank is -9.51; can't reject the null hypothesis.
#Median salary grow by 3.3%

#2.2
model2 <- lm(log(salary)~ LSAT + GPA + log(libvol) + log(cost) + rank, data =lawsch85)
summary(model3)
#The t-statistic on LSAT is 1.065 and that on GPA is 2.749 which are significant at 95% confidence level
model3 <- lm(log(salary)~LSAT+I(GPA-LSAT)+log(libvol)+log(cost)+rank, data = lawsch85)
summary(model3)
#TStat  for GPA and LSTAT jointly is 2.749 
#regression R square is 0.8447.which means it is significatnt.

#2.3
model4 <- lm(log(salary)~ LSAT + GPA + log(libvol) + log(cost) + rank + clsize + faculty, data = lawsch85)
summary(model4)
model5 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank+clsize+I(faculty-clsize),data=lawsch85)
summary(model5)
#jointly these 2 factors are not so significant.Joint T stat clsize and faculty; 0.704

#2.4
#The other factors other than salary regression would be number of faculty publication,students undergraduate grades, 
# and other variables.

##################################################################################################################
# Question 3 

Hprice1 <- wpull('Hprice1')
#3.1
model6 <- lm(log(price)~sqrft+bdrms,data=Hprice1)
summary(model6)
#log(price) = B0 + B1*sqrft + B2*bdrms + error
##B0 = 4.766
##B1 = 0.0003794
##B2 = 0.02888
##n = 88
##R-squared = 0.5883
data1 = data.frame(sqrft=150,bdrms=1)
predict(model6, data1, interval="confidence")
#T1 = 150*Beta1 + Beta2
#Adding a 150 sq-ft room will increase the price by 8.576%
#150 sq-ft room we get the best fit=4.851829, with lowest price=4.6952 and highest price=5.008458

#3.2
model7 <- lm(log(price)~I(sqrft-150*bdrms)+bdrms,data=Hprice1)
summary(model7)

#model7 => log(price) = B0 + B1*[sqrft-150*bdrms] + T1*bdrms + Error

#3.3
modelbd<-lm(log(price)~ I(sqrft - 150 * bdrms) + bdrms, data = Hprice1)
confint(modelbd)
# The 95 % confidence level is from 0.0325803714 to 0.1390223618.

################################################################################################################
## Question 4 

Wage2 <- wpull('wage2')
#4.1
model8 <- lm(log(wage)~educ+exper+I(tenure+exper),data = Wage2)
summary(model8)

#log(wage) = b0 + b1*educ + b2*exper + b3*tenure + u
#Null Hypothesis b2 = b3

#4.2
#T2= B2-B3. As per the estimation,
#the coefficent of experience is significant at 5% significance interval.
#The probability of getting t-value is 68%, we fail to reject null hypothesis.
#1 more year of general workforce experience has the same effect on log(wage) as another year.

# log equation becomes: log(wage)= beta0+beta1*educ+theta2*exper+beta3*(exper+tenure)+u



#####################################################################################################################
#Question 5 

Data401KSUBS <- wpull('401KSUBS')

#5.1
KSUBS401_S<- filter(Data401KSUBS, fsize==1)
nrow(KSUBS401_S)
# 2017 single person households in the data

#5.2
Kmodel <- lm(nettfa~inc+age,data=KSUBS401_S)
summary(Kmodel)
#nettfa=-43.03981+0.79932inc+0.84266age + u
#1 year increase in age increases total financial assets
#by $843 and every $1000 increase in annual income increases the 
#financial assets by $800. 

#5.3
#Intercept: -43.03981
#The intercept does not have a meaning 
#the intercept equal to zero means that for both age and income equal to zero

#5.4
#    B2=0.84266,std error = 0.09202.
x<-abs((0.84266-1)/0.09202)
x
# B2-1/std error gives T-value as -1.709846. 
# Calulating  p-value using tstat is 0.04372419 which is greater than 0.01,
# we reject null hypothesis at 1% level.

#5.5
Kmodel2 <- lm(nettfa~inc,data=KSUBS401_S)
summary(Kmodel2)
# we see from the summary of Kmodel2 the coefficient is 0.8207; which is close to
# 0.799 obtained in Q2, since the correlation between the two variables is very low
# multiple regression estimates and single regression estimates are close enough.

####################################################################################################################
# Question 6

Kielmc <- wpull('kielmc')
kielmc<-kielmc[kielmc$year==1981,]

#6.1
Kielmc1 <- lm(log(price)~log(dist),data= Kielmc)
summary(Kielmc1)
#log(price)=8.25750+0.31722log(dist)
#beta1 should be positive
#more costly should be the house
#1% increase in distance is associated with a predicted price that is about .37% higher.

#6.2
Kielmc2<- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,data= Kielmc)
summary(Kielmc2)
#B1 is now 0.0281 and it not  significant to price of the house
#R squared increased to 0.5925
# There are other factors which will explain the model.
# does not really have great significance on price so its coefficient is reduced.

#6.3
Kielmc3<- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2),data= Kielmc)
summary(Kielmc3)
#B1 increases to 0.189 from 0.0281. R squared is now increased to 0.6178(61.78%) 
#significant now which means that as the distance from the home to the interstate increases

#6.4
Kielmc5 <-lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2)+I(log(dist)^2),data= Kielmc)
summary(Kielmc5)
#T value = -1.105
#The square of log(dist) is not so significant


#################################################################################################################
# Question 7 

wage1 <- wpull('wage1')

#7.1
model10 <- lm(log(wage)~educ+exper+I(exper^2),data=wage1)
summary(model10)
# 0.126 + 0.0906*educ + 0.0409*exper - 0.000712*exper^2 + u

#7.2
#exper^2  statistically significant at 1% level 
# t-value is -6.141

#7.3
#100*(0.0409 - 2*0.0007*4) * 1 = 3.53
#100*(0.0409 - 2*0.0007*19) * 1 = 1.43
#return to 5th year is 3.53%
#return to 20th year is 1.43%

#7.4
#-0.0409731/(2*-0.0007121) = 28.7692
# sum(wage1$exper>28.7692)
# There are 121 people having more experience in this sample

####################################################################################################################
Question 8 

wage2 <- wpull('wage2')

#8.1
model11 <- lm(log(wage)~educ+exper+I(educ*exper),data = wage2)
summary(model11)

X <- predict(model11,data.frame(educ=2,exper=1))
Y <- predict(model11,data.frame(educ=1,exper=1))
X-Y

# holding experience ???xed, changing the experience we get value of 0.04725277 
# it is equal to b1+b3*exper

#8.2
#Null hypothesis is b3=0
# alternative hypothesis b3>0

#8.3
summary(lm(log(wage)~educ+exper+I(educ*exper), data =wage2))
summary(lm(log(wage)~educ+exper, data =wage2))

#B3 has t-value of 2.095 
#Greater than critical value.
#Hypothesis should be that B3 > 0.

#8.4
model13 <- lm(log(wage)~educ+exper+I((educ*exper)-10),data = wage2)
summary(model13)
#log(wage)=5.98+0.044educ-0.021exper+0.003educ*(exper-10)
#Theta0  value = 0.044050
#standard error is 0.017391 
#confidence interval is 0.0099194642 to 0.078180123

###################################################################################################################
#Question 9.1 

Gpa2 <- wpull('gpa2')
Model14 <- lm(sat~hsize+I(hsize^2),data=Gpa2)
summary(Model14)
#sat = B0 + B1*hsize + B2*hsize^2 + error
#B0 = 997.981
#B1 = 19.814
#B2 = -2.131      t-value: -3.881
#R-squared = 0.00765
#n = 4137
#The quadratic term is statistically significant with a t-value of -3.881.

#Question 9.2

Gpa2 <- wpull('gpa2')
Model15 <- lm(log(sat)~hsize+I(hsize^2),data= Gpa2)
summary(Model15)
#-19.814/(2*-2.131)
#Differentiating both sides with respect to Hsize and equalting them to zero we arrive at Hsize = 4.648991
#Using the estimated equation from part 1, the "optimal" high school size is 465. 
#R-squared value of 0.00765
# poor model.

#Question 9.3
#No. Data is restricted to students who took SAT and does not take into account of all high school seniors.

#Question 9.4
#-0.0196029/(2*-0.0020872) = 470
#Estimated equation: log(sat)=6.8960291+0.0196029hsize -0.0020872hsize^2 .  
#There is a difference of 470-465=5 for  class size compared to the previous model.

#####################################################################################################################
#Question 10
Hprice1 <- wpull('Hprice1')
#Question 10.1
Hprice_model <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data = Hprice1)
summary(Hprice_model)
# log(price)=-1.29704+0.16797log(lotsize)+0.70023log(sqrft)+0.03696bdrms
#R-squared is 0.643.

#Question 10.2
price<-lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=Hprice1)
predict(price,data.frame(lotsize=20000,sqrft=2500,bdrms=4))
#The predicted value of log(price) is 5.992899 on 20000 lotsize and 2500sqrft and 4 bedrooms .

#Question 10.3
summary(lm(price~lotsize+sqrft+bdrms,data=Hprice1))
# This model is better as it has a better R Squared value.
# R squared value = 67.24%







