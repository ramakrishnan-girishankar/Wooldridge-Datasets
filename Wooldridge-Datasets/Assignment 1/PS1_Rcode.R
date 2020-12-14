
library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(dplyr)
build.nsim <- function(nsim,nvec){
  simdx <- c()
  for(i in 1:length(nvec)) 
    simdx <- c(simdx,rep(1:nsim,each=nvec[i])+(i-1)*nsim)
  dt <- data.table(sim=simdx)
  bigN <- nrow(dt)
  dt$n <- rep(rep(nvec,nvec),each=nsim)
  dt$one <- 1
  dt$simc <- dt[,cumsum(one),by=sim]$V1
  dt$one <- NULL
  return(dt)
}
wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

#Question 1
wage1=wpull("wage1")

#part_1

summary(wage1$educ)
#lowest year of education - 0.00,  
#highest year of education -18.00 
#average education level - 12.56

#part_2
summary(wage1$wage)
#average hourly wage - $5.909, average hourly wage seems to be low

#part_3

cpi_1976 = 56.9
cpi_2010 = 218.1
#As per the Economic Report of the President report : cpi_1976 = 56.9, cpi_2010 = 218.1

#part_4

cpi=cpi_2010/cpi_1976
#Hourly wage in 2010
w_2010=cpi*mean(wage1$wage)
w_2010
# average hourly wage in 2010 = $22.6494
#The new average hourly wage rate seems reasonable as compare the previous average rate

#part_5

table(wage1$female)
#Women: 252. Men :274

#------------------------------------------------------------------------------
#question 2

meap01 =wpull("meap01")

#part_1

summary(meap01$math4)

#largest value of math4 - 100 and 
#smallest values of math4 = 0.
#Range makes sense as students got 100% and 0.00%.

#part_2

sum(meap01$math4==100)
sum(meap01$math4==100)/nrow(meap01) *100
#schools have a perfect pass rate on the math test = 38 which is 2.08%

#part_3
(sum(meap01$math4==50))
#schools have math pass rates of exactly 50% = 17

#part_4

c(mean(meap01$math4),mean(meap01$read4))
#math_prate =71.909%, read_prate= 60.06%
#read is harder to pass

#part_5

cor((meap01$math4),(meap01$read4))
#correlation = 0.8427 or 84.27%
#Math and #read are highly correlated

#part_6

summary(meap01$exppp)
sd(meap01$exppp)
#expenditure per pupil = 5194.865, Standard deviation = 1091.89. 
#yes, there is wide variation in per pupil spending.

#part_7

percent_diff =((6000-5500)/5500)*100
log_diff=(log(6000)-log(5500))*100
# School A's spending exceeds School B's by 9.09%, log_diff : 8.701%

#------------------------------------------------------------------------------
#Question_3

k401= wpull("401k")

#part_1

c(mean(k401$prate),mean(k401$mrate))
# Average Participation rate : 87.36%
# Average match rate : 0.7315 %

#part_2

model_k401=lm(prate~mrate,data=k401)
summary(model_k401)
#prate = 83.0755 + 5.8611 mrate,
# df=1532, Sample = 1534, R-squared = 0.0747

#part_3

#When mrate =0, intercept = 83.0755. The predicted participation rate is 83.07%
#For every 1 percent increace in mrate, the participation rate increases by 5.86%

#part_4

predict(model_1,data.frame(mrate = 3.5))
#When mrate= 3.5, the Participation rate is 103.58%, which make no sense.
#Because Participation rate can not be more than 100%

#part_5

#R-Squared = 7.47%, which is very low.
#The variation in mrate does not explain the variation in prate.

#------------------------------------------------------------------------------
#Question_4

ceosal2=wpull("ceosal2")

#part_1

c(mean(ceosal2$salary),mean(ceosal2$ceoten))
#Average CEO salary : $865.56K 
#Average CEO tenure : 7.95 years

#part_2

sum(ceosal2$ceoten==0)
max(ceosal2$ceoten)

# CEOs in first year = 5
# Longest tenure as CEO =37 years

#part_3

lm_ceosal=lm(log(salary)~ceoten,data=ceosal2)
summary(lm_ceosal)
#ln[salary] = 6.505498 + 0.0097 ceoten + u
#R-Squared =0.01316, Sample= 177, df=175
#For one year increase as CEO, the predicted percentage increase in salary is 0.97% 

#------------- -----------------------------------------------------------------
#Question_5

wage2=wpull("wage2")
names(wage2)

#part_1

c(mean(wage2$wage),mean(wage2$IQ),sd(wage2$IQ))
#Avg salary = $ 957.94545, Average IQ= 101.28235, Standard deviation IQ= 15.05264

#part_2
model_wage=lm(wage~IQ,data=wage2)
summary(model_wage)
15*8.303

# wage = 116.992+8.303 IQ.
# Sample = 935, df = 933, R-Squared = 0.0955 or 9.55%
#15 point increase in IQ, the wage will be increased by 125.53.
#But R-square is 9.5%, the variation in IQ does not explain the variation in Wage.

#part_3

model_ln_wage=lm(log(wage)~IQ,data=wage2)
summary(model_ln_wage)

15*0.0088072

# log(wage) = 5.886 + .0088IQ
# Sample = 935, df = 933, R-Squared = 0.09909 or 9.909%
# 15 point increase in IQ, the wage will be increase by 13.21% 
#-----------------------------------------------------------------------------
#Question 6

meap93=wpull("meap93")
names(meap93)

#part_1

model_meap93=lm(math10~(expend),data=meap93)
summary(model_meap93)
plot <- meap93 %>% ggplot(aes(x=expend,y=math10)) + geom_point() 
plot

# math10 = 13.359 + 0.0024 expend
#Sample Size = 408, df =406,  R- squared = 0.03296
#Diminishing effect seem more appropriate.
#Expend has very less effect on the math pass rate

#part_2
model_meap93_ln=lm(math10~log(expend),data=meap93) 
summary(model_meap93_ln)
#In the given model math10 = b0 + b1 ln[expend] + u, 
#the expend coefficient is in log form thats gives percentage,
#and math10 is also in percentage format. 
#So, 10% increase in expend means a 1/10 increase in log value.
#b1/10 implies one percent increase in math10.

#part_3
model_meap93_ln=lm(math10~log(expend),data=meap93) 
summary(model_meap93_ln)
nrow(meap93)
# math10 = -69.341 + 11.164*ln[expend]
# N=408 ; df= 406; R-square = 2.966%

#part_4

# 10% increase in expend -> 1.064% increase in math rate

#part_5

model_meap93_ln %>% 
  predict %>% 
  summary
#Range of predicted math10 [21.22,30.15]
#The maximum predicted value is 30.15.
# lets assume math10 value be 101.
# the equation becomes: 101 =-69.34+11.164*ln(expend)
#ln(expend)=(101+69.34)/(11.164)
(101+69.34)/(11.164)
#expend is approximately greater than $4.2m

#------------------------------------------------

#question 7

hprice=wpull("hprice1")

#part_1
model_hprice=lm(price~sqrft+bdrms,data=hprice1)
summary(model_hprice)
# price = -19.315 + 0.1284sqrft + 15.198bdrms,
# Sample =88, df =85, R-Squared = 0.6315 or 63.15%


#part_2

#estimated increase in price for a house with one more bedroom, holding
#square footage constant will be by  
b1=15.1982*1 
# $15.198k or $ 15198 

#part_3
#If we add a bedroom and 140 square feet to a house, the estimated increase in 
b1_sq140=((0.1284*140)+15.198)*1000
b1_sq140
# $33174

#part_4

model_hprice %>%
  summary
#R-squared is 63.19% (variation is percentage is explained)

#part_5

psp=predict(model_hprice,data.frame(sqrft=2438,bdrms=4))
#predicted selling price will be $ 354K

#part_6
asp=300 #actual selling price
residual=asp-psp
residual

# Residual = -54k, Underpaid

#------------------------------------------------------------------------
#question 8

ceosal2=wpull("ceosal2")

#part_1

model_ceosal=lm(log(salary)~log(sales)+log(mktval),data=ceosal2)
summary(model_ceosal)

# ln(salary)=4.6209+ 0.1621 sales+ 0.1067 mktval
#R-squared:  0.2991, Sample =177, df =174

#part_2
range(ceosal2$profits)
model_ceosal_2=lm(log(salary)~log(sales)+log(mktval)+(profits),data=ceosal2)
summary(model_ceosal_2)

#	Profit values range from -463 to 2700, some are negatives. We can't use log(profits). 
#Therefore, log (Profit) attribute can't be added to this model. 
#The R-squared is 0.2993, approximately 70% of the variation in log(salary) is unexplained. 
#Profits seems to add very little to the model, 
#suggesting that profits have mild influence on log(salary).

#part_3

model_ceosal_3=lm(log(salary)~log(sales)+log(mktval)+(profits)+ceoten,data=ceosal2)
summary(model_ceosal_3)
#One year 1 year increase in tenure is associated with a 1.167% increase in a CEO's salary.

#part_4

cor(log(ceosal2$mktval),ceosal2$profits)

#Correlation = 0.776, Market value and Profits are highly correlated. 
#Both of these needs to be included.

#---------------------------------------------------
#question9

attend=wpull("attend")

attend=attend%>%
  mutate(atndrte=attend/32*100)
  
#part1
summary(data.table(attend$atndrte,attend$priGPA,attend$ACT))

# atndrte : min :6.25%, max: 100%, average: 81.71%
# PriGPA  : min :0.857, max: 3.930, average: 2.587
# ACT     : min :13.00, max: 32.00, average: 22.51

#part2
model_atndrte=lm(atndrte~priGPA+ACT,data=attend)
summary(model_atndrte)

#  antdrte=75.70+17.261 priGPA - 1.717 ACT
#sample size = 680, df=677 R-squared = 0.2906
#The predicted attend rate is 75.70% for a student who has Zero ACT score and Zero PriGPA. 
#No, it does not have any useful meaning

#part3

# For every point increase in "priGPA", the attendance rate is predicted to increase
#by 17.26% which makes sense.
# but For every point increase in "ACT", the attendance rate is predicted 
#decrease by 1.717% which doesnot make any sense.

#part_4

predict(model_atndrte,data.frame(priGPA=3.65,ACT=20))

# When priGPA=3.65 and ACT=20,the attend rate is over 100 %, which is impossible and makes no sense.

filter(attend,priGPA==3.65,ACT==20)

#one student is the there in these criterias. 
# index number is 568, the original "attendance" is "87.5%"

part_5

predict(model_atndrte,data.frame(priGPA=c(3.1,2.1),ACT=c(21,26)))

predict_diff_atnd=93.1606-67.3172
#The predicted difference between the attendance between student A and student B is 25.846%
#-----------------------------------------------------------------------------
#question 10
htv=wpull("htv")
names(htv)        

#part_1

range(htv$educ)
(count(filter(htv,educ==12))/nrow(htv))*100
c(mean(htv$educ),mean(htv$fatheduc),mean(htv$motheduc))
# range [6,20], 41.62% has completed 12th but no higher degree, 
# Mean education levels of Men, Father, and Mother are 13.03740, 12.44715, 12.17805 respectively.
# So, men have higher education level than their parents

#part_2

model_educ=lm(educ~motheduc+fatheduc,data=htv) 
summary(model_educ)

# Educ = 6.96 + 0.3042 motheduc + 0.1902 fatheduc.
# Sample = 1230, df =1227, R-Squared =0.2493.So, 24.93% of sample vaariation in educ
#is explained by the parents' education.
# for every one year increase in mother's education, 0.3042 years increase in son's year in education

#part_3

model_educ_abil=lm(educ~motheduc+fatheduc+abil,data=htv) 
summary(model_educ_abil)

#Educ = 8.4486 + 0.1891 motheduc + 0.111 fatheduc + 0.5024 abill ; 
#Sample = 1230, df =1226, R-square = 42.75%.
# For every one year increase in ability, 0.5024 years increase in student years in education

#part_4

model_educ_abil_sq=lm(educ~motheduc+fatheduc+abil+I(abil^2),data=htv) 
summary(model_educ_abil_sq)

#educ = 8.2302 + 0.1901 motheduc + 0.1089 fatheduc + 0.4014 abil + 0.0505 abil^2
#Sample = 1230, df =1225, R-square = 0.4444or 44.44%.
#1st derivative = 0
# d(Educ)/d(abil)= 0.4014+ 2*0.0505*abil = 0

abil1= -(0.4014/(2*0.0505))
abil1

# abil = -3.974

#2nd derivative
#d2(Educ)/d2(abil)=0.10
# the second derivative is positive, so there is a minimum 

#part_5
count(filter(htv,abil<abil1))
count(filter(htv,abil<abil1))/nrow(htv)*100


#15 or 1.21%
# Out of 1230 only 15 students are predicted to have ability less than -3.974.
# 1.21% is very low and we dont have enough data for low ability students

#part_6
8.2302 + 0.1901*12.18 + 0.1089*12.45 
#When motheduc=12.18 and fatheduc = 12.45
#educ = 8.2302 + 0.1901*12.18 + 0.1089*12.45 + 0.4014 abil + 0.0505 abil^2
#educ=11.901+ 0.4014 abil + 0.0505 abil^2

model_educ_abil_sq_1=lm(educ~abil+I(abil^2),data=htv) 
summary(model_educ_abil_sq_1)

ggplot(htv,aes(x=abil,y=educ)) + 
  geom_point() + 
  geom_line(aes(y=predict(model_educ_abil_sq_1)),color='red') +
  scale_x_continuous('Student Ability') +
  scale_y_continuous('Education(In years)')