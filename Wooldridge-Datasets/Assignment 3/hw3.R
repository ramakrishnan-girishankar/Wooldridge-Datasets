#Q1
mlb1 <- wpull('mlb1')
# ln(salary) = β0 + β1years + β2gamesyr + β3bavg + β4hrunsyr + β5rbisyr + β6runsyr + β7fldperc
#+ β8allstar + β9frstbase + β10scndbase + β11thrdbase + β12shrtstop + β13catcher + u
#i
mlb_model <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(mlb_model)
# At 5% significance level the Null Hypothesis H0:β13=0 cannot be rejected as the p-value is greater than 0.05(0.05432)
# which means that catchers and outfielders on an average earn the same amount

#ii
mlb_model2 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,data=mlb1)
summary(mlb_model2)
anova(mlb_model,mlb_model2)
# From the ANOVA we can see that the model even at the 10% significance level, the p-value is greater(0.1168)
# hence we cannot reject the Null Hypothesis that there is no difference in average salary across positions, once other factors have been controlled for

#iii
# The result from part1 and part2 is consistent

#Q2
gpa2 <- wpull('gpa2')
summary(gpa2)
# colgpa = β0 + β1hsize + β2hsize2 + β3hsperc + β4sat + β5female + β6athlete + u
#i
model_gpa <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
summary(model_gpa)
tidy(model_gpa)
# From the above model we can see that the an increase in SAT scores and atheltic nature results in a good colgpa as it is common
# that a student performing well in school will perform well in college
# I am unsure how the academic percentile in the graduating class would have a negative effect effect on colgpa

#ii
#The equation is of the form colgpa = 1.24 - 0.0569hsize + 0.00468hsize^2 - 0.0132hsperc + 0.00165SAT + 0.155female + 0.169athlete
# The estimated colgpa differential between athletes and non-athletes is 0.169 and it is statistically significant

#iii
model_gpa2 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete,data=gpa2)
summary(model_gpa2)
# There is still a postive effect on colgpa with athlete but the effect is quite low compared to the first model
# That is because athlete has a correlation with sat

#iv
gpa2$math <- (1-gpa2$female)*gpa2$athlete
gpa2$fath <- gpa2$female*gpa2$athlete
gpa2$mnon <- (1-gpa2$female)*(1-gpa2$athlete)
gpa2$fnon <- gpa2$female*(1-gpa2$athlete)
# leave out female nonathletes to be the base group.
model_gpa4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+math+fath+mnon,data=gpa2)
summary(model_gpa4)
#From the above model we can see that there is no ceteris paribus difference between
# women athletes and woman non-athletes

#v
gpa2$male <- (1-gpa2$female)
model_gpa3 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+male*athlete,data=gpa2)
summary(model_gpa3)
# The effect of SAT on colgpa does not differ much based on gender

#Q3
loanapp <- wpull('loanapp')
summary(loanapp)
model_loanapp <- lm(approve~white,data=loanapp)
summary(model_loanapp)
#i
# The sign of β1 is positive

#ii
#The coefficient of white is statistically significant at 0.01%.
#It is practically large as well. The interpretation is that for every
#white person, the chances of getting a mortgage loan approval is 20.060%

#iii
model_loanapp2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
summary(model_loanapp2)
# The coefficient of white is still statistically significant even after adding other variables for control
# The estimates of white people's approval rates is now 12.8820%
# As we include other races, the mangnitude of discrimination is getting reduced

#iv
model_loanapp3 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+white:obrat,data=loanapp)
summary(model_loanapp3)
#Based on the above model, the interaction term is significant

#v
summary(loanapp$obrat)
model_loanapp4 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*(obrat-32)),data=loanapp)
summary(model_loanapp4)
confint(model_loanapp4)
# The confidence interval is between [0.0732462753,0.152430088]

#Q4
hprice1 <- wpull('hprice1')
summary(hprice1)
#i
# price = β0 + β1lotsize + β2sqrft + β3bdrms + u
model_price <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
summary(model_price)
# The standard errors for lot size and square feet are very less compared to other variables

#ii
# ln(price) = β0 + β1 ln(lotsize) + β2 ln(sqrft) + β3bdrms + u
model_price2 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(model_price2)
# From the above model we can see that the standard errors of these variables have increased

#iii
# From the last model we created, we can see that the heteroskedasticity has reduced which
# means there is less variation in error terms and this model would be a better choice than
# the other model

#Q5
gpa1 <- wpull('gpa1')
summary(gpa1)
#i
model_gpa1 <- lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
summary(model_gpa1)
# The equation is of the form
# colGPA = 1.356509 + 0.412952*hsGPA + 0.013344*ACT -0.071034*skipped+ 0.124439*PC
coeftest(model_gpa1,vcov=vcovHC)
residual_variables <- residuals(model_gpa1)
residual_variables
fitted_variables <- predict(model_gpa1)
fitted_variables

#ii
model_gpa12 <- lm(I(residual_variables^2)~fitted_variables+I(fitted_variables^2))
summary(model_gpa12)
h_i <- predict(model_gpa12)
summary(h_i)

#iii
model_gpa15 <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=(1/h_i),data=gpa1)
summary(model_gpa15)

#iv
coeftest(model_gpa15,vcov=vcovHC)
# There is no change in standard error values

#Q6
library(data.table)
library(dplyr)
library(DBI)
library(tseries)
library(TSA)
library(vars)
library(ggplot2)
library(gtable)
library(grid)
library(forecast)
#i
bitcoin <- fread('bitcoin.csv')

#ii
dcoil <- fread('DCOILWTICO.csv')
dex <- fread('DEXUSEU.csv')
gold <- fread('GOLDAMGBD228NLBM.csv')
sp500 <- fread('SP500.csv')
head(dex)
head(dcoil)
head(gold)
head(sp500)

#iii
draft_table <- merge(bitcoin, dcoil,by.x="date",by.y="DATE", all.x= F, all.y= F)
draft_table <- merge(draft_table, dex ,by.x="date",by.y="DATE", all.x= F, all.y= F)
draft_table <- merge(draft_table, gold,by.x="date",by.y="DATE", all.x= F, all.y= F)
draft_table <- merge(draft_table, sp500,by.x="date",by.y="DATE", all.x= F, all.y= F)
summary(draft_table)
final_table <- draft_table[,c(1,6,17:20)]
head(final_table)
final_table$date <- as.Date(final_table$date)
final_table$oil <- as.numeric(final_table$DCOILWTICO)
final_table$euro <- as.numeric(final_table$DEXUSEU)
final_table$gold <- as.numeric(final_table$GOLDAMGBD228NLBM)
final_table$sp500 <- as.numeric(final_table$SP500)
final_table$bitcoin <- final_table$`price(USD)`
summary(final_table)

#iv
plot_bitcoin <- ggplot(final_table,aes(x= date,y=bitcoin)) + geom_line() 
plot_dcoil <- ggplot(final_table,aes(x=date,y=oil)) + geom_line()
plot_dex <- ggplot(final_table,aes(x=date,y=euro)) + geom_line()
plot_gold <- ggplot(final_table,aes(x=date,y=gold)) + geom_line()
plot_sp500 <- ggplot(final_table,aes(x=date,y=sp500)) + geom_line()
plot_bitcoin
plot_dcoil
plot_dex
plot_gold
plot_sp500

#v
bitcoin_model <- lm(bitcoin~sp500+gold+oil+euro,data=final_table)
summary(bitcoin_model)
tidy(bitcoin_model)
# The spurious equation is of the form bitcoin = -43158+10.5*sp500-1.39*gold-57.2*oil+23349*euro

#vi
library(tseries)
kpss.test(diff(final_table$price),null="Trend")
kpss.test(diff(final_table$oil),null="Trend")
kpss.test(diff(final_table$euro),null="Trend")
kpss.test(diff(final_table$gold),null="Trend")
kpss.test(diff(final_table$sp500),null="Trend")

#vii
number_rows <- nrow(final_table)
bitcoin_model2 <- lm(diff(bitcoin)~diff(sp500)+diff(gold)+diff(oil)+diff(euro)+as.numeric(date)[2:number_rows],data=final_table)
summary(bitcoin_model2)
tidy(bitcoin_model2)
# The equation after the differences becomes diff(bitcoin_=21.4+0.871*diff(sp500)-1.42*diff(gold)-2.40*diff(oil)-271*diff(euro)-0.000959*as.numeric(date)[2:number_rows]

#viii
final_table <- final_table[date>=as.Date('2017-01-01')]
plot_bitcoin <- ggplot(bitcoin,aes(x=date,y=bitcoin)) + geom_line() 
plot_sp500 <- ggplot(bitcoin,aes(x=date,y=sp500)) + geom_line()
plot_gold <- ggplot(bitcoin,aes(x=date,y=gold)) + geom_line()
plot_dcoil <- ggplot(bitcoin,aes(x=date,y=oil)) + geom_line()
plot_dex <- ggplot(bitcoin,aes(x=date,y=euro)) + geom_line()
plot_bitcoin
plot_sp500
plot_gold
plot_dcoil
plot_dex

#ix
acf(diff(final_table$bitcoin),na.action=na.pass)
acf(diff(final_table$sp500),na.action=na.pass)
acf(diff(final_table$gold),na.action=na.pass)
acf(diff(final_table$oil),na.action=na.pass)
acf(diff(final_table$euro),na.action=na.pass)

pacf(diff(final_table$bitcoin),na.action=na.pass)
pacf(diff(final_table$sp500),na.action=na.pass)
pacf(diff(final_table$gold),na.action=na.pass)
pacf(diff(final_table$oil),na.action=na.pass)
pacf(diff(final_table$euro),na.action=na.pass)

#x
auto.arima(final_table$bitcoin,d=1,max.p=10,max.q=10)

#xi
bitcoin_model3 <- stats::arima(final_table$bitcoin, c(0,1,0))
bitcoin_forecast <- forecast(bitcoin_model3,h=30)
plot(bitcoin_forecast)

#xii
periodogram(diff(final_table$bitcoin))

#xiii
final_table$weekday <- as.factor(weekdays(final_table$date))
number_rows_2 <- nrow(final_table)
bitcoin_model4 <-lm(diff(bitcoin)~weekday[2:number_rows_2],data=final_table)
final_table$res[2:number_rows_2] <- residuals(lm(diff(bitcoin)~weekday[2:number_rows_2],data=final_table))
summary(bitcoin_model4)
periodogram(final_table$res[2:number_rows_2])

#xiv

diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- final_table %>% dplyr::select(bitcoin,sp500,gold,oil,euro) %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=5,type="both") %>% AIC
bitcoin_model5 <- VAR(x,p=2,type="both")
summary(bitcoin_model5)

#xv
number_rows_3 <- nrow(bitcoin)
bitcoin_forecast2 <- predict(bitcoin_model5,number_rows_3.ahead=30)$fcst$bitcoin
bitcoin_forecast2 <- bitcoin_forecast2[,1]
bitcoin_forecast2 <- final_table$bitcoin[number_rows_3] + cumsum(bitcoin_forecast2)
cbind(bitcoin_forecast$mean,bitcoin_forecast2)
final_table$bitcoin[number_rows_3]+cumsum(predict(bitcoin_model5,n.ahead=30)$fcst$bitcoin[,1])
