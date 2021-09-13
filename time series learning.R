#######################
########Lec 1##########
#######################
#how to get histogram and change default
getwd()
setwd('E:\\公共课程\\2021年上半期\\时序列学习')
getwd()
install.packages('faraway')
data(package='faraway')
data(coagulation,package='faraway')
ls()
coagulation
plot(coag~diet, data=coagulation)
summary(coagulation)
data = worldcup
hist(data$Time, xlab = 'My data points', main='Histogram of my data', freq = F,
     col ='blue',breaks=10)
lines(density(data$Time),col='yellow',lwd=5)

#######################
########Lec 2##########
#######################
#how to plot scatterplot
set.seed=2016
scores1=round(rnorm(50,78,10))#round 四舍五入
scores2=round(rnorm(50,70,14))
scores1
scores2
plot(scores1~scores2,main='Test scores for two exams',
     xlab='test 2 scores', ylab = 'test 1 scores',
     col=c('red','blue'))

#######################
########Lec 3##########
#######################
#linearRegression-OLS
help(co2)
plot(co2, main='Atmospheric CO2 Concentration')
#Note:linearreg cannot capture all the features in the plot such as trends and oscillatory piece.
#OLS Reg
co2ols=lm(co2~time(co2))#time is a command to extract the time part of co2 data.
summary(co2ols)
abline(co2ols) #plot fitted line
co2.residuals=resid(co2ols)
hist(co2.residuals,main ='Histogram of Residuals')
qqnorm(co2.residuals) #quantile-quantile, to show normality  of residual. While in this case, we see systematically differences from straight line
qqline(co2.residuals)
plot(co2.residuals~time(co2))
plot(co2.residuals~time(co2),xlim=c(1960,1963),main='Zoomed Residuals')

#######################
########Lec 4##########
#######################
#Hypothesis-t Test
help(sleep)
plot(extra~group,data=sleep,main='Extra Sleep in Gossett Data by Group')
attach(sleep) #不用调用dataframe，直接得到sleep中的变量
extra.1=extra[group==1]
extra.2=extra[group==2]
t.test(extra.1,extra.2,paired = TRUE,alternative = 'two.sided') #双边检测-不等号

#######################
########Lec 5##########
#######################
#Correlation
help(trees)
pairs(trees,pch=21,bg=c('red')) #pair绘图矩阵
cov(trees) #covariance 会因为单位更大而更大，corr消除了这个缺点
cor(trees)

#######################
########Lec 6##########
#######################
#Time plots
install.packages('astsa')
require(astsa)
help(jj)
plot(jj,type='o',main='Johnson&Johnson quarterly earnings per share', ylab='Earnings',xlab='Years')
help(flu)
plot(flu,main='Monthly Pneumonia and Influenza Deaths in US',ylab='Number of Deaths per 10,000 people',xlab='Months')

#######################
########Lec 7##########
#######################
#Stationarity
#time series is the realization of stochastic process
purely_random_process=ts(rnorm(100)) #ts将dataframe变成time series,rnorm means normal random variables
print(purely_random_process)
acf(purely_random_process, type='covariance') #use acf to plot
acf(purely_random_process, main='Correlogram of a purely random process') #蓝色的先代表有意水准
(acf(purely_random_process, main='Correlogram of a purely random process'))

#######################
########Lec 8##########
#######################
#Random Walk
x=NULL
x[1]=0
for(i in 2:1000){x[i]=x[i-1]+rnorm(1)} #rnorm(1)只取一个normal random variable
print(x)
# however x isn't a time series database, so need to generate time series database
random_walk=ts(x)
plot(random_walk, main='A random walk', ylab=' ', xlab='Days', col='blue', lwd=2)
#random walk depends on its white noise and isn't a stationary process. However if take difference of random walk, we'll get purely random process which is stationary.
plot(diff(random_walk)) #The plot looks like a white noise.
acf(diff(random_walk))

#######################
########Lec 9##########
#######################
#MA process
#xt=zt +αzt-1 + βzt-2, the independent variable depends on several past white noise.
#MA(2) process
noise=rnorm(10000)
#Introduce a variable
ma_2=NULL
#Loop for generating MA(2) process
for (i in 3:10000){ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]}
#shift data to left by 2 units since ma starts from period 3
moving_average_process=ma_2[3:10000] 
#put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)
#partition outut graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))
#plot the process and plot is ACF
plot(moving_average_process,main='A MA process of order2',ylab=' ',col='blue')
acf(moving_average_process, main='correlogram of a MA process of order2')

#######################
########Lec 10#########
#######################
#MA2 process
#white noise and MA process are stationary, random walk isn't.
par( mfrow=c(3,1) );
plot( arima.sim(n=150, list(order=c(0,0,0) )  ), main="WN" );
plot( arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33)      )  ) , main="MA3");
plot( arima.sim(n=150, list(ma=c(0.2, 0.2, 0.2, 0.2, 0.2) )  ), main="MA5" );
set.seed=1
(acf(arima.sim(n=1000, model=list(ma=c(0.5,0.5)))))

#######################
########Lec 11#########
#######################
#AR process
#AR(1) process
set.seed(2016)
n=1000;phi=.4
z=rnorm(n,0,1);x=NULL
x[1]=z[1]
for(t in 2:n){x[t]=z[t]+phi*x[t-1]}
x=ts(x)
par(mfrow=c(2,1))
plot(x,main='AR(1) time series on white noise,phi=.4')
acf=acf(x,main='AR(1) time series on white noise,phi=.4')
#use arima routine to run AR(1)
phi1 = .9;
X.ts <- arima.sim(list(ar = c(phi1)), n=1000)
par(mfrow=c(2,1))
plot(X.ts,main=paste("AR(1) Time Series, phi1=",phi1))
X.acf = acf(X.ts, main="Autocorrelation of AR(1) Time Series,phi=0.9")
#AR(2) Process
set.seed(2017)
x<-arima.sim(list(ar=c(.7, .2)),n=1000)
par(mfrow=c(2,1))
plot(x,main=paste('AR(2) time series, phi1=.7, phi2=.2'))
acf=acf(x,main='Autocorrelation of AR(2) time series')
phi1=.5;phi2=-.4
x2<-arima.sim(list(ar=c(phi1,phi2)),n=1000)
par(mfrow=c(2,1))
plot(x2,main=paste('AR(2) time series,phi1=',phi1,'phi2=',phi2)) #paste将文字符连接起来
acf=acf(x2,main='Autocorrelation of AR(2) time series')

#######################
########Lec 12#########
#######################
#PAFC function
#PAFC use to determine orders of AR(ρ) process.
#For MA(q) process,ACF is enough to determine orders.
#1st,AR(2) Process
rm(list=ls(all=TRUE)) #rm()清空所有记忆
par(mfrow=c(3,1))
phi.1=.6;phi.2=.2
data.ts=arima.sim(n=500,list(ar=c(phi.1,phi.2)))
plot(data.ts,main=paste('Auroregressive Process with phi1=',phi.1,'phi2=',phi.2))
acf(data.ts,main='Autocorrelation Function')
acf(data.ts,type='partial',main='Partial Autocorrelation Function') #order 2 is significcant
#2nd,AR(3) process
rm(list=ls(all=TRUE))
phi.1=.9;phi.2=-.6;phi.3=.3
data.ts=arima.sim(n=500,list(ar=c(phi.1,phi.2,phi.3)))
par(mfrow=c(3,1))
plot(data.ts,main=paste('Auroregressive Process with phi1=',phi.1,'phi2=',phi.2,'phi3=',phi.3))
acf(data.ts,main='Autocorrelation Function')
acf(data.ts,type='partial',main='Partial Autocorrelation Function')
#3rd,MA process-take Beveridge as an example
bev=read.csv('beveridge.csv',header = TRUE) #不将header作为第一行
bev.ts=ts(bev[,2],start = 1500) #[:2]从第二列开始取数据，从1500年开始
plot(bev.ts,ylab='price',main='Beveridge Wheat Price Data')
bev.MA=filter(bev.ts,rep(1/31,31),sides=2) #MA process using filter commmand:向上向下抓取各抓取15个数据点
lines(bev.MA,col='red')
par(mfrow=c(3,1))
Y=bev.ts/bev.MA #notice:there are some NA in bev.ts so later we'll deal with this
plot(Y,ylab='scaled price',main='Transformed Beveridge Wheat Price Data')
acf(na.omit(Y),main='Autocorrelation Function of Transformed Beveridge Data') #na.omit to delete some NA value in Y
acf(na.omit(Y),type = 'partial',main='Partial Autocorrelation Function of Transformed Beveridge Data')
#Other evidence to show the order is 2 by using ar(),which obtains the coefficients of AR process
ar(na.omit(Y),order.max = 5) #Shows that coefficients are only 2

#######################
########Lec 13#########
#######################
#PACF of bodyfat partial out reg
install.packages("isdals")
library(isdals);data(bodyfat)
attach(bodyfat)
pairs( cbind( Fat, Triceps, Thigh, Midarm) ) #cbind形成矩阵
cor( cbind( Fat, Triceps, Thigh, Midarm) ) #triceps and thigh has high correlation so we gonna partial it out
Fat.hat = predict(lm(Fat~Thigh))
Triceps.hat = predict( lm(Triceps~Thigh) )
cor( (Fat- Fat.hat), (Triceps- Triceps.hat) )
#what if we control for both Thigh and Midarm? use ppcor library
install.packages("ppcor")
library(ppcor)
pcor(cbind(Fat,Triceps,Thigh)) #get partial out coefficients of the 3 variables
pcor(cbind(Fat,Triceps,Thigh,Midarm))

#######################
########Lec 14#########
#######################
#Yule-Walker Equations
#AR2 Process
n=10000
sigma=4
phi= c(1/3,1/2)
set.seed(2017)
ar.process=arima.sim(n,model=list(ar=c(1/3,1/2)),sd=4)
ar.process[1:5]
r=acf(ar.process,plot=F)$acf[2:3]
R=matrix(1,2,2) #2 by 2 matrix with element 1
R[1,2]=r[1]
R[2,1]=r[1]
R
b=matrix(r,2,1)
b
solve(R,b)
phi.hat=matrix(c(solve(R,b)[1,1],solve(R,b)[2,1]),2,1) #2 by 1 matrix with element  c[]
phi.hat
c0=acf(ar.process,type='covariance',plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
par(mfrow=c(3,1))
plot(ar.process,main='Simulated AR(2)')
acf(ar.process,main='ACF')
pacf(ar.process,main='PACF')

#######################
########Lec 15#########
#######################
#Model fitting
library(astsa)
my.data=rec
plot(rec, main='Recruitment time series', col='blue', lwd=3)
# subtract mean to get a time series with mean zero
ar.process=my.data-mean(my.data)
# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main='Recruitment', col='green', lwd=3)
# order
p=2
# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')
# matrix R
R=matrix(1,p,p) # matrix of dimension 2 by 2, with entries all 1's.
# define non-diagonal entires of R
for(i in 1:p){
  for(j in 1:p){
    if(i!=j)
      R[i,j]=r[abs(i-j)]
  }
}
R
# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b
# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
#variance estimation using Yule-Walker Estimator
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')

#######################
########Lec 16#########
#######################
#AR(4) MODEL-using log transformation
library(astsa)
# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson&Johnosn earnings per share', col='blue', lwd=3)
# log-return of Johnson&Johnson
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)
# Plots for log-returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='PACF')
# Order
p=4
# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r
# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.
# define non-diagonal entires of R
for(i in 1:p){
  for(j in 1:p){
    if(i!=j)
      R[i,j]=r[abs(i-j)]
  }
}
R
# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b
phi.hat=solve(R,b)[,1]
phi.hat
# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')

#######################
########Lec 17#########
#######################
#LakeHuron database
LakeHuron
plot(LakeHuron)
#Remove trend by using diff
data<- diff(LakeHuron)
plot(data)
pacf(data,main='PACF of LakeHuron',lag.max = 30)
r=NULL
r=acf(data, plot=F)$acf[0:3]
r
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
r=NULL
r[1:2]=acf(diff(LakeHuron), plot=F)$acf[2:3]
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R
b=matrix(r,nrow=2,ncol=1)
b
#find the coefficients of the fitted model. 
phi.hat=solve(R,b)
phi.hat
c0=acf(diff(LakeHuron), type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat

#######################
########Lec 18#########
#######################
#AIC criterion
#use arima to simulate AR(3) process
rm(list=ls(all=TRUE))
set.seed(500) #500	Seven kingdoms Kent, Essex, Sussex, Wessex, East Anglia, Mercia, and Northumbria.)
data = arima.sim( list(order = c(3,0,0), ar =c( 0.6, -0.1, .4)), n = 5000)
arima(data, order=c(2,0,0), include.mean=FALSE )
arima(data, order=c(3,0,0), include.mean=FALSE )
arima(data, order=c(4,0,0), include.mean=FALSE )
#AR3 has the smallest AIC so the better model is AR(3)
#Estimate x_t=0.7x_t-1+z_t+0.2z_t-1(AR1+MA1)
rm(list=ls(all=TRUE))
set.seed(500) 
data = arima.sim( list(order = c(1,0,1), ar =.7, ma=.2), n = 1000000) #c(1,0,1)表示AR的1，差分阶数的0，MA的1
par(mfcol = c(3,1 ))
plot(data, main="ARMA(1,1) Time Series: phi1=.7, theta1=.2", xlim=c(0,400)) #first terms
acf(data, main="Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
acf(data, type="partial", main="Partial Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
#ARMA model
discoveries
plot(discoveries,
     main = "Time Series of Number of Major Scientific Discoveries in a Year")
stripchart(discoveries, method = "stack", offset=.5, at=.15,pch=19,
           main="Number of Discoveries Dotplot",
           xlab="Number of Major Scientific Discoveries in a Year",
           ylab="Frequency") #stripchart 用于离散变量
par(mfcol = c(2,1 ))
acf(discoveries, main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries, type="partial", main="PACF of Number of Major Scientific Discoveries
in a Year")
#Both ACF and PACF tails off so you cannot tell the accurate order of your model.
#A possible way is to select several candidate and check AIC for each of them.(16 groups)
p=c(0:3)
for(i in (1:4)){
  for(j in (1:4)){
    b=AIC(arima(discoveries,order=c(p[i],0,p[j])))
    print(b)}}  
#auto.arima helps to calculate automatically
install.packages('forecast')
auto.arima(discoveries, d=0, approximation=FALSE)
auto.arima(discoveries, d=0, ic="bic", approximation=FALSE)
auto.arima(discoveries, d=0, ic="aic", approximation=FALSE)
auto.arima(discoveries, d=0, ic="aic", approximation=TRUE)

#######################
########Lec 19#########
#######################
#ARIMA fitting
#ACF suggests order of MA and PACF suggests order of AR.
#How to select model:AIC,BIC,SSE,Ljung-Box Q-statistics
birth.data<-read.csv('birds.csv')
library(astsa)
# pull out number of births column
number_of_births<-birth.data$Daily.total.female.births.in.California.1959
# use date format for dates 
birth.data$Date<-as.Date(birth.data$Date, "%Y/%m/%d")
# plot the series
plot(number_of_births ~ birth.data$Date, type = "l",
     main='Daily total female births in california, 1959',
     ylab = 'Number of births', xlab='Date')
# Test for correlation,H0：ρ1=ρ2=...=0
Box.test(number_of_births, lag = log(length(number_of_births)))
# plot the differenced series
plot(diff(number_of_births) ~ birth.data$Date[1:364], type = "l",
     main='Differenced series',
     ylab = '', xlab='Date')
Box.test(diff(number_of_births), lag = log(length(diff(number_of_births))))
# acf and pacf of the differenced data
acf(diff(number_of_births), main='ACF of differenced data', 50) #acf suggests lags for MA process=2
pacf(diff(number_of_births), main='PACF of differenced data', 50) #pacf suggests lags for AR process=7
# Fit various ARIMA models
model1<-arima(number_of_births, order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals))) #to test if residual is white noise
model2<-arima(number_of_births, order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))
model3<-arima(number_of_births, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))
model4<-arima(number_of_births, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))
df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')
format(df, scientific=FALSE)
#all pvalue are big so we cannot reject H0:ρ=0——no autocorrelation in residuals
#min AIC shows(0,1,2) is the best, min SSE shows(7,1,2) is the best, however we choose (0,1,2) for the reason of simplicity
sarima(number_of_births, 0,1,2,0,0,0)
#So the fitted model is:(1-B)Xt=0.015+Zt-0.8511Zt-1-0,1113Zt-2
#Another fitted model example
library(datasets)
data <- BJsales
plot.ts(data)
plot(diff(data))
plot(diff(diff(data)))
data.2diff <- diff(diff(data))
pacf(data.2diff)
acf(data.2diff)
d=2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=6){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}
d=2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=8){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}
model<-arima(BJsales, order=c(0,2,1))
par(mfrow=c(2,2))
plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)
model

#######################
########Lec 20#########
#######################
#SARIMA(0,0,1,0,0,1) simulation
x=NULL
z=NULL
n=10000
z=rnorm(n)
for(i in 14:n){
  x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}
x[1:13]=1
par(mfrow=c(3,1))
plot.ts(x,main='Simulated time series SARIMA(0,0,1,0,0,1)_12')
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 
acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')
#acf shows spike at lag11 which doesn't include in the model,why?——calculate r(11)&ρ(11) by yourself

#######################
########Lec 21#########
#######################
#SARIMA fitting using Johnson&Johnson
#Objectives are to fit SARIMA models to quarterly earnings of Johnson & Johnson shares
#General steps:
#1.Look at time plots
#2.Data transformation to stablize the variance(log)
#3.Differencing to remove trend and seasonality and keep stationary and invertible(diff)
#4.Ljung-Box test to check if autocorrelation exists between lags(H0:no autocorr)
#5.ACF to decide MA lags(closer spikes) and seasonal lags(spikes around seasonal lags)
#6.PACF to decide AR lags and SAR lags
#7.Fit different models
#8.Compare AIC and choose model with minimum AIC or min SSE(use parsimony principle in the end)
#9.plot time plot,acf,pacf and Ljung-Box of residuals to test whether residuals are white noise
#10.Forecast
#The parsimony principle:p+d+q+P+D+Q <= 6
library(astsa)
#step1
plot.ts(jj,main='Quarterly Earnings per JJ share($)',col='blue')#observe trend and seasonality
#step2&step3
logjj.diff <- diff(log(jj))
#step4
Box.test(logjj.diff, lag = log(length(logjj.diff))) #pvalue is small so autocorr exists
#step5&6
par(mfrow=c(3,1))
plot(logjj.diff,main='Log-return of JJ earnings per share')
acf(logjj.diff,main='ACF')
pacf(logjj.diff,main='PACF')#observe seasonal lags of lag4 and lag8 so we need remove seasonality
#redo step2&3-remove seasonality with seasonal differencing D=1
slogjj.diff <- diff(diff(log(jj)),4)
#redo step1 to check
plot(slogjj.diff,main='Non-seasonal and seasonal differenced logatithm of earning JJ')
#redo step4
Box.test(slogjj.diff,lag=log(length(slogjj.diff)))#pvalues is small so reject H0
#redo step5&6
par(mfrow=c(2,1))
acf(slogjj.diff,main='ACF of slogdiff') #suggests q=0/1 & Q=0/1
pacf(slogjj.diff,main='PACF of slogdiff') #suggests p=0/1 & P=0/1
#model:SARIMA(p,1,q,P,1,Q)_4 with p&q&P&Q=0/1
#step7&8
d=1
DD=1
per=4
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#We want high pvalue and small AIC&SSE, so we choose (0,1,1,1,1,0,4)
#step9
sarima(log(jj), 0,1,1,1,1,0,4)
#step10
install.packages('forecast')
library(forecast)
model <- arima(x=log(jj),order=c(0,1,1),seasonal = list(order=c(1,1,0),period=4))
plot(forecast(model))
forecast(model)#point estimation under 80% and 90% confidence interval in next 2 years

#######################
########Lec 22#########
#######################
#SARIMA fitting using milk production data
#Data usage:https://pkg.yangzhuoranyang.com/tsdl/articles/tsdl.html
install.packages("devtools")
devtools::install_github("FinYang/tsdl")
library(tsdl)
milk <- subset(tsdl,description='Monthly milk production: pounds per cow. Jan 62 – Dec 75')[1] #extract data from tsdl
milk <- Reduce(rbind, milk) #convert list into matrix
plot(milk)# observe trend and seasonality
plot(milk,xlim=c(1962,1964))#zoom into the first 2 years
acf(milk)#suggests that there is some cyclic behavior so need to do sseasonal differencing
pacf(milk)
milk_diff <- diff(diff(milk,12))
par(mfrow=c(3,1))
plot(milk_diff)
mx=50
acf(milk_diff,lag.max = mx)
axis(1, at=0:mx/12, labels=0:mx)
pacf(milk_diff,lag.max = mx)
axis(1, at=0:mx/12, labels=0:mx)
#A little different from lecture cause different data source
#observe q=1,Q=1,p=1,P=1&2&3 while p=q=0,P=1&2,Q=1&2&3 in lecture(using the latter in the following context)
library(forecast)
d=NULL
DD=NULL
d=1
DD=1
per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#choose 0 1 0 0 1 1 12
sarima(milk,0,1,0,0,1,1,12)
model<- arima(x=milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)

#######################
########Lec 23#########
#######################
#SARIMA fitting using sales data
library(tsdl)
sales <- subset(tsdl,description='souvenir')
sales <- Reduce(rbind, sales)
library(astsa)
library(forecast)
plot(sales)#observe seasonality and trend and var varies so we need to logalize
par(mfrow=c(2,1))
mx=50
acf(sales,lag.max = mx)
axis(1, at=0:mx/12, labels=0:mx)
pacf(sales,lag.max = mx)
axis(1, at=0:mx/12, labels=0:mx)
#observe seasonal lags so need to diff
sales_ldiff <- diff(diff(log(sales)),12)
plot(sales_ldiff)
par(mfrow=c(2,1))
acf(sales_ldiff,50)#lag1代表lag10
pacf(sales_ldiff,50)
#observe q=p=1
d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(sales), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
sarima(log(sales),1,1,0,0,1,1,12)
model<- arima(x=log(sales), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)
#sarima.for routine use to forecast 12 time points ahead  
a<-sarima.for(log(sales),12,1,1,0,0,1,1,12)
plot.ts(c(sales,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)

#######################
########Lec 24#########
#######################
#Simple Exponential Forecasting
#scan command helps to access data from website and get an array
rm(list=ls(all=TRUE))
rain.data <-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain.ts <- ts(rain.data, start=c(1813)) #form time series 
par( mfrow=c(1,2) )
hist(rain.data, main="Annual London Rainfall 1813-1912",
     xlab="rainfall in inches")
qqnorm(rain.data,main="Normal Plot of London Rainfall")
qqline(rain.data)
par( mfrow=c(2,1) )
plot.ts(rain.ts, main="Annual London Rainfall 1813-1912",
        xlab="year", ylab="rainfall in inches")
acf(rain.ts, main="ACF: London Rainfall")
library(forecast)
auto.arima(rain.ts) #check if there is autocorrelations
#SES forecasting
#x_{n+1}^{n}=ax_{n}+(1-a)x_{n}^{n-1}——SES
alpha=.2 #increase alpha for more rapid decay
forecast.values = NULL #establish array to store forecast values
n = length(rain.data)
#naive first forecast
forecast.values [1] = rain.data[1]
#loop to create all forecast values
for( i in 1:n ) {
  forecast.values [i+1] = alpha*rain.data[i] + (1-alpha)* forecast.values [i]
}
paste("forecast for time",n+1," = ", forecast.values [n+1])
#forecast at time: 101 = 25.3094062064236
#How to choose alpha
SSE=NULL
n = length(rain.data)
alpha.values = seq( .001, .999, by=0.001)
number.alphas = length(alpha.values)
for( k in 1:number.alphas ) {
  forecast.values=NULL
  alpha = alpha.values[k]
  forecast.values[1] = rain.data[1]
  for( i in 1:n ) {
    forecast.values[i+1] = alpha*rain.data[i] + (1-alpha)*forecast.values[i]
  }
  SSE[k] = sum( (rain.data - forecast.values[1:n])^2 )
}
plot(SSE~alpha.values, main="Optimal alpha value Minimizes SSE")
plot(SSE~alpha.values, main="Optimal alpha value Minimizes SSE",xlim=c(0.02,0.03),ylim=c(1828,1831))
index.of.smallest.SSE = which.min(SSE) #returns position 24
alpha.values[which.min(SSE)] #returns 0.024
#apply alpha=.024 to forecast
alpha=.024
forecast.values=NULL
n=length(rain.data)
forecast.values[1]=rain.data[1]
for (i in 1:n) {
  forecast.values[i+1]=alpha*rain.data[i]+(1-alpha)*forecast.values[i]
}
paste('forecast for time',n+1,'=',forecast.values[i+1])
#Another way to forecast using Holt-Winters
HoltWinters(rain.ts, beta=FALSE, gamma=FALSE)
#Holt-Winters exponential smoothing without trend and without seasonal component.

#######################
########Lec 25#########
#######################
#Double Exponential, or Exponential Smoothing with Trend
install.packages("imputeTS")
library(imputeTS)
library(tsdl)
library(astsa)
library(forecast)
money.data <- subset(tsdl,description='money')[2]
money.data.ts <- na.seadec(Reduce(rbind,money.data))#add data points to missing values
par(mfrow=c(3,1))
plot(money.data.ts, main="Time Plot of Volume of Money")
acf(money.data.ts, main="ACF of Volume of Money",na.action = na.pass) #check missing values
acf(money.data.ts, type="partial", main="PACF of Volume of Money",na.action = na.pass)
#forecast=level+trend
#set up our transformed data and smoothing parameters
data = money.data.ts
N = length(data)
alpha = 0.7
beta = 0.5
##prepare empty arrays so we can store values
forecast = NULL
level = NULL
trend = NULL
#initialize level and trend in a very simple way
level[1] = data [1]
trend[1] = data[2]- data [1]
#initialize forecast to get started
forecast[1] = data [1]
forecast[2] = data [2]
#loop to build forecasts
for( n in 2:N ) {
  level[n] = alpha* data [n] +
    (1-alpha)*(level[n-1]+trend[n-1])
  trend[n] = beta*(level[n] - level[n-1]) +
    (1-beta)*trend[n-1]
  forecast[n+1] = level[n] + trend[n]
}
#display your calculated forecast values
forecast[3:N]
#verify that we have recovered HoltWinters() output
m = HoltWinters(data, alpha = 0.7, beta = 0.5, gamma = FALSE)
m$fitted[,1]
plot(m, main="Holt Winters Fitting of Money Volume with Bogus Parameters")
m=HoltWinters(data, gamma = FALSE)
plot(m, main="Holt Winters Fitting of Money Volume with Optimal Parameters")

#######################
########Lec 26#########
#######################
#Triple Exponential Smoothing-Exponential Smoothing with Trend and Seasonality
AirPassengers.HW = HoltWinters( log10(AirPassengers) )
AirPassengers.HW
rm(list=ls(all=TRUE))
library("forecast")
AirPassengers.hw <- HoltWinters(log10(AirPassengers))
AirPassengers.forecast <- forecast:::forecast.HoltWinters(AirPassengers.hw)
AirPassengers.forecast
plot(AirPassengers.forecast, xlim=c(1949, 1963))
forecast(HoltWinters(sunspots))
library(forecast)
a<-forecast:::forecast.HoltWinters( HoltWinters(sunspots) )

knitr::stitch('time series learning.R')


