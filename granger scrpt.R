library(tseries)
library(Ts)
library(xts)
library(tseries)
library(urca)
library(vars)
library(fBasics)
library(tidyverse)

install.packages("tsDyn")
library(tsDyn)
#transforming the dataset to a dataframe
pd <- as.data.frame(pdd)
head(pd)

#data cleaning
pd[1,1:13] <- pd[1,1:13]/0.01
head(pd)


view(pd)

pd[2,2] <- 8.41
pd[2,2]


View(pd)


pd[17,5] <- 8.13
pd[17,5]




pd <- pd[,-c(4,11)] #dropping amana dy and ear columns

head(pd)
nrow(pd)
ncol(pd)


pd <- pd[-c(57:60),] #dropping the last four months of 2020
colSums(is.na(pd))


#Testing for stationarity column wise
#1.................Madison
adf.test(pd$maddy)

#madison daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lnmaddy <- log(pd$maddy) #log transform of madison daily yield. 
adf.test(lnmaddy) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dmaddy <- diff(lnmaddy) #first difference of the log transform of madison's daily yield. 
adf.test(dmaddy)

d2maddy <- diff(lnmaddy, difference =2)
#pvalue of dmaddy is less than alpha therefore the series has been made stationary. 


#2..................Old mutual
adf.test(pd$omdy)

#Old mutual daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lomdy <- log(pd$omdy) #log transform of madison daily yield. 
adf.test(lomdy) # p value is still higher than 0.05 hence stationarity has not been achieved. 



ldomdy <- diff(lomdy) #first difference of the log transform. 
adf.test(lomdy)

#pvalue of dmaddy is not less than alpha therefore the series has not been made stationary. Proceeding to second differencing 

d2omdy <- diff(lomdy, difference = 2)
adf.test(d2omdy)


#pvalue is less than alpha therefore the series has been made stationary after the second differencing of the log transform


#3 ...................Britam
adf.test(pd$bridy)

#Britam  daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lbridy <- log(pd$bridy) #log transform of madison daily yield. 
adf.test(lbridy) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dbridy <- diff(lbridy) #first difference of the log transform . 
adf.test(dbridy)

d2bridy <- diff(lbridy, difference = 2) 
#pvalue of dbridy is less than alpha therefore the series has been made stationary. 






#4..............Zimele


adf.test(pd$zimdy)

#Zimele  daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lzimdy <- log(pd$zimdy) #log transform of Zimele daily yield. 
adf.test(lzimdy) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dzimdy <- diff(lzimdy) #first difference of the log transform. 
adf.test(dzimdy)

#pvalue of dzimdy is less than alpha therefore the series has been made stationary. 

d2zimdy <- diff(lzimdy, difference = 2) #first difference of the log transform. 



#5...............ICEA
adf.test(pd$icdy)

#ICEA daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

licdy <- log(pd$icdy) #log transform of madison daily yield. 
adf.test(licdy) # p value is lower than 0.05 hence stationarity has  been achieved by log transformation.  

d2licdy <- diff(licdy , difference = 2)







#6............CIC

adf.test(pd$cidy)

#CIC daily yield is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lcidy <- log(pd$cidy) #log transform of madison daily yield. 
adf.test(lcidy) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dcidy <- diff(lcidy) #first difference of the log transform  
adf.test(dcidy)
d2cidy <- diff(lcidy , difference = 2)  

#pvalue of ldcidy is  less than alpha therefore the series has  been made stationary. 

#7..........Madison effective annual rate 

adf.test(pd$madear)

#Madision effective annual rate  is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lmadear <- log(pd$madear) #log transform of madison daily yield. 
adf.test(lmadear) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dmadear <- diff(lmadear) #first difference of the log transform . 
adf.test(dmadear)

#pvalue of lmadear is less than alpha therefore the series has  been made stationary. 
d2madear <- diff(lmadear , difference = 2) #first difference of the log transform . 


#8...............Old mutual effective annual rate

adf.test(pd$omear)

#Old mutual effective annual rate  is not stationary as the p value is higher than 0.05. 
#generating a log version of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lomear <- log(pd$omear) #log transform . 
adf.test(lomear) # p value is still higher than 0.05 hence stationarity has not been achieved. 



domear <- diff(lomear) #first difference of the log transform . 
adf.test(domear)

d2omear <- diff(lomear , difference =2) #first difference of the log transform . 

#p value is less than aloha therefore stationarity has been achieved.

#9.............Britam effective annual rate. 

adf.test(pd$britear)

#Britam effective annual rate is not stationary as the p value is higher than 0.05. 
#generating a log transform of the series with several differencings will be performed iteratively until stationarity is achieved i.e p value is less than 0.05.

lbritear <- log(pd$britear) #log transform of madison daily yield. 
adf.test(lbritear) # p value is still higher than 0.05 hence stationarity has not been achieved. 



dbritear <- diff(lbritear) #first difference of the log transform. 
adf.test(dbritear)
d2britear <- diff(lbritear , difference =2) #first difference of the log transform. 

#pvalue is less than alpha therefore the series has  been made stationary


#10.........Zimele effective annual rate
adf.test(pd$zimear) #P-value greater than 0.5, not stationary. 


#log transform
lzimear <- log(pd$zimear) ##P-value greater than 0.5, not stationary.

#first difference

dzimear <- diff(lzimear)

adf.test(dzimear)#P-value greater than 0.5, not stationary.

#second difference

d2zimear <- diff(lzimear, difference = 2)

adf.test(d2zimear) #P-value less than 0.5, series is stationary










#11............ICEA effective annual rate 

adf.test(pd$icear) #P-value is less  than 0.5, series is stationary. 


icear <- pd$icear

adf.test(icear)

d2icear <- diff(log(icear),difference =2)
#12.............CIC effective annual rate 

adf.test(pd$cicear) #P-value greater than 0.5, not stationary. 


#log transform
lcicear <- log(pd$cicear) ##P-value greater than 0.5, not stationary.

#first difference

dcicear <- diff(lcicear)

adf.test(dcicear)#P-value is less than 0.5, series has been made stationary.


d2cicear <- diff(lcicear , difference =2)

#13.......inflation


adf.test(pd$inflation) #P-value greater than 0.5, not stationary. 


#log transform
linf <- log(pd$inflation) 
#P-value greater than 0.5, not stationary.
adf.test(linf)

#first difference

dinf <- diff(linf)

adf.test(dinf)#P-value greater than 0.5, series has not been made stationary.

#second difference

d2inf <- diff(linf, difference = 2)

adf.test(d2inf) ##P-value less than 0.5, series is stationary


#14.......cbrrates
adf.test(pd$cbrrates) #P-value greater than 0.5, not stationary. 


#log transform
lrates <- log(pd$cbrrates) 
adf.test(lrates)#P-value greater than 0.5, not stationary.


#first difference

drates <- diff(lrates)

adf.test(drates)#P-value greater than 0.5, series has not been made stationary.

#second difference

d2rates <- diff(lrates, difference = 2)

adf.test(d2rates) ##P-value less than 0.5, series is stationary


#15..........dollar
adf.test(pd$dollar)
#not stationary

ldol <- log(pd$dollar)
adf.test(ldol) #not stationary

#second differencing

d2dol <- diff(ldol, difference = 2)
adf.test(d2dol)#statiionary


#16........pound

adf.test(pd$pound)
#not stationary

lpound <- log(pd$pound)
adf.test(lpound) #not stationary

#second differencing

d2pound <- diff(lpound, difference = 2)
adf.test(d2pound)#stationary




?data.frame


#combining the series of second differences into a tibble
pdstat <- tibble(d2maddy,d2omdy,d2bridy,d2zimdy,d2licdy,d2cidy, d2madear,d2omear,d2britear,d2zimear,d2icear,d2cicear,d2rates,d2inf,d2dol,d2pound)
head(pdstat)





#transforming the tibble into a time series object
pds <- as.ts(pdstat , freq = 12, start = c(2016,1), end = c(2020,8))





#Analyzing Cointegrating relationships between various mmf funds and macroeconomic variables. 

#1........MADISON

######Madison daily yield

#####combining madison daily yield with the independent variables into a dataframe
maddyd<- cbind(pdstat$d2maddy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling madison daily yield,cbr rates, inflation and the dollar rate
maddyp<- cbind(pdstat$d2maddy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling madison daily yield,cbr rates, inflation and the sterling poind rate

head(maddyd)
colnames(maddyd) <- c("madison_daily_yield","Inflation","cbr_rates","dollar")#adding column names
colnames(maddyp) <- c("madison_daily_yield","Inflation","cbr rates","Sterling_Pound")#adding column names

maddydt<- as.ts(maddyd , freq = 12, start = c(2016,1), end = c(2020,6))
maddypt<- as.ts(maddyp , freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(maddydt,
        title = "Madison daily yield",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(maddypt,
        title = "Madison daily yield",
        Xtitle = "month",
        Ytitle = ""
)








#lag selection criteria
#selecting lags
maddydlag <- VARselect(maddyd , lag.max = 10,type = "const")#lag selection of Madison with the dollar rate
maddyplag <- VARselect(maddyp , lag.max = 10,type = "const")#lag selection of Madison with the pound rate


maddydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.
maddyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.

#9 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 9-1=8 lags.We wil use nine to check for the cointegrating relationships in the next step.
maddylag$criteria


#johansen test for madison daily yield with the pound
maddypcjt <- ca.jo(maddyp, type = "trace",ecdet = "const",K =8)#trace test 
maddypcje <- ca.jo(maddyp, type = "eigen",ecdet = "const",K =8)#eigen value test 

summary(maddypcjt)
#There are at least 1 cointegerating relationships between Madison's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for madison daily yield with the dollar
maddydcjt <- ca.jo(maddyd, type = "trace",ecdet = "const",K =8)#trace test 
maddydcje <- ca.jo(maddyd, type = "eigen",ecdet = "const",K =8)#eigen value test 
summary(maddydcjt)
summary(maddydcje)

#there are at least 1  co integrating relationships between Madison's daily yield, inflation, the dollar rate and the current central bank rate. 

madvecm <- VECM(maddyd, 8, r = 1, estim =("2OLS"))
summary(madvecm)























######Madison effective annual rate yield

#####combining madison daily yield with the independent variables into a data frame
madeard<- cbind(pdstat$d2madear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling Madison effective annual rate,cbr rates, inflation and the dollar rate
madearp<- cbind(pdstat$d2madear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling Madison daily yield,cbr rates, inflation and the sterling poind rate

madeardt<- as.ts(madeard , freq = 12, start = c(2016,1), end = c(2020,6))
madearpt<- as.ts(madearp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(madeardt,
        title = "Madison effective annual rate",
        Xtitle = "month",
        Ytitle = "With dollar"
)
ts_plot(madearpt,
        title = "Madison effective annual rate",
        Xtitle = "month",
        Ytitle = "With Sterling Pound"
)



colnames(madeard) <- c("Madison effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(madeard)



colnames(madearp) <- c("Madison effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(madearp)










#lag selection criteria
#selecting lags
madeardlag <- VARselect(madeard , lag.max = 10,type = "const")#lag selection of Madison with the dollar rate
madearplag <- VARselect(madearp , lag.max = 10,type = "const")#lag selection of Madison with the pound rate


madeardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.
madearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.


#10 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 10-1=9 lags.We wil use nine to check for the cointegrating relationships in the next step.
madeardlag$criteria#checking the lags criteria
madearplag$criteria#checking the lag criteria


#####johansen test for madison effective annual rate with the pound
madearpcjt <- ca.jo(madearp, type = "trace",ecdet = "const",K =9)#trace test 
madearpcje <- ca.jo(madearp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(madearpcjt)
######There are at least 2 cointegerating relationships between Madison's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for madison effective annual rate with the dollar
madeardcjt <- ca.jo(madeard, type = "trace",ecdet = "const",K =9)#trace test 
madeardcje <- ca.jo(madeard, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(madeardcjt)
summary(madeardcje)

#there are atleast 2 co integrating relationships between Madison's effective annual rate, inflation, the dollar rate and the current central bank rate. 


#ploting the VECM model
madearpvecm <- VECM(madearp, 9, r = 2, estim =("ML"))
madearpvecm <- VECM(madearp, 9, r = 2, estim =("ML"))


madeardvecm <- VECM(madeard, 8, r = 2, estim =("ML"))

summary(madearvecm)













#2..........OLD MUTUAL 
######old mutual  daily yield

#####combining old mutual daily yield with the independent variables into a dataframe
omdyd<- cbind(pdstat$d2omdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling old mutual daily yield,cbr rates, inflation and the dollar rate
omdyp<- cbind(pdstat$d2omdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling old mutual daily yield,cbr rates, inflation and the sterling poind rate

head(omdyd)
head(omdyp)
colnames(omdyd) <-  c("old mutual daily yield","inflation","cbr rates","Dollar")#adding column names
colnames(omdyp) <-  c("old mutual daily yield","inflation","cbr rates","Sterling pound")#adding column names

omdydt<- as.ts(omdyd , freq = 12, start = c(2016,1), end = c(2020,6))
omdypt<- as.ts(omdyp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(omdydt,
        title = "Old mutual daily yield with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(omdypt,
        title = "Old mutual daily yield with pound",
        Xtitle = "month",
        Ytitle = "With Sterling Pound"
)









#lag selection criteria
#selecting lags
omdydlag <- VARselect(omdyd , lag.max = 10,type = "const")#lag selection of old mutual with the dollar rate
omdyplag <- VARselect(omdyp , lag.max = 10,type = "const")#lag selection of old mutual with the pound rate


omdydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
omdyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10  is the ideal lag

#10 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 10-1=9 lags.We wil use nine to check for the cointegrating relationships in the next step.
omdydlag$criteria


#johansen test for old mutual daily yield with the pound
omdypcjt <- ca.jo(omdyp, type = "trace",ecdet = "const",K =9)#trace test 
omdypcje <- ca.jo(omdyp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(omdypcjt)
#There are at least 3 cointegerating relationships between old mutual's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for old mutual daily yield with the dollar
omdydcjt <- ca.jo(omdyd, type = "trace",ecdet = "const",K =9)#trace test 
omdydcje <- ca.jo(omdyd, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(omdydcjt)
summary(omdydcje)

#there is no cointegrating relationship between old mutual daily yield and the independent variables. A VAR model will be used for estimating granger causality
























######old mutual effective annual rate yield

#####combining old mutual daily yield with the independent variables into a data frame
omeard<- cbind(pdstat$d2omear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling old mutual effective annual rate,cbr rates, inflation and the dollar rate
omearp<- cbind(pdstat$d2omear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling old mutual daily yield,cbr rates, inflation and the sterling poind rate




colnames(omeard) <- c("old mutual effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(omeard)



colnames(omearp) <- c("old mutual effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(omearp)

omeardt<- as.ts(omeard , freq = 12, start = c(2016,1), end = c(2020,6))
omearpt<- as.ts(omearp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(omeardt,
        title = "Old mutual effective annual rate with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(omearpt,
        title = "Old mutual effective annual rate with pound",
        Xtitle = "month",
        Ytitle = "With Sterling Pound"
)









#lag selection criteria
#selecting lags
omeardlag <- VARselect(omeard , lag.max = 10,type = "const")#lag selection of old mutual with the dollar rate
omearplag <- VARselect(omearp , lag.max = 10,type = "const")#lag selection of old mutual with the pound rate


omeardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
omearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9  is  the ideal lag


#9 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 9-1=8 lags.We will use 8 to check for the cointegrating relationships in the next step.
omeardlag$criteria#checking the lags criteria
omearplag$criteria#checking the lag criteria


#####johansen test for old mutual effective annual rate with the pound
omearpcjt <- ca.jo(omearp, type = "trace",ecdet = "const",K =9)#trace test 
omearpcje <- ca.jo(omearp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(omearpcjt)
######There are at least 2 cointegerating relationships between old mutual's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for old mutual effective annual rate with the dollar
omeardcjt <- ca.jo(omeard, type = "trace",ecdet = "const",K =9)#trace test 
omeardcje <- ca.jo(omeard, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(omeardcjt)
summary(omeardcje)

#there are atleast 2 co integrating relationships between Britam's effective annual rate, inflation, the dollar rate and the current central bank rate. 




#3..........Britam 
######Britam  daily yield

#####combining Britam daily yield with the independent variables into a dataframe
bridyd<- cbind(pdstat$d2bridy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling Britam daily yield,cbr rates, inflation and the dollar rate
bridyp<- cbind(pdstat$d2bridy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling Britam daily yield,cbr rates, inflation and the sterling poind rate



colnames(bridyd) <-  c("Britam daily yield","inflation","cbr rates","Dollar")#adding column names
head(bridyd)
colnames(bridyp) <-  c("Britam daily yield","inflation","cbr rates","Sterling pound")#adding column names
head(bridyp)



bridydt<- as.ts(bridyd , freq = 12, start = c(2016,1), end = c(2020,6))
bridypt<- as.ts(bridyp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(bridydt,
        title = "Britam daily yield with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(bridypt,
        title = "Britam daily yield with  pound",
        Xtitle = "month",
        Ytitle = ""
)







#lag selection criteria
#selecting lags
bridydlag <- VARselect(bridyd , lag.max = 10,type = "const")#lag selection of Britam with the dollar rate
bridyplag <- VARselect(bridyp , lag.max = 10,type = "const")#lag selection of Britam with the pound rate


bridydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
bridyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag

#9 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 9-1=8 lags.We wil use 8 to check for the cointegrating relationships in the next step.
bridydlag$criteria


#johansen test for Britam daily yield with the pound
bridypcjt <- ca.jo(bridyp, type = "trace",ecdet = "const",K =9)#trace test 
bridypcje <- ca.jo(bridyp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(bridypcjt)
#There are at least 2 cointegerating relationships between Britam's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for Britam daily yield with the dollar
bridydcjt <- ca.jo(bridyd, type = "trace",ecdet = "const",K =9)#trace test 
bridydcje <- ca.jo(bridyd, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(bridydcjt)
summary(bridydcje)

#there are at least 2 cointegrating relationship between Britam daily yield and the independent variables. 
























######Britam effective annual rate yield

#####combining Britam daily yield with the independent variables into a data frame
briteard<- cbind(pdstat$d2britear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling Britam effective annual rate,cbr rates, inflation and the dollar rate
britearp<- cbind(pdstat$d2britear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling Britam daily yield,cbr rates, inflation and the sterling poind rate




colnames(briteard) <- c("Britam effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(briteard)



colnames(britearp) <- c("Britam effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(britearp)





briteardt<- as.ts(briteard , freq = 12, start = c(2016,1), end = c(2020,6))
britearpt<- as.ts(britearp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(briteardt,
        title = "Britam effective annual rate with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(britearpt,
        title = "Britam effective annual rate with pound",
        Xtitle = "month",
        Ytitle = ""
)






#lag selection criteria
#selecting lags
briteardlag <- VARselect(briteard , lag.max = 10,type = "const")#lag selection of Britam with the dollar rate
britearplag <- VARselect(britearp , lag.max = 10,type = "const")#lag selection of Britam with the pound rate


briteardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10 is the ideal lag
britearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9  is  the ideal lag


#i#if p is the selected lag, p-1 is used as k in the next step of teh cointegration test.
briteardlag$criteria#checking the lags criteria
britearplag$criteria#checking the lag criteria






#####johansen test for Britam effective annual rate with the pound
britearpcjt <- ca.jo(britearp, type = "trace",ecdet = "const",K =9)#trace test 
britearpcje <- ca.jo(britearp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(britearpcjt)
######There are at least 3 cointegerating relationships between Britam's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for Britam effective annual rate with the dollar
briteardcjt <- ca.jo(briteard, type = "trace",ecdet = "const",K =8)#trace test 
briteardcje <- ca.jo(briteard, type = "eigen",ecdet = "const",K =8)#eigen value test 
summary(briteardcjt)
summary(briteardcje)

#there exists no co-integrating relationships between Britam's effective annual rate, inflation, the dollar rate and the current central bank rate. A VAR model will be used to estimate granger causality 















#3..........ZIMELE 
######ZIMELE   daily yield

#####combining Britam daily yield with the independent variables into a dataframe
zimdyd<- cbind(pdstat$d2zimdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling Zimele daily yield,cbr rates, inflation and the dollar rate
zimdyp<- cbind(pdstat$d2zimdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling Zimele daily yield,cbr rates, inflation and the sterling poind rate



colnames(zimdyd) <-  c("Zimele daily yield","inflation","cbr rates","Dollar")#adding column names
head(zimdyd)
colnames(zimdyp) <-  c("Zimele daily yield","inflation","cbr rates","Sterling pound")#adding column names
head(zimdyp)



zimdydt<- as.ts(zimdyd , freq = 12, start = c(2016,1), end = c(2020,6))
zimdypt<- as.ts(zimdyp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(zimdydt,
        title = "Zimele daily yield with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(zimdypt,
        title = "Zimele daily yield with  pound",
        Xtitle = "month",
        Ytitle = ""
)







#lag selection criteria
#selecting lags
zimdydlag <- VARselect(zimdyd , lag.max = 10,type = "const")#lag selection of Zimele with the dollar rate
zimdyplag <- VARselect(zimdyp , lag.max = 10,type = "const")#lag selection of Zimele with the pound rate


zimdydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10 is the ideal lag
zimdyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag. 10is the ideal lag

#10 is the dominant lag selected . Since we are estimating co integration with VECM , we subtract one so we have 9-1=8 lags.We wil use 8 to check for the cointegrating relationships in the next step.
zimdydlag$criteria


#johansen test for Zimele daily yield with the pound
zimdypcjt <- ca.jo(zimdyp, type = "trace",ecdet = "const",K =9)#trace test 
zimdypcje <- ca.jo(zimdyp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(zimdypcjt)
#There are more than 3 cointegerating relationships between Zimele's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for Zimele daily yield with the dollar
zimdydcjt <- ca.jo(zimdyd, type = "trace",ecdet = "const",K =9)#trace test 
zimdydcje <- ca.jo(zimdyd, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(zimdydcjt)
summary(zimdydcje)

#there are more than 3 cointegrating relationships between Zimele daily yield and the independent variables. 
























######Zimele effective annual rate yield

#####combining Zimele daily yield with the independent variables into a data frame
zimeard<- cbind(pdstat$d2zimear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling Zimele effective annual rate,cbr rates, inflation and the dollar rate
zimearp<- cbind(pdstat$d2zimear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling Zimele daily yield,cbr rates, inflation and the sterling poind rate


zimeardt<- as.ts(zimeard , freq = 12, start = c(2016,1), end = c(2020,6))
zimearpt<- as.ts(zimearp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(zimeardt,
        title = "Zimele effective annual rate with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(zimearpt,
        title = "Zimele effective annual rate with  pound",
        Xtitle = "month",
        Ytitle = ""
)



colnames(zimeard) <- c("Zimele effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(zimeard)



colnames(zimearp) <- c("Zimele effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(zimearp)










#lag selection criteria
#selecting lags
zimeardlag <- VARselect(zimeard , lag.max = 10,type = "const")#lag selection of Zimele with the dollar rate
zimearplag <- VARselect(zimearp , lag.max = 10,type = "const")#lag selection of Zimele with the pound rate


zimeardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10 is the ideal lag
zimearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9  is  the ideal lag


#i#if p is the selected lag, p-1 is used as k in the next step of teh cointegration test.
zimeardlag$criteria#checking the lags criteria. P=10
zimearplag$criteria#checking the lag criteria p=9






#####johansen test for Zimele effective annual rate with the pound
zimearpcjt <- ca.jo(zimearp, type = "trace",ecdet = "const",K =9)#trace test 
zimearpcje <- ca.jo(zimearp, type = "eigen",ecdet = "const",K =8)#eigen value test 

summary(zimearpcjt)
######There are more than 3 cointegerating relationships between Zimele's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for Zimele effective annual rate with the dollar
zimeardcjt <- ca.jo(zimeard, type = "trace",ecdet = "const",K =8)#trace test 
zimeardcje <- ca.jo(zimeard, type = "eigen",ecdet = "const",K =8)#eigen value test 
summary(zimeardcjt)
summary(zimeardcje)

#there are exactly 3 cointegrating relationships between Zimele's effective annual rate, inflation, the dollar rate and the current central bank rate. 

















#5..........ICEA 
######ICEA   daily yield

#####combining ICEA  daily yield with the independent variables into a dataframe
licdyd<- cbind(pdstat$d2licdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling ICEA daily yield,cbr rates, inflation and the dollar rate
licdyp<- cbind(pdstat$d2licdy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling ICEA daily yield,cbr rates, inflation and the sterling poind rate



colnames(licdyd) <-  c("ICEA daily yield","inflation","cbr rates","Dollar")#adding column names
head(licdyd)
colnames(licdyp) <-  c("ICEA daily yield","inflation","cbr rates","Sterling pound")#adding column names
head(licdyp)


licdydt<- as.ts(licdyd , freq = 12, start = c(2016,1), end = c(2020,6))
licdypt<- as.ts(licdyp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(licdydt,
        title = "ICEA daily yield with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(licdypt,
        title = "ICEA daily yield with  pound",
        Xtitle = "month",
        Ytitle = ""
)






#lag selection criteria
#selecting lags
licdydlag <- VARselect(licdyd , lag.max = 10,type = "const")#lag selection of ICEA with the dollar rate
licdyplag <- VARselect(licdyp , lag.max = 10,type = "const")#lag selection of ICEA with the pound rate


licdydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
licdyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag. 10 is the ideal lag

#if p is the selected lag then k =p-1 for the cointegration test


#johansen test for ICEA daily yield with the pound
licdypcjt <- ca.jo(licdyp, type = "trace",ecdet = "const",K =9)#trace test 
licdypcje <- ca.jo(licdyp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(licdypcjt)
#There are more than 3 cointegerating relationships between ICEA's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for ICEA daily yield with the dollar
licdydcjt <- ca.jo(licdyd, type = "trace",ecdet = "const",K =9)#trace test 
licdydcje <- ca.jo(licdyd, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(licdydcjt)
summary(licdydcje)

#there are more than 3 cointegrating relationships between ICEA daily yield and the independent variables. 
























######ICEA effective annual rate yield

#####combining ICEA daily yield with the independent variables into a data frame
iceard<- cbind(pdstat$d2icear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling ICEA effective annual rate,cbr rates, inflation and the dollar rate
icearp<- cbind(pdstat$d2icear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling ICEA daily yield,cbr rates, inflation and the sterling poind rate


licdydt<- as.ts(licdyd , freq = 12, start = c(2016,1), end = c(2020,6))
licdypt<- as.ts(licdyp, freq = 12, start = c(2016,1), end = c(2020,6))

ts_plot(licdydt,
        title = "ICEA daily yield with dollar",
        Xtitle = "month",
        Ytitle = ""
)
ts_plot(licdypt,
        title = "ICEA daily yield with  pound",
        Xtitle = "month",
        Ytitle = ""
)

colnames(iceard) <- c("ICEA effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(iceard)



colnames(icearp) <- c("ICEA effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(icearp)










#lag selection criteria
#selecting lags
iceardlag <- VARselect(iceard , lag.max = 10,type = "const")#lag selection of ICEA with the dollar rate
icearplag <- VARselect(icearp , lag.max = 10,type = "const")#lag selection of ICEA with the pound rate


iceardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
icearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10  is  the ideal lag


#i#if p is the selected lag, p-1 is used as k in the next step of teh cointegration test.
iceardlag$criteria#checking the lags criteria. P=9
icearplag$criteria#checking the lag criteria p=10






#####johansen test for ICEA effective annual rate with the pound
icearpcjt <- ca.jo(icearp, type = "trace",ecdet = "const",K =8)#trace test 
icearpcje <- ca.jo(icearp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(icearpcjt)
######There are more than 3 cointegrating relationships between ICEA's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for ICEA effective annual rate with the dollar
iceardcjt <- ca.jo(iceard, type = "trace",ecdet = "const",K =8)#trace test 
iceardcje <- ca.jo(iceard, type = "eigen",ecdet = "const",K =8)#eigen value test 
summary(iceardcjt)
summary(iceardcje)

#there are no cointegrating relationships between ICEA's effective annual rate, inflation, the dollar rate and the current central bank rate. A VAR model will be used to estimate granger causality 























#6..........CIC 
######cic   daily yield

#####combining cic  daily yield with the independent variables into a dataframe
cidyd<- cbind(pdstat$d2cidy,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling cic daily yield,cbr rates, inflation and the dollar rate
cidyp<- cbind(pdstat$d2cidy,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling cic daily yield,cbr rates, inflation and the sterling poind rate



colnames(cidyd) <-  c("cic daily yield","inflation","cbr rates","Dollar")#adding column names
head(cidyd)
colnames(cidyp) <-  c("cic daily yield","inflation","cbr rates","Sterling pound")#adding column names
head(cidyp)









#lag selection criteria
#selecting lags
cidydlag <- VARselect(cidyd , lag.max = 10,type = "const")#lag selection of cic with the dollar rate
cidyplag <- VARselect(cidyp , lag.max = 10,type = "const")#lag selection of cic with the pound rate


cidydlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10 is the ideal lag
cidyplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag. 10 is the ideal lag

#if p is the selected lag then k =p-1 for the cointegration test


#johansen test for cic daily yield with the pound
cidypcjt <- ca.jo(cidyp, type = "trace",ecdet = "const",K =9)#trace test 
cidypcje <- ca.jo(cidyp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(licdypcjt)
#There are more than 3 cointegerating relationships between cic's daily yield, inflation, the pound rate and the current central bank rate. 


#Johansen test for cic daily yield with the dollar
cidydcjt <- ca.jo(cidyd, type = "trace",ecdet = "const",K =9)#trace test 
cidydcje <- ca.jo(cidyd, type = "eigen",ecdet = "const",K =9)#eigen value test 
summary(cidydcjt)
summary(cidydcje)

#there are more than 3 cointegrating relationships between cic daily yield and the independent variables. 
























######cic effective annual rate yield

#####combining cic daily yield with the independent variables into a data frame
ciceard<- cbind(pdstat$d2cicear,pdstat$d2inf,pdstat$d2rates,pdstat$d2dol)#modelling cic effective annual rate,cbr rates, inflation and the dollar rate
cicearp<- cbind(pdstat$d2cicear,pdstat$d2inf,pdstat$d2rates,pdstat$d2pound)#modelling cic daily yield,cbr rates, inflation and the sterling poind rate




colnames(ciceard) <- c("cic effective annual rate","inflation","cbr rates","Dollar")#adding column names
head(ciceard)



colnames(cicearp) <- c("cic effective annual rate","inflation","cbr rates","Sterling pound")#adding column names
head(cicearp)










#lag selection criteria
#selecting lags
ciceardlag <- VARselect(ciceard , lag.max = 10,type = "const")#lag selection of cic with the dollar rate
cicearplag <- VARselect(cicearp , lag.max = 10,type = "const")#lag selection of cic with the pound rate


ciceardlag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.9 is the ideal lag
cicearplag$selection#checking the ideal lag ,NB:  the most recurrent is the ideal lag.10  is  the ideal lag


#i#if p is the selected lag, p-1 is used as k in the next step of teh cointegration test.
ciceardlag$criteria#checking the lags criteria. P=9
cicearplag$criteria#checking the lag criteria p=10






#####johansen test for cic effective annual rate with the pound
cicearpcjt <- ca.jo(cicearp, type = "trace",ecdet = "const",K =8)#trace test 
cicearpcje <- ca.jo(cicearp, type = "eigen",ecdet = "const",K =9)#eigen value test 

summary(cicearpcjt)
######There are more than 3 cointegrating relationships between cic's effective annual rate, inflation, the pound rate and the current central bank rate. 


#####Johansen test for cic effective annual rate with the dollar
ciceardcjt <- ca.jo(ciceard, type = "trace",ecdet = "const",K =8)#trace test 
ciceardcje <- ca.jo(ciceard, type = "eigen",ecdet = "const",K =8)#eigen value test 
summary(ciceardcjt)
summary(ciceardcje)

#there are more than 3 cointegrating relationships between cic's effective annual rate, inflation, the dollar rate and the current central bank rate. A VAR model will be used to estimate granger causality 



















library(TSstudio)
data.frame(dmaddy,d2omdy,dbridy)
nrow(dmaddy)
n(dmaddy)
ts_plot(pds,
        title = "MMF yield",
        Xtitle = "month",
        Ytitle = "yield"
)

dset <- cbind( pds[""])











str(d2omdy)










View(pdstat)
as.vector <- (dmaddy)
class(dmaddy)

str(dmaddy)

View(pd)










view(pd_ts)

str(pd_ts)


acf(pd_ts)
par(mfrow=c(2,2))


par(mar=c(1,1,1,1))



graphics.off()

head(pd_ts,1)


pacf(pd_ts$omdy)
pacf(pd$maddy)

acf(pd$maddy)

pdt

#iteratively testing for ideal model by checking the lowest aic
arima(pd$maddy , c(2,0,0))
arima(pd$maddy , c(2,1,0))
arima(pd$maddy , c(1,2,1))


pdt<- as.ts(pd , freq = 12, start = c(2016,1), end = c(2020,8))
ts.plot(pdt["maddy"])
str(pdt)
plo
plot.xts(pdt["maddy"],
         main = "Madison yield",
         multi.panel = FALSE,
         col = c("black", "blue")
         )

madvar <- VAR(pd$m, type = "const", lag.max = 4 , ic="AIC")
install.packages("TSstudio")






library(TSstudio)
ts_plot(pdt,
        title = "MMF yield",
        Xtitle = "month",
        Ytitle = "yield"
        )




pdt
pds$d2inf



lagselect <- VARselect(pdt , lag.max = 7,type = "const")
?VARselect
 
dset <- cbind(pds["d2inf"],pds["d2rates"],pds["d2maddy"])

pddy <- select(pd , )