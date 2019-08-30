library(forecast)
library(seasonal)

Input_Pax_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Real.csv"
Input_Rev_USD_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Real.csv"
Input_Rev_COP_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Real.csv"

Input_Pax_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Outliers.csv"
Input_Rev_USD_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Outliers.csv"
Input_Rev_COP_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Outliers.csv"

Pax_Real <- read.table(Input_Pax_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Real <- read.table(Input_Rev_USD_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_COP_Real <- read.table(Input_Rev_COP_Real, sep=" ", header=TRUE, fileEncoding="latin1") 

Pax_Outliers <- read.table(Input_Pax_Outliers, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Outliers<- read.table(Input_Rev_USD_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")
Rev_COP_Outliers<- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")

legs <- ncol(Pax_Real)
cantDatos <- nrow(Pax_Real)
forecast_periods <- 3
iterations <- 4

matriz_Indicadores_Pax_Real <- data.frame()
matriz_Indicadores_Rev_USD_Real <- data.frame()
matriz_Indicadores_Rev_COP_Real <- data.frame()

matriz_Indicadores_Pax_Outlier <- data.frame()
matriz_Indicadores_Rev_USD_Outlier <- data.frame()
matriz_Indicadores_Rev_COP_Outlier <- data.frame()

X13_forecast_Error_Pax_Real <- data.frame()
X13_forecast_Error_USD_Real <- data.frame()
X13_forecast_Error_COP_Real <- data.frame()

X13_forecast_Error_Pax_Outlier <- data.frame()
X13_forecast_Error_Rev_USD_Outlier <- data.frame()
X13_forecast_Error_Rev_COP_Outlier <- data.frame()

for(g in 1:legs) {
a <- as.integer(format(as.Date(rownames(Pax_Real)[1]),format = "%Y"))
b <- as.integer(format(as.Date(rownames(Pax_Real)[1]),format = "%m"))
c <- as.integer(format(as.Date(rownames(Pax_Real)[cantDatos]),format = "%Y"))
d <- as.integer(format(as.Date(rownames(Pax_Real)[cantDatos]),format = "%m"))

ts_Pax_Real <- ts(Pax_Real[g] , frequency = 12, start = c(a,b), end= c(c,d))
ts_Rev_USD_Real <- ts(Rev_USD_Real[g] , frequency = 12, start = c(a,b), end= c(c,d))
ts_Rev_COP_Real <- ts(Rev_COP_Real[g] , frequency = 12, start = c(a,b), end= c(c,d))

ts_Pax_Outlier <- ts(Pax_Outliers[g] , frequency = 12, start = c(a,b), end= c(c,d))
ts_Rev_USD_Outlier <- ts(Rev_USD_Outliers[g] , frequency = 12, start = c(a,b), end= c(c,d))
ts_Rev_COP_Outlier <- ts(Rev_COP_Outliers[g] , frequency = 12, start = c(a,b), end= c(c,d))

x <- ts_Pax_Real
h <- forecast_periods
j <- iterations
i <- integer()
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

	
   train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1] 
   test <- window(x,start=test.start)
   train <- as.ts(window(x,end=train.end))
      fit <- as.ts(series(seas(train,outlier = NULL), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
  mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Pax_Real <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

x <- ts_Pax_Outlier
h <- forecast_periods
j <- iterations
i <- integer
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

  train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   test <- window(x,start=test.start)
   train <- as.ts(window(x, end=train.end))
      fit <- as.ts(series(seas(train), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
    mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Pax_Outlier <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

x <- ts_Rev_USD_Real
h <- forecast_periods
j <- iterations
i <- integer
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

   train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   test <- window(x,start=test.start)
   train <- as.ts(window(x, end=train.end))
      fit <- as.ts(series(seas(train, outlier = NULL), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
    mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Rev_USD_Real <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

x <- ts_Rev_USD_Outlier
h <- forecast_periods
j <- iterations
i <- integer
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

  train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   test <- window(x,start=test.start)
   train <- as.ts(window(x, end=train.end))
      fit <- as.ts(series(seas(train), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
    mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Rev_USD_Outlier <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

x <- ts_Rev_COP_Real
h <- forecast_periods
j <- iterations
i <- integer
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

  train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   test <- window(x,start=test.start)
   train <- as.ts(window(x, end=train.end))
      fit <- as.ts(series(seas(train,outlier = NULL), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j+ i -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j+ i -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
    mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Rev_COP_Real <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

x <- ts_Rev_COP_Outlier
h <- forecast_periods
j <- iterations
i <- integer
 mape <- double()
me <- double()
rmse <- double()
mae <- double()
for(i in 1:j){

   train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   test <- window(x,start=test.start)
   train <- as.ts(window(x, end=train.end))
      fit <- as.ts(series(seas(train), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + j-2> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
   
    mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
Indicadores_Rev_COP_Outlier <- c(mean(mape), mean(me), mean(rmse),	mean(mae))

matriz_Indicadores_Pax_Real <- rbind(matriz_Indicadores_Pax_Real,Indicadores_Pax_Real)
matriz_Indicadores_Pax_Outlier <- rbind(matriz_Indicadores_Pax_Outlier,Indicadores_Pax_Outlier)
matriz_Indicadores_Rev_USD_Real <- rbind(matriz_Indicadores_Rev_USD_Real,Indicadores_Rev_USD_Real)
matriz_Indicadores_Rev_USD_Outlier <- rbind(matriz_Indicadores_Rev_USD_Outlier,Indicadores_Rev_USD_Outlier)
matriz_Indicadores_Rev_COP_Real <- rbind(matriz_Indicadores_Rev_COP_Real,Indicadores_Rev_COP_Real)
matriz_Indicadores_Rev_COP_Outlier <- rbind(matriz_Indicadores_Rev_COP_Outlier,Indicadores_Rev_COP_Outlier)

}

X13_forecast_Error_Pax_Real <- t(matriz_Indicadores_Pax_Real)
X13_forecast_Error_USD_Real <- t(matriz_Indicadores_Rev_USD_Real)
X13_forecast_Error_COP_Real <- t(matriz_Indicadores_Rev_COP_Real)

X13_forecast_Error_Pax_Outlier <- t(matriz_Indicadores_Pax_Outlier)
X13_forecast_Error_Rev_USD_Outlier <- t(matriz_Indicadores_Rev_USD_Outlier)
X13_forecast_Error_Rev_COP_Outlier <- t(matriz_Indicadores_Rev_COP_Outlier)

colnames(X13_forecast_Error_Pax_Real) <- colnames(Pax_Real)
colnames(X13_forecast_Error_USD_Real) <- colnames(Pax_Real)
colnames(X13_forecast_Error_COP_Real) <- colnames(Pax_Real)

colnames(X13_forecast_Error_Pax_Outlier) <- colnames(Pax_Real)
colnames(X13_forecast_Error_Rev_USD_Outlier) <- colnames(Pax_Real)
colnames(X13_forecast_Error_Rev_COP_Outlier) <- colnames(Pax_Real)

Vector_Indicadores <- c("MAPE","ME","RMSE","MAE")

rownames(X13_forecast_Error_Pax_Real) <- Vector_Indicadores
rownames(X13_forecast_Error_USD_Real) <- Vector_Indicadores
rownames(X13_forecast_Error_COP_Real) <- Vector_Indicadores

rownames(X13_forecast_Error_Pax_Outlier) <- Vector_Indicadores
rownames(X13_forecast_Error_Rev_USD_Outlier) <- Vector_Indicadores
rownames(X13_forecast_Error_Rev_COP_Outlier) <- Vector_Indicadores

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/X13")

write.table(file="Indicadores_PAX_Real.csv",X13_forecast_Error_Pax_Real)
write.table(file="Indicadores_REV_USD_Real.csv",X13_forecast_Error_USD_Real)
write.table(file="Indicadores_REV_COP_Real.csv",X13_forecast_Error_COP_Real)

write.table(file="Indicadores_PAX_Outliers.csv",X13_forecast_Error_Pax_Outlier)
write.table(file="Indicadores_REV_USD_Outliers.csv",X13_forecast_Error_Rev_USD_Outlier)
write.table(file="Indicadores_REV_COP_Outliers.csv",X13_forecast_Error_Rev_COP_Outlier)

