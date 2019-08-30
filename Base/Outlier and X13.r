library(seasonal)
library(forecast)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Input")
#Lee el archivo
InputFile <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Consolidado.txt"	
RAW_DATA <- read.table(InputFile, sep=";", header=TRUE, fileEncoding="latin1")
legs <- unique(RAW_DATA$LEG_OW)
cantDatos <- nrow(RAW_DATA)/length(legs)
iterations <- 4
forecast_periods <- 3
forecast_vector <- c(1,2,3)
x <- ts()
h <- integer()
j <- integer()
	

matriz_pax <- data.frame()
matriz_rev_usd <- data.frame()
matriz_rev_cop <- data.frame()

data_pax <- data.frame()
data_rev_usd <- data.frame()
data_rev_cop <- data.frame()

matriz_dataOutput_pax <- data.frame()
matriz_dataOutput_rev_usd <- data.frame()
matriz_dataOutput_rev_cop <- data.frame()

dataOutput_pax <- data.frame()
dataOutput_rev_usd <- data.frame()
dataOutput_rev_cop <- data.frame()

matriz_Pax_Real <- data.frame()
matriz_Rev_USD_Real <- data.frame()
matriz_Rev_COP_Real <- data.frame()

matriz_Pax_Outlier <- data.frame()
matriz_Rev_USD_Outlier <- data.frame()
matriz_Rev_COP_Outlier <- data.frame()

matriz_Indicadores_Pax_Real <- data.frame()
matriz_Indicadores_Rev_USD_Real <- data.frame()
matriz_Indicadores_Rev_COP_Real <- data.frame()

matriz_Indicadores_Pax_Outlier <- data.frame()
matriz_Indicadores_Rev_USD_Outlier <- data.frame()
matriz_Indicadores_Rev_COP_Outlier <- data.frame()

x13_forecast_Pax_Real <- data.frame()
x13_forecast_Rev_USD_Real <- data.frame()
x13_forecast_Rev_COP_Real <- data.frame()

x13_forecast_Pax_Outlier <- data.frame()
x13_forecast_Rev_USD_Outlier <- data.frame()
x13_forecast_Rev_COP_Outlier <- data.frame()

x13_forecast_Error_Pax_Real <- data.frame()
x13_forecast_Error_USD_Real <- data.frame()
x13_forecast_Error_COP_Real <- data.frame()

x13_forecast_Error_Pax_Outlier <- data.frame()
x13_forecast_Error_Rev_USD_Outlier <- data.frame()
x13_forecast_Error_Rev_COP_Outlier <- data.frame()

pax <- vector()
rev_usd <- vector()
rev_cop <- vector()

y <- 0

for(i in 1:length(legs)) {
y = y +1
pax[y] <- paste(legs[i],"PAX" )
rev_usd[y] <- paste(legs[i],"REV_USD" )
rev_cop[y] <- paste(legs[i],"REV_COP" ) 
}

for(g in 1:y) {

seriepax <- subset(RAW_DATA,LEG_OW == legs[g])$PAX 
serierev_usd <- subset(RAW_DATA,LEG_OW == legs[g])$REV_USD
serierev_cop <- subset(RAW_DATA,LEG_OW == legs[g])$REV_COP

matriz_pax <- rbind(matriz_pax,seriepax)
matriz_rev_usd <- rbind(matriz_rev_usd,serierev_usd)
matriz_rev_cop <- rbind(matriz_rev_cop,serierev_cop)

}  

data_pax <- as.data.frame(t(matriz_pax))
data_rev_usd <- as.data.frame(t(matriz_rev_usd))
data_rev_cop <- as.data.frame(t(matriz_rev_cop))

getIndicadores <- function(x,h,j,...)
{
	
for(i in 1:j)
{


   train.end <- time(x)[length(x)-h-i]
   test.start <- time(x)[length(x)-h-i+1] 
	test.end <- time(x)[length(x)-h+1] 
   test <- window(x,start=test.start, end = test.end)
   y <- x
   train <- as.ts(window(y,start = train.start, end=train.end))
      fit <- as.ts(series(seas(train), "fct")[,1])

   YYFinal<-start(fit)[1] 
   MMFinal<-start(fit)[2] 

   if (MMFinal + i -1> 12){
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + i-12-1))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + i-12-1))
   } else {
   fit_ts <- ts( fit, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+i-1))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+i-1))
   }
   
  mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
  me <- c(me, accuracy(fit,test_ts)[1,1])
   rmse <- c(rmse, accuracy(fit,test_ts)[1,2])
   mae <- c(mae, accuracy(fit,test_ts)[1,3])
    
 }
 return(c(mean(mape), mean(me), mean(rmse),	mean(mae)))
}

for(g in 1:y) {

Data_ts_pax <- ts(data_pax[g] , frequency = 12, start = c(2008, 1), end= c(2014, 12))
Data_ts_rev_usd <- ts(data_rev_usd[g] , frequency = 12, start = c(2008, 1), end= c(2014, 12))
Data_ts_rev_cop <- ts(data_rev_cop[g] , frequency = 12, start = c(2008, 1), end= c(2014, 12))

x13_model_Pax_Real <- seas(Data_ts_pax,outlier = NULL)
x13_model_Rev_USD_Real <- seas(Data_ts_rev_usd,outlier = NULL)
x13_model_Rev_COP_Real <- seas(Data_ts_rev_cop,outlier = NULL)

x13_model_Pax_Outlier <- seas(Data_ts_pax)
x13_model_Rev_USD_Outlier <- seas(Data_ts_rev_usd)
x13_model_Rev_COP_Outlier <- seas(Data_ts_rev_cop)

x13_Pax_Real <- series(x13_model_Pax_Real,"fct")
matriz_Pax_Real <- rbind(matriz_Pax_Real, x13_Pax_Real[forecast_vector,1])
x <- Data_ts_pax
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
matriz_Indicadores_Pax_Real <- rbind(matriz_Indicadores_Pax_Real,Indicadores_Pax_Real)
x13_Pax_Outlier <- series(x13_model_Pax_Outlier,"fct")
matriz_Pax_Outlier <- rbind(matriz_Pax_Outlier, x13_Pax_Outlier[forecast_vector,1])
Correccion_Pax <- series(x13_model_Pax_Outlier,"b1")
matriz_dataOutput_pax <- rbind(matriz_dataOutput_pax,Correccion_Pax)
x <- Data_ts_pax
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
matriz_Indicadores_Pax_Outlier <- rbind(matriz_Indicadores_Pax_Outlier,Indicadores_Pax_Outlier)

x13_Rev_USD_Real <- series(x13_model_Rev_USD_Real,"fct")
matriz_Rev_USD_Real <- rbind(matriz_Rev_USD_Real, x13_Rev_USD_Real[forecast_vector,1])
x <- Data_ts_rev_usd
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
matriz_Indicadores_Rev_USD_Real <- rbind(matriz_Indicadores_Rev_USD_Real,Indicadores_Rev_USD_Real)
x13_Rev_USD_Outlier <- series(x13_model_Rev_USD_Outlier,"fct")
matriz_Rev_USD_Outlier <- rbind(matriz_Rev_USD_Outlier, x13_Rev_USD_Outlier[forecast_vector,1])
Correccion_Rev_USD <- series(x13_model_Rev_USD_Outlier,"b1")
matriz_dataOutput_rev_usd <- rbind(matriz_dataOutput_rev_usd,Correccion_Rev_USD)
x <- Data_ts_rev_usd
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
matriz_Indicadores_Rev_USD_Outlier <- rbind(matriz_Indicadores_Rev_USD_Outlier,Indicadores_Rev_USD_Outlier)

x13_Rev_COP_Real <- series(x13_model_Rev_COP_Real,"fct")
matriz_Rev_COP_Real <- rbind(matriz_Rev_COP_Real, x13_Rev_COP_Real[forecast_vector,1])
x <- Data_ts_rev_cop
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
matriz_Indicadores_Rev_COP_Real <- rbind(matriz_Indicadores_Rev_COP_Real,Indicadores_Rev_COP_Real)
x13_Rev_COP_Outlier <- series(x13_model_Rev_COP_Outlier,"fct")
matriz_Rev_COP_Outlier <- rbind(matriz_Rev_COP_Outlier, x13_Rev_COP_Outlier[forecast_vector,1])
Correccion_Rev_COP <- series(x13_model_Rev_COP_Outlier,"b1")
matriz_dataOutput_rev_cop <- rbind(matriz_dataOutput_rev_cop,Correccion_Rev_COP)
x <- Data_ts_rev_cop
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
matriz_Indicadores_Rev_COP_Outlier <- rbind(matriz_Indicadores_Rev_COP_Outlier,Indicadores_Rev_COP_Outlier)
}

x13_forecast_Pax_Real <- as.data.frame(t(matriz_Pax_Real))
x13_forecast_Rev_USD_Real <- as.data.frame(t(matriz_Rev_USD_Real))
x13_forecast_Rev_COP_Real <- as.data.frame(t(matriz_Rev_COP_Real))

x13_forecast_Pax_Outlier <- as.data.frame(t(matriz_Pax_Outlier))
x13_forecast_Rev_USD_Outlier <- as.data.frame(t(matriz_Rev_USD_Outlier))
x13_forecast_Rev_COP_Outlier <- as.data.frame(t(matriz_Rev_COP_Outlier))

dataOutput_pax <- as.data.frame(t(matriz_dataOutput_pax))
dataOutput_rev_usd <- as.data.frame(t(matriz_dataOutput_rev_usd))
dataOutput_rev_cop <- as.data.frame(t(matriz_dataOutput_rev_cop))

x13_forecast_Error_Pax_Real <- as.data.frame(t(matriz_Indicadores_Pax_Real))
x13_forecast_Error_USD_Real <- as.data.frame(t(matriz_Indicadores_Rev_USD_Real))
x13_forecast_Error_COP_Real <- as.data.frame(t(matriz_Indicadores_Rev_COP_Real))

x13_forecast_Error_Pax_Outlier <- as.data.frame(t(matriz_Indicadores_Pax_Outlier))
x13_forecast_Error_Rev_USD_Outlier <- as.data.frame(t(matriz_Indicadores_Rev_USD_Outlier))
x13_forecast_Error_Rev_COP_Outlier <- as.data.frame(t(matriz_Indicadores_Rev_COP_Outlier))


colnames(dataOutput_pax) <- legs
colnames(dataOutput_rev_usd) <- legs
colnames(dataOutput_rev_cop) <- legs

colnames(data_pax) <- legs
colnames(data_rev_usd) <- legs
colnames(data_rev_cop) <- legs

colnames(x13_forecast_Pax_Real) <- legs
colnames(x13_forecast_Rev_USD_Real) <- legs
colnames(x13_forecast_Rev_COP_Real) <- legs

colnames(x13_forecast_Pax_Outlier) <- legs
colnames(x13_forecast_Rev_USD_Outlier) <- legs
colnames(x13_forecast_Rev_COP_Outlier) <- legs

colnames(x13_forecast_Error_Pax_Outlier) <- legs
colnames(x13_forecast_Error_Rev_USD_Outlier) <- legs
colnames(x13_forecast_Error_Rev_COP_Outlier) <- legs

colnames(x13_forecast_Error_Pax_Real) <- legs
colnames(x13_forecast_Error_USD_Real) <- legs
colnames(x13_forecast_Error_COP_Real) <- legs


rownames(dataOutput_pax) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)
rownames(dataOutput_rev_usd) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)
rownames(dataOutput_rev_cop) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)

rownames(data_pax) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)
rownames(data_rev_usd) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)
rownames(data_rev_cop) <- seq(as.Date("2008/1/1"),by = "month", length.out = cantDatos)

rownames(x13_forecast_Pax_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(x13_forecast_Rev_USD_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(x13_forecast_Rev_COP_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

rownames(x13_forecast_Pax_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(x13_forecast_Rev_USD_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(x13_forecast_Rev_COP_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

Vector_Indicadores <- c("MAPE","ME","RMSE","MAE")

rownames(x13_forecast_Error_Pax_Outlier) <- Vector_Indicadores
rownames(x13_forecast_Error_Rev_USD_Outlier) <- Vector_Indicadores
rownames(x13_forecast_Error_Rev_COP_Outlier) <- Vector_Indicadores

rownames(x13_forecast_Error_Pax_Real) <- Vector_Indicadores
rownames(x13_forecast_Error_USD_Real) <- Vector_Indicadores
rownames(x13_forecast_Error_COP_Real) <- Vector_Indicadores


setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/X13")

write.table(file="PAX_Outliers.csv",x13_forecast_Error_Pax_Outlier)
write.table(file="REV_USD_Outliers.csv",x13_forecast_Error_Rev_USD_Outlier)
write.table(file="REV_COP_Outliers.csv",x13_forecast_Error_Rev_COP_Outlier)

write.table(file="PAX_Real.csv",x13_forecast_Error_Pax_Real)
write.table(file="REV_USD_Real.csv",x13_forecast_Error_USD_Real)
write.table(file="REV_COP_Real.csv",x13_forecast_Error_COP_Real)

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier")

write.table(file="PAX_Real.csv",data_pax)
write.table(file="REV_USD_Real.csv",data_rev_usd)
write.table(file="REV_COP_Real.csv",data_rev_cop)

write.table(file="PAX_Outliers.csv",dataOutput_pax)
write.table(file="REV_USD_Outliers.csv",dataOutput_rev_usd)
write.table(file="REV_COP_Outliers.csv",dataOutput_rev_cop)

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13")

write.table(file="PAX_Outliers.csv",x13_forecast_Pax_Real)
write.table(file="REV_USD_Outliers.csv",x13_forecast_Rev_USD_Real)
write.table(file="REV_COP_Outliers.csv",x13_forecast_Rev_COP_Real)

write.table(file="PAX_Real.csv",x13_forecast_Pax_Outlier)
write.table(file="REV_USD_Real.csv",x13_forecast_Rev_USD_Outlier)
write.table(file="REV_COP_Real.csv",x13_forecast_Rev_COP_Outlier)




