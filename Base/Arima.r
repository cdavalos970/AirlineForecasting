library(forecast)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima")
#Lee el archivo

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
confidence_level <- 95
iterations <- 4

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

Arima_forecast_Pax_Real <- data.frame()
Arima_forecast_USD_Real <- data.frame()
Arima_forecast_COP_Real <- data.frame()

Arima_forecast_Pax_Outlier <- data.frame()
Arima_forecast_Rev_USD_Outlier <- data.frame()
Arima_forecast_Rev_COP_Outlier <- data.frame()

Arima_forecast_Error_Pax_Real <- data.frame()
Arima_forecast_Error_USD_Real <- data.frame()
Arima_forecast_Error_COP_Real <- data.frame()

Arima_forecast_Error_Pax_Outlier <- data.frame()
Arima_forecast_Error_Rev_USD_Outlier <- data.frame()
Arima_forecast_Error_Rev_COP_Outlier <- data.frame()

getrmse <- function(x,h,j,k...)     #x representa los datos, h los periodos a pronosticar, j el número de corridas
{
mape <- double()
me <- double()
rmse <- double()
mase <- double()
mae <- double()

#OutputFile <- paste("E:/Jairo/Accuracy.txt", sep="")  #Ruta donde se guardara el output
for(i in 1:j)
{
#i=3
   train.end <- time(x)[length(x)-h-j+i]                                                                     #Fin del training set
   test.start <- time(x)[length(x)-h-j+i+1]                                                #Inicio del test set
   train <- window(x,end=train.end)
   test <- window(x,start=test.start)
   fit <- auto.arima(train)                                                                                                            #Corre ARIMA
   fc <- forecast(fit) 
   prueba <- as.data.frame(fc)[c(1,2,3),1]
   
   YYinicial <- start(test)[1]
   MMinicial <- start(test)[2]
   
   a <- ts(prueba,start=c(YYinicial,MMinicial), frequency = 12)
   
   YYFinal<-start(a)[1] 
   MMFinal<-start(a)[2]
   
   if (MMFinal + j-2> 12){
   fit_ts <- ts( a, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
   } else {
   fit_ts <- ts( a, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
   }
  
  mape <- c(mape, accuracy(a,test)[1,5])  #Se puede cambiar MAPE por MSE, RMSE, MAE, ETC
  me <- c(me, accuracy(a,test)[1,1])
   rmse <- c(rmse, accuracy(a,test)[1,2])
   mae <- c(mae, accuracy(a,test)[1,3])
   
   }
return(c(mean(me),mean(rmse), mean(mae), mean(mape)))

}

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

Arima_Pax_Real <- auto.arima(ts_Pax_Real)
Indicadores_Pax_Real <- getrmse(ts_Pax_Real, forecast_periods, iterations, "Pax_Real")
Arima_Rev_USD_Real <- auto.arima(ts_Rev_USD_Real)
Indicadores_Rev_USD_Real <- getrmse(ts_Rev_USD_Real, forecast_periods, iterations)
Arima_Rev_COP_Real <- auto.arima(ts_Rev_COP_Real)
Indicadores_Rev_COP_Real <- getrmse(ts_Rev_COP_Real, forecast_periods, iterations)

Arima_Pax_Outlier <- auto.arima(ts_Pax_Outlier)
Indicadores_Pax_Outlier  <- getrmse(ts_Pax_Outlier, forecast_periods, iterations)
Arima_Rev_USD_Outlier <- auto.arima(ts_Rev_USD_Outlier)
Indicadores_Rev_USD_Outlier  <- getrmse(ts_Rev_USD_Outlier, forecast_periods, iterations)
Arima_Rev_COP_Outlier <- auto.arima(ts_Rev_COP_Outlier)
Indicadores_Rev_COP_Outlier  <- getrmse(ts_Rev_COP_Outlier, forecast_periods, iterations)

matriz_Pax_Real <- rbind(matriz_Pax_Real,as.data.frame(forecast(Arima_Pax_Real))[c(1,2,3),1])
matriz_Rev_USD_Real <- rbind(matriz_Rev_USD_Real,as.data.frame(forecast(Arima_Rev_USD_Real))[c(1,2,3),1])
matriz_Rev_COP_Real <- rbind(matriz_Rev_COP_Real,as.data.frame(forecast(Arima_Rev_COP_Real))[c(1,2,3),1])

matriz_Pax_Outlier <- rbind(matriz_Pax_Outlier,as.data.frame(forecast(Arima_Pax_Outlier))[c(1,2,3),1])
matriz_Rev_USD_Outlier <- rbind(matriz_Rev_USD_Outlier,as.data.frame(forecast(Arima_Rev_USD_Outlier))[c(1,2,3),1])
matriz_Rev_COP_Outlier <- rbind(matriz_Rev_COP_Outlier,as.data.frame(forecast(Arima_Rev_COP_Outlier))[c(1,2,3),1])

matriz_Indicadores_Pax_Real <- rbind(matriz_Indicadores_Pax_Real,Indicadores_Pax_Real)
matriz_Indicadores_Rev_USD_Real <- rbind(matriz_Indicadores_Rev_USD_Real,Indicadores_Rev_USD_Real)
matriz_Indicadores_Rev_COP_Real <- rbind(matriz_Indicadores_Rev_COP_Real,Indicadores_Rev_COP_Real)

matriz_Indicadores_Pax_Outlier <- rbind(matriz_Indicadores_Pax_Outlier,Indicadores_Pax_Outlier)
matriz_Indicadores_Rev_USD_Outlier <- rbind(matriz_Indicadores_Rev_USD_Outlier,Indicadores_Rev_USD_Outlier)
matriz_Indicadores_Rev_COP_Outlier <- rbind(matriz_Indicadores_Rev_COP_Outlier,Indicadores_Rev_COP_Outlier)

}

Arima_forecast_Pax_Real <- t(matriz_Pax_Real)
Arima_forecast_USD_Real <- t(matriz_Rev_USD_Real)
Arima_forecast_COP_Real <- t(matriz_Rev_COP_Real)

Arima_forecast_Pax_Outlier <- t(matriz_Pax_Outlier)
Arima_forecast_Rev_USD_Outlier <- t(matriz_Rev_USD_Outlier)
Arima_forecast_Rev_COP_Outlier <- t(matriz_Rev_COP_Outlier)

Arima_forecast_Error_Pax_Real <- t(matriz_Indicadores_Pax_Real)
Arima_forecast_Error_USD_Real <- t(matriz_Indicadores_Rev_USD_Real)
Arima_forecast_Error_COP_Real <- t(matriz_Indicadores_Rev_COP_Real)

Arima_forecast_Error_Pax_Outlier <- t(matriz_Indicadores_Pax_Outlier)
Arima_forecast_Error_Rev_USD_Outlier <- t(matriz_Indicadores_Rev_USD_Outlier)
Arima_forecast_Error_Rev_COP_Outlier <- t(matriz_Indicadores_Rev_COP_Outlier)

colnames(Arima_forecast_Pax_Real) <- colnames(Pax_Real)
colnames(Arima_forecast_USD_Real) <- colnames(Pax_Real)
colnames(Arima_forecast_COP_Real) <- colnames(Pax_Real)

colnames(Arima_forecast_Pax_Outlier) <- colnames(Pax_Real)
colnames(Arima_forecast_Rev_USD_Outlier) <- colnames(Pax_Real)
colnames(Arima_forecast_Rev_COP_Outlier) <- colnames(Pax_Real)

colnames(Arima_forecast_Error_Pax_Real) <- colnames(Pax_Real)
colnames(Arima_forecast_Error_USD_Real) <- colnames(Pax_Real)
colnames(Arima_forecast_Error_COP_Real) <- colnames(Pax_Real)

colnames(Arima_forecast_Error_Pax_Outlier) <- colnames(Pax_Real)
colnames(Arima_forecast_Error_Rev_USD_Outlier) <- colnames(Pax_Real)
colnames(Arima_forecast_Error_Rev_COP_Outlier) <- colnames(Pax_Real)

rownames(Arima_forecast_Pax_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(Arima_forecast_USD_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(Arima_forecast_COP_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

rownames(Arima_forecast_Pax_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(Arima_forecast_Rev_USD_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(Arima_forecast_Rev_COP_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

Vector_Indicadores <- c("ME","RMSE","MAE","MAPE")

rownames(Arima_forecast_Error_Pax_Real) <- Vector_Indicadores
rownames(Arima_forecast_Error_USD_Real) <- Vector_Indicadores
rownames(Arima_forecast_Error_COP_Real) <- Vector_Indicadores

rownames(Arima_forecast_Error_Pax_Outlier) <- Vector_Indicadores
rownames(Arima_forecast_Error_Rev_USD_Outlier) <- Vector_Indicadores
rownames(Arima_forecast_Error_Rev_COP_Outlier) <- Vector_Indicadores

write.table(file="PAX_Outliers.csv",Arima_forecast_Pax_Real)
write.table(file="REV_USD_Outliers.csv",Arima_forecast_USD_Real)
write.table(file="REV_COP_Outliers.csv",Arima_forecast_COP_Real)

write.table(file="PAX_Real.csv",Arima_forecast_Pax_Outlier)
write.table(file="REV_USD_Real.csv",Arima_forecast_Rev_USD_Outlier)
write.table(file="REV_COP_Real.csv",Arima_forecast_Rev_COP_Outlier)

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/Arima")

write.table(file="Indicadores_PAX_Real.csv",Arima_forecast_Error_Pax_Real)
write.table(file="Indicadores_REV_USD_Real.csv",Arima_forecast_Error_USD_Real)
write.table(file="Indicadores_REV_COP_Real.csv",Arima_forecast_Error_COP_Real)

write.table(file="Indicadores_PAX_Outliers.csv",Arima_forecast_Error_Pax_Outlier)
write.table(file="Indicadores_REV_USD_Outliers.csv",Arima_forecast_Error_Rev_USD_Outlier)
write.table(file="Indicadores_REV_COP_Outliers.csv",Arima_forecast_Error_Rev_COP_Outlier)

