library(forecast)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima")
#Lee el archivo

Input_Pax_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Real.csv"
Input_Rev_USD_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Real.csv"
Input_Rev_COP_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Real.csv"

Input_Pax_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Outliers.csv"
Input_Rev_USD_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Outliers.csv"
Input_Rev_COP_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Outliers.csv"

Input_MINMAX <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"

Pax_Real <- read.table(Input_Pax_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Real <- read.table(Input_Rev_USD_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_COP_Real <- read.table(Input_Rev_COP_Real, sep=" ", header=TRUE, fileEncoding="latin1") 

Pax_Outliers <- read.table(Input_Pax_Outliers, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Outliers<- read.table(Input_Rev_USD_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")
Rev_COP_Outliers<- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")

MINMAX <- read.table(Input_MINMAX, sep=" ", header=TRUE, fileEncoding="latin1")

legsPaxReal <- ncol(Pax_Real)
cantDatosPaxReal <- nrow(Pax_Real)

legsUSDReal <- ncol(Rev_USD_Real)
cantDatosUSDReal <- nrow(Rev_USD_Real)

legsCOPReal <- ncol(Rev_COP_Real)
cantDatosCOPReal <- nrow(Rev_COP_Real)

legsPaxOutliers <- ncol(Pax_Outliers)
cantDatosPaxOutliers <- nrow(Pax_Outliers)

legsUSDOutliers <- ncol(Rev_USD_Outliers)
cantDatosUSDOutliers <- nrow(Rev_USD_Outliers)

legsCOPOutliers <- ncol(Rev_COP_Outliers)
cantDatosOutliers <- nrow(Rev_COP_Outliers)

forecast_periods <- 3
confidence_level <- 95
iterations <- 4

matriz_Indicadores_Pax_Real <- data.frame()
matriz_Indicadores_Rev_USD_Real <- data.frame()
matriz_Indicadores_Rev_COP_Real <- data.frame()

matriz_Indicadores_Pax_Outlier <- data.frame()
matriz_Indicadores_Rev_USD_Outlier <- data.frame()
matriz_Indicadores_Rev_COP_Outlier <- data.frame()

ets_forecast_Error_Pax_Real <- data.frame()
ets_forecast_Error_USD_Real <- data.frame()
ets_forecast_Error_COP_Real <- data.frame()

ets_forecast_Error_Pax_Outlier <- data.frame()
ets_forecast_Error_Rev_USD_Outlier <- data.frame()
ets_forecast_Error_Rev_COP_Outlier <- data.frame()

getrmse <- function(x,h,j,...)     #x representa los datos, h los periodos a pronosticar, j el número de corridas
{
mape <- double()
me <- double()
rmse <- double()
mae <- double()
mase <- double()

#OutputFile <- paste("E:/Jairo/Accuracy.txt", sep="")  #Ruta donde se guardara el output
for(i in 1:j)
{
#i=3
   train.end <- time(x)[length(x)-h-j+i]                                                                     #Fin del training set
   test.start <- time(x)[length(x)-h-j+i+1]                                                #Inicio del test set
   train <- window(x,end=train.end)
   test <- window(x,start=test.start)
   fit <- ets(train)                                                                                                           #Corre ARIMA
    fc <- forecast(fit,h = h) 
   prueba <- as.data.frame(fc[2])
   
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
   }                                                                                                                   #Pronóstico
  
  mape <- c(mape, accuracy(a,test)[1,5])  #Se puede cambiar MAPE por MSE, RMSE, MAE, ETC
  me <- c(me, accuracy(a,test)[1,1])
   rmse <- c(rmse, accuracy(a,test)[1,2])
   mae <- c(mae, accuracy(a,test)[1,3])
   
   }
return(c(mean(me),mean(rmse), mean(mae), mean(mape)))
}

for(g in 1:legsPaxReal){
	posRuta <- match(colnames(Pax_Real)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	seriepaxesp_REAL <- Pax_Real[g][which(rownames(Pax_Real[g])>=inicio & rownames(Pax_Real[g]) <= fin),]
	ts_Pax_Real <- ts(seriepaxesp_REAL, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	Indicadores_Pax_Real <- getrmse(ts_Pax_Real, forecast_periods, iterations)
	matriz_Indicadores_Pax_Real <- rbind(matriz_Indicadores_Pax_Real,Indicadores_Pax_Real)
}

for(g in 1:legsPaxOutliers){
	posRuta <- match(colnames(Pax_Outliers)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	seriepaxesp_OUTLIER <- Pax_Outliers[g][which(rownames(Pax_Outliers[g])>=inicio & rownames(Pax_Outliers[g]) <= fin),]
	ts_Pax_Outlier <- ts(seriepaxesp_OUTLIER, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	Indicadores_Pax_Outlier  <- getrmse(ts_Pax_Outlier, forecast_periods, iterations)
	matriz_Indicadores_Pax_Outlier <- rbind(matriz_Indicadores_Pax_Outlier,Indicadores_Pax_Outlier)
}

for(g in 1:legsUSDReal){

	posRuta <- match(colnames(Rev_USD_Real)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	data_rev_usdesp_REAL <- Rev_USD_Real[g][which(rownames(Rev_USD_Real[g])>=inicio & rownames(Rev_USD_Real[g]) <= fin),]
	ts_Rev_USD_Real <- ts(data_rev_usdesp_REAL, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	Indicadores_Rev_USD_Real <- getrmse(ts_Rev_USD_Real, forecast_periods, iterations)
	matriz_Indicadores_Rev_USD_Real <- rbind(matriz_Indicadores_Rev_USD_Real,Indicadores_Rev_USD_Real)
}

for(g in 1:legsUSDOutliers){
	posRuta <- match(colnames(REV_USD_Outliers)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	data_rev_usdesp_OUTLIER <- Rev_USD_Outliers[g][which(rownames(Rev_USD_Outliers[g])>=inicio & rownames(Rev_USD_Outliers[g]) <= fin),]
	ts_Rev_USD_Outlier <- ts(data_rev_usdesp_OUTLIER, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	Indicadores_Rev_USD_Outlier  <- getrmse(ts_Rev_USD_Outlier, forecast_periods, iterations)
	matriz_Indicadores_Rev_USD_Outlier <- rbind(matriz_Indicadores_Rev_USD_Outlier,Indicadores_Rev_USD_Outlier)
}

for(g in 1:legsCOPReal){
	posRuta <- match(colnames(REV_COP_Real)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))


}

for(g in 1:legsCOPOutliers){
	posRuta <- match(colnames(REV_COP_Outliers)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))


}

ets_forecast_Error_Pax_Real <- t(matriz_Indicadores_Pax_Real)
ets_forecast_Error_USD_Real <- t(matriz_Indicadores_Rev_USD_Real)
ets_forecast_Error_COP_Real <- t(matriz_Indicadores_Rev_COP_Real)

ets_forecast_Error_Pax_Outlier <- t(matriz_Indicadores_Pax_Outlier)
ets_forecast_Error_Rev_USD_Outlier <- t(matriz_Indicadores_Rev_USD_Outlier)
ets_forecast_Error_Rev_COP_Outlier <- t(matriz_Indicadores_Rev_COP_Outlier)

colnames(ets_forecast_Error_Pax_Real) <- colnames(Pax_Real)
colnames(ets_forecast_Error_USD_Real) <- colnames(Pax_Real)
colnames(ets_forecast_Error_COP_Real) <- colnames(Pax_Real)

colnames(ets_forecast_Error_Pax_Outlier) <- colnames(Pax_Real)
colnames(ets_forecast_Error_Rev_USD_Outlier) <- colnames(Pax_Real)
colnames(ets_forecast_Error_Rev_COP_Outlier) <- colnames(Pax_Real)

Vector_Indicadores <- c("ME","RMSE","MAE","MAPE")

rownames(ets_forecast_Error_Pax_Real) <- Vector_Indicadores
rownames(ets_forecast_Error_USD_Real) <- Vector_Indicadores
rownames(ets_forecast_Error_COP_Real) <- Vector_Indicadores

rownames(ets_forecast_Error_Pax_Outlier) <- Vector_Indicadores
rownames(ets_forecast_Error_Rev_USD_Outlier) <- Vector_Indicadores
rownames(ets_forecast_Error_Rev_COP_Outlier) <- Vector_Indicadores


setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/ETS")

write.table(file="Indicadores_PAX_Real.csv",ets_forecast_Error_Pax_Real)
write.table(file="Indicadores_REV_USD_Real.csv",ets_forecast_Error_USD_Real)
write.table(file="Indicadores_REV_COP_Real.csv",ets_forecast_Error_COP_Real)

write.table(file="Indicadores_PAX_Outliers.csv",ets_forecast_Error_Pax_Outlier)
write.table(file="Indicadores_REV_USD_Outliers.csv",ets_forecast_Error_Rev_USD_Outlier)
write.table(file="Indicadores_REV_COP_Outliers.csv",ets_forecast_Error_Rev_COP_Outlier)

