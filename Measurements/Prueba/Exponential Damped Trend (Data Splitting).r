library(forecast)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima")

Input_Pax_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/PAX_Real.csv"
Input_Rev_USD_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_USD_Real.csv"
Input_Rev_COP_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_COP_Real.csv"

Input_Pax_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/PAX_Outliers.csv"
Input_Rev_USD_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_USD_Outliers.csv"
Input_Rev_COP_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_COP_Outliers.csv"

Input_MINMAX <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"

Pax_Real <- read.table(Input_Pax_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Real <- read.table(Input_Rev_USD_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_COP_Real <- read.table(Input_Rev_COP_Real, sep=" ", header=TRUE, fileEncoding="latin1") 

Pax_Outliers <- read.table(Input_Pax_Outliers, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Outliers <- read.table(Input_Rev_USD_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")
Rev_USD_Outliers <- Rev_USD_Outliers[-c(6,28)]
Rev_COP_Outliers <- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")

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

hw_forecast_Error_Pax_Real <- data.frame()
hw_forecast_Error_USD_Real <- data.frame()
hw_forecast_Error_COP_Real <- data.frame()

hw_forecast_Error_Pax_Outlier <- data.frame()
hw_forecast_Error_Rev_USD_Outlier <- data.frame()
hw_forecast_Error_Rev_COP_Outlier <- data.frame()

DSPaxRealLeg <- data.frame()
DSPaxReal <- data.frame()

DSPaxOutlierLeg <- data.frame()
DSPaxOutlier <- data.frame()

DSUSDRealLeg <- data.frame()
DSUSDReal <- data.frame()

DSUSDOutliersLeg <- data.frame()
DSUSDOutliers <- data.frame()

DSCOPRealLeg <- data.frame()
DSCOPReal <- data.frame()

DSCOPOutliersLeg <- data.frame()
DSCOPOutliers <- data.frame()


getrmse <- function(x,h,j,l){
mape <- double()
me <- double()
rmse <- double()
mase <- double()
mae <- double()
l <- data.frame()

for(i in 1:j)
{

   train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   train <- window(x,end=train.end)
   test <- window(x,start=test.start)
   fit <- holt(train, exponential=TRUE,damped=TRUE)
   fc <- forecast(fit,h = h) 
   prueba <- as.data.frame(fit[2])
   
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
   
    l <- rbind(l,t(fit_ts))
  
	mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
	me <- c(me, accuracy(fit_ts,test_ts)[1,1])
	rmse <- c(rmse, accuracy(fit_ts,test_ts)[1,2])
	mae <- c(mae, accuracy(fit_ts,test_ts)[1,3])
   
   }
return(cbind(as.data.frame(c(mean(me),mean(rmse), mean(mae), mean(mape))),l))
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
	generarErrores <- getrmse(ts_Pax_Real, forecast_periods, iterations,DSPaxRealLeg)
	Indicadores_Pax_Real <- as.vector(t(generarErrores)[1,])
	DSPaxR <- generarErrores[,-1]
	DSPaxR <- cbind(DSPaxR,colnames(Pax_Real)[g])
	colnames(DSPaxR) <- colnames(DSPaxReal)
	matriz_Indicadores_Pax_Real <- rbind(matriz_Indicadores_Pax_Real,Indicadores_Pax_Real)
	DSPaxReal <- rbind2(DSPaxReal,DSPaxR)
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
	generarErrores <- getrmse(ts_Pax_Outlier, forecast_periods, iterations,DSPaxOutliersLeg)
	Indicadores_Pax_Outlier <- as.vector(t(generarErrores)[1,])
	DSPaxO <- generarErrores[,-1]
	DSPaxO <- cbind(DSPaxO,colnames(Pax_Outliers)[g])
	colnames(DSPaxO) <- colnames(DSPaxOutlier)
	matriz_Indicadores_Pax_Outlier <- rbind(matriz_Indicadores_Pax_Outlier,Indicadores_Pax_Outlier)
	DSPaxOutlier <- rbind2(DSPaxOutlier,DSPaxO)

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
	generarErrores <- getrmse(ts_Rev_USD_Real, forecast_periods, iterations,DSUSDRealLeg)
	Indicadores_Rev_USD_Real <- as.vector(t(generarErrores)[1,])
	DSUSDR <- generarErrores[,-1]
	DSUSDR <- cbind(DSUSDR,colnames(Rev_USD_Real)[g])
	colnames(DSUSDR) <- colnames(DSUSDReal)
	matriz_Indicadores_Rev_USD_Real <- rbind(matriz_Indicadores_Rev_USD_Real,Indicadores_Rev_USD_Real)
	DSUSDReal <- rbind2(DSUSDReal,DSUSDR)
}

for(g in 1:legsUSDOutliers){
	posRuta <- match(colnames(Rev_USD_Outliers)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	data_rev_usdesp_OUTLIERS <- Rev_USD_Outliers[g][which(rownames(Rev_USD_Outliers[g])>=inicio & rownames(Rev_USD_Outliers[g]) <= fin),]
	ts_Rev_USD_Outliers <- ts(data_rev_usdesp_OUTLIERS, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_Rev_USD_Outliers, forecast_periods, iterations,DSUSDOutliersLeg)
	Indicadores_Rev_USD_Outlier <- as.vector(t(generarErrores)[1,])
	DSUSDO <- generarErrores[,-1]
	DSUSDO <- cbind(DSUSDO,colnames(Rev_USD_Outliers)[g])
	colnames(DSUSDO) <- colnames(DSUSDOutliers)
	matriz_Indicadores_Rev_USD_Outlier <- rbind(matriz_Indicadores_Rev_USD_Outlier,Indicadores_Rev_USD_Outlier)
	DSUSDOutliers <- rbind2(DSUSDOutliers,DSUSDO)
}

for(g in 1:legsCOPReal){

	posRuta <- match(colnames(Rev_COP_Real)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	data_rev_copesp_REAL <- Rev_COP_Real[g][which(rownames(Rev_COP_Real[g])>=inicio & rownames(Rev_COP_Real[g]) <= fin),]
	ts_Rev_COP_Real <- ts(data_rev_copesp_REAL, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_Rev_COP_Real, forecast_periods, iterations,DSCOPRealLeg)
	Indicadores_Rev_COP_Real <- as.vector(t(generarErrores)[1,])
	DSCOPR <- generarErrores[,-1]
	DSCOPR<- cbind(DSCOPR,colnames(Rev_COP_Real)[g])
	colnames(DSCOPR) <- colnames(DSCOPReal)
	matriz_Indicadores_Rev_COP_Real <- rbind(matriz_Indicadores_Rev_COP_Real,Indicadores_Rev_COP_Real)
	DSCOPReal <- rbind2(DSCOPReal,DSCOPR)
}

for(g in 1:legsCOPOutliers){
	posRuta <- match(colnames(Rev_COP_Outliers)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	data_rev_copesp_OUTLIERS <- Rev_COP_Outliers[g][which(rownames(Rev_COP_Outliers[g])>=inicio & rownames(Rev_COP_Outliers[g]) <= fin),]
	ts_Rev_COP_Outliers <- ts(data_rev_copesp_OUTLIERS, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_Rev_COP_Outliers, forecast_periods, iterations,DSCOPOutliersLeg)
	Indicadores_Rev_COP_Outlier <- as.vector(t(generarErrores)[1,])
	DSCOPO <- generarErrores[,-1]
	DSCOPO<- cbind(DSCOPO,colnames(Rev_COP_Outliers)[g])
	colnames(DSCOPO) <- colnames(DSCOPOutliers)
	matriz_Indicadores_Rev_COP_Outlier <- rbind(matriz_Indicadores_Rev_COP_Outlier,Indicadores_Rev_COP_Outlier)
	DSCOPOutliers <- rbind2(DSCOPOutliers,DSCOPO)
}



hw_forecast_Error_Pax_Real <- t(matriz_Indicadores_Pax_Real)
hw_forecast_Error_USD_Real <- t(matriz_Indicadores_Rev_USD_Real)
hw_forecast_Error_COP_Real <- t(matriz_Indicadores_Rev_COP_Real)

hw_forecast_Error_Pax_Outlier <- t(matriz_Indicadores_Pax_Outlier)
hw_forecast_Error_Rev_USD_Outlier <- t(matriz_Indicadores_Rev_USD_Outlier)
hw_forecast_Error_Rev_COP_Outlier <- t(matriz_Indicadores_Rev_COP_Outlier)

colnames(hw_forecast_Error_Pax_Real) <- colnames(Pax_Real)
colnames(hw_forecast_Error_USD_Real) <- colnames(Rev_USD_Real)
colnames(hw_forecast_Error_COP_Real) <- colnames(Rev_COP_Real)

colnames(hw_forecast_Error_Pax_Outlier) <- colnames(Pax_Outliers)
colnames(hw_forecast_Error_Rev_USD_Outlier) <- colnames(Rev_USD_Outliers)
colnames(hw_forecast_Error_Rev_COP_Outlier) <- colnames(Rev_COP_Outliers)

Vector_Indicadores <- c("ME","RMSE","MAE","MAPE")

rownames(hw_forecast_Error_Pax_Real) <- Vector_Indicadores
rownames(hw_forecast_Error_USD_Real) <- Vector_Indicadores
rownames(hw_forecast_Error_COP_Real) <- Vector_Indicadores

rownames(hw_forecast_Error_Pax_Outlier) <- Vector_Indicadores
rownames(hw_forecast_Error_Rev_USD_Outlier) <- Vector_Indicadores
rownames(hw_forecast_Error_Rev_COP_Outlier) <- Vector_Indicadores

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/Prueba/Damped Trend")

write.table(file="Indicadores_PAX_Real.csv",hw_forecast_Error_Pax_Real)
write.table(file="Indicadores_REV_USD_Real.csv",hw_forecast_Error_USD_Real)
write.table(file="Indicadores_REV_COP_Real.csv",hw_forecast_Error_COP_Real)

write.table(file="Indicadores_PAX_Outliers.csv",hw_forecast_Error_Pax_Outlier)
write.table(file="Indicadores_REV_USD_Outliers.csv",hw_forecast_Error_Rev_USD_Outlier)
write.table(file="Indicadores_REV_COP_Outliers.csv",hw_forecast_Error_Rev_COP_Outlier)

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Prueba/Datos Data Splitting/Damped Trend")

write.table(file="Datos_PAX_Real.csv",DSPaxReal)
write.table(file="Datos_REV_USD_Real.csv",DSUSDReal)
write.table(file="Datos_REV_COP_Real.csv",DSCOPReal)

write.table(file="Datos_PAX_Outliers.csv",DSPaxOutlier)
write.table(file="Datos_REV_USD_Outliers.csv",DSUSDOutliers)
write.table(file="Datos_REV_COP_Outliers.csv",DSCOPOutliers)
