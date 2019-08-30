library(forecast)

Input_TMPAXOUTLIERvsREVREAL <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/TM/TMPAXOUTLIERvsREVREAL.csv"
Input_TMPAXOvsREVOUTLIER <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/TM/TMPAXOvsREVOUTLIER.csv"
Input_TMPAXREALvsREVOUTLIER <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/TM/TMPAXREALvsREVOUTLIER.csv"
Input_TMPAXREALvsREVREAL <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/TM/TMPAXREALvsREVREAL.csv"

Input_MINMAX <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"

TMPAXOUTLIERvsREVREAL <- read.table(Input_TMPAXOUTLIERvsREVREAL, sep=" ", header=TRUE, fileEncoding="latin1") 
TMPAXOUTLIERvsREVREAL <- TMPAXOUTLIERvsREVREAL[-c(143)]
TMPAXOvsREVOUTLIER <- read.table(Input_TMPAXOvsREVOUTLIER, sep=" ", header=TRUE, fileEncoding="latin1") 
TMPAXOvsREVOUTLIER <- TMPAXOvsREVOUTLIER[-c(157)]
TMPAXREALvsREVOUTLIER <- read.table(Input_TMPAXREALvsREVOUTLIER, sep=" ", header=TRUE, fileEncoding="latin1") 
TMPAXREALvsREVOUTLIER <- TMPAXREALvsREVOUTLIER[-c(153)]
TMPAXREALvsREVREAL <- read.table(Input_TMPAXREALvsREVREAL, sep=" ", header=TRUE, fileEncoding="latin1") 

MINMAX <- read.table(Input_MINMAX, sep=" ", header=TRUE, fileEncoding="latin1")

legsTMPAXOUTLIERvsREVREAL <- ncol(TMPAXOUTLIERvsREVREAL)
legsTMPAXOvsREVOUTLIER <- ncol(TMPAXOvsREVOUTLIER)
legsTMPAXREALvsREVOUTLIER <- ncol(TMPAXREALvsREVOUTLIER)
legsTMPAXREALvsREVREAL <- ncol(TMPAXREALvsREVREAL)

forecast_periods <- 3
confidence_level <- 95
iterations <- 4

matriz_TMPAXOUTLIERvsREVREAL <- data.frame()
matriz_TMPAXOvsREVOUTLIER <- data.frame()
matriz_TMPAXREALvsREVOUTLIER <- data.frame()
matriz_TMPAXREALvsREVREAL <- data.frame()

Forecast_Error_TMPAXOUTLIERvsREVREAL <- data.frame()
Forecast_Error_TMPAXOvsREVOUTLIER <- data.frame()
Forecast_Error_TMPAXREALvsREVOUTLIER <- data.frame()
Forecast_Error_TMPAXREALvsREVREAL <- data.frame()

DSTMPAXOUTLIERvsREVREALT <- data.frame()
DSTMPAXOvsREVOUTLIERT <- data.frame()
DSTMPAXREALvsREVOUTLIERT <- data.frame()
DSTMPAXREALvsREVREALT <- data.frame()

getrmse <- function(x,h,j,l){
mape <- double()
me <- double()
rmse <- double()
mase <- double()
mae <- double()
l <- data.frame()

for(i in 1:j){
   train.end <- time(x)[length(x)-h-j+i]
   test.start <- time(x)[length(x)-h-j+i+1]
   train <- window(x,end=train.end)
   test <- window(x,start=test.start)
   fit <- HoltWinters(train)
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
   
    l <- rbind(l,t(fit_ts))
  
	mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
	me <- c(me, accuracy(fit_ts,test_ts)[1,1])
	rmse <- c(rmse, accuracy(fit_ts,test_ts)[1,2])
	mae <- c(mae, accuracy(fit_ts,test_ts)[1,3])
   
   }
return(cbind(as.data.frame(c(mean(me),mean(rmse), mean(mae), mean(mape))),l))
}

for(g in 1:legsTMPAXOUTLIERvsREVREAL){
	posRuta <- match(colnames(TMPAXOUTLIERvsREVREAL)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	serie_TMPAXOUTLIERvsREVREAL <- TMPAXOUTLIERvsREVREAL[g][which(rownames(TMPAXOUTLIERvsREVREAL[g])>=inicio & rownames(TMPAXOUTLIERvsREVREAL[g]) <= fin),]
	ts_TMPAXOUTLIERvsREVREAL <- ts(serie_TMPAXOUTLIERvsREVREAL, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_TMPAXOUTLIERvsREVREAL, forecast_periods, iterations)
	Indicadores_TMPAXOUTLIERvsREVREAL <- as.vector(t(generarErrores)[1,])
	DSTMPAXOUTLIERvsREVREAL <- generarErrores[,-1]
	DSTMPAXOUTLIERvsREVREAL <- cbind(DSTMPAXOUTLIERvsREVREAL,colnames(TMPAXOUTLIERvsREVREAL)[g])
	colnames(DSTMPAXOUTLIERvsREVREAL) <- colnames(DSTMPAXOUTLIERvsREVREALT)
	matriz_TMPAXOUTLIERvsREVREAL <- rbind(matriz_TMPAXOUTLIERvsREVREAL,Indicadores_TMPAXOUTLIERvsREVREAL)
	DSTMPAXOUTLIERvsREVREALT <- rbind2(DSTMPAXOUTLIERvsREVREALT,DSTMPAXOUTLIERvsREVREAL)
	}
	
for(g in 1:legsTMPAXOvsREVOUTLIER){
	posRuta <- match(colnames(TMPAXOvsREVOUTLIER)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	serie_TMPAXOvsREVOUTLIER <- TMPAXOvsREVOUTLIER[g][which(rownames(TMPAXOvsREVOUTLIER[g])>=inicio & rownames(TMPAXOvsREVOUTLIER[g]) <= fin),]
	ts_TMPAXOvsREVOUTLIER <- ts(serie_TMPAXOvsREVOUTLIER, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_TMPAXOvsREVOUTLIER, forecast_periods, iterations)
	Indicadores_TMPAXOvsREVOUTLIER <- as.vector(t(generarErrores)[1,])
	DSTMPAXOvsREVOUTLIER <- generarErrores[,-1]
	DSTMPAXOvsREVOUTLIER <- cbind(DSTMPAXOvsREVOUTLIER,colnames(TMPAXOvsREVOUTLIER)[g])
	colnames(DSTMPAXOvsREVOUTLIER) <- colnames(DSTMPAXOvsREVOUTLIERT)
	matriz_TMPAXOvsREVOUTLIER <- rbind(matriz_TMPAXOvsREVOUTLIER,Indicadores_TMPAXOvsREVOUTLIER)
	DSTMPAXOvsREVOUTLIERT <- rbind2(DSTMPAXOvsREVOUTLIERT,DSTMPAXOvsREVOUTLIER)
	}
	
for(g in 1:legsTMPAXREALvsREVOUTLIER){
	posRuta <- match(colnames(TMPAXREALvsREVOUTLIER)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	serie_TMPAXREALvsREVOUTLIER <- TMPAXREALvsREVOUTLIER[g][which(rownames(TMPAXREALvsREVOUTLIER[g])>=inicio & rownames(TMPAXREALvsREVOUTLIER[g]) <= fin),]
	ts_TMPAXREALvsREVOUTLIER <- ts(serie_TMPAXREALvsREVOUTLIER, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_TMPAXREALvsREVOUTLIER, forecast_periods, iterations)
	Indicadores_TMPAXREALvsREVOUTLIER <- as.vector(t(generarErrores)[1,])
	DSTMPAXREALvsREVOUTLIER <- generarErrores[,-1]
	DSTMPAXREALvsREVOUTLIER <- cbind(DSTMPAXREALvsREVOUTLIER,colnames(TMPAXREALvsREVOUTLIER)[g])
	colnames(DSTMPAXREALvsREVOUTLIER) <- colnames(DSTMPAXREALvsREVOUTLIERT)
	matriz_TMPAXREALvsREVOUTLIER <- rbind(matriz_TMPAXREALvsREVOUTLIER,Indicadores_TMPAXREALvsREVOUTLIER)
	DSTMPAXREALvsREVOUTLIERT <- rbind2(DSTMPAXREALvsREVOUTLIERT,DSTMPAXREALvsREVOUTLIER)
	}
	
for(g in 1:legsTMPAXREALvsREVREAL){
	posRuta <- match(colnames(TMPAXREALvsREVREAL)[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	serie_TMPAXREALvsREVREAL <- TMPAXREALvsREVREAL[g][which(rownames(TMPAXREALvsREVREAL[g])>=inicio & rownames(TMPAXREALvsREVREAL[g]) <= fin),]
	ts_TMPAXREALvsREVREAL <- ts(serie_TMPAXREALvsREVREAL, frequency = 12, start = c(añoInicio,mesInicio), end= c(añoFin,mesFin))
	generarErrores <- getrmse(ts_TMPAXREALvsREVREAL, forecast_periods, iterations)
	Indicadores_TMPAXREALvsREVREAL <- as.vector(t(generarErrores)[1,])
	DSTMPAXREALvsREVREAL <- generarErrores[,-1]
	DSTMPAXREALvsREVREAL <- cbind(DSTMPAXREALvsREVREAL,colnames(TMPAXREALvsREVREAL)[g])
	colnames(DSTMPAXREALvsREVREAL) <- colnames(DSTMPAXREALvsREVREALT)
	matriz_TMPAXREALvsREVREAL <- rbind(matriz_TMPAXREALvsREVREAL,Indicadores_TMPAXREALvsREVREAL)
	DSTMPAXREALvsREVREALT <- rbind2(DSTMPAXREALvsREVREALT,DSTMPAXREALvsREVREAL)
	}

Forecast_Error_TMPAXOUTLIERvsREVREAL <- t(matriz_TMPAXOUTLIERvsREVREAL)
Forecast_Error_TMPAXOvsREVOUTLIER <- t(matriz_TMPAXOvsREVOUTLIER)
Forecast_Error_TMPAXREALvsREVOUTLIER <- t(matriz_TMPAXREALvsREVOUTLIER)
Forecast_Error_TMPAXREALvsREVREAL <- t(matriz_TMPAXREALvsREVREAL)

colnames(Forecast_Error_TMPAXOUTLIERvsREVREAL) <- colnames(TMPAXOUTLIERvsREVREAL)
colnames(Forecast_Error_TMPAXOvsREVOUTLIER) <- colnames(TMPAXOvsREVOUTLIER)
colnames(Forecast_Error_TMPAXREALvsREVOUTLIER) <- colnames(TMPAXREALvsREVOUTLIER)
colnames(Forecast_Error_TMPAXREALvsREVREAL) <- colnames(TMPAXREALvsREVREAL)

Vector_Indicadores <- c("ME","RMSE","MAE","MAPE")

rownames(Forecast_Error_TMPAXOUTLIERvsREVREAL) <- Vector_Indicadores
rownames(Forecast_Error_TMPAXOvsREVOUTLIER) <- Vector_Indicadores
rownames(Forecast_Error_TMPAXREALvsREVOUTLIER) <- Vector_Indicadores
rownames(Forecast_Error_TMPAXREALvsREVREAL) <- Vector_Indicadores

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Indicadores/Prueba/TM/HW")

write.table(file="Indicadores_TMPAXOUTLIERvsREVREAL.csv",Forecast_Error_TMPAXOUTLIERvsREVREAL)
write.table(file="Indicadores_TMPAXOUTLIERvsREVOUTLIER.csv",Forecast_Error_TMPAXOvsREVOUTLIER)
write.table(file="Indicadores_TMPAXREALvsREVOUTLIER.csv",Forecast_Error_TMPAXREALvsREVOUTLIER)
write.table(file="Indicadores_TMPAXREALvsREVREAL.csv",Forecast_Error_TMPAXREALvsREVREAL)

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Prueba/Datos Data Splitting/TM/HW")

write.table(file="Datos_TMPAXOUTLIERvsREVREAL.csv",DSTMPAXOUTLIERvsREVREALT)
write.table(file="Datos_TMPAXOvsREVOUTLIER.csv",DSTMPAXOvsREVOUTLIERT)
write.table(file="Datos_TMPAXREALvsREVOUTLIER.csv",DSTMPAXREALvsREVOUTLIERT)
write.table(file="Datos_TMPAXREALvsREVREAL.csv",DSTMPAXREALvsREVREALT)
