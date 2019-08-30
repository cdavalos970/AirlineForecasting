library(forecast)

Ruta_Consolidado <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Prueba/Clasificacion.csv"
Consolidado <- read.table(Ruta_Consolidado, sep=",", header=TRUE, fileEncoding="latin1")

Input_Consolidado <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Consolidado Total.txt"
RAW_DATA <- read.table(Input_Consolidado, sep=";", header=TRUE, fileEncoding="latin1")


Input_REGIONES <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Regiones.txt"
REGIONES <- read.table(Input_REGIONES, sep=";", header=TRUE, fileEncoding="latin1")

Input_MINMAX <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"
MINMAX <- read.table(Input_MINMAX, sep=" ", header=TRUE, fileEncoding="latin1")

Rutas <- unique(Consolidado$LEG_OW)
cantRutas <- length(Rutas)

AGREGADOTM <- data.frame()
INPUTPAXC <- data.frame()
INPUTREVC <- data.frame()
RESULTADO <- data.frame()

forecast_periods <- 3
confidence_level <- 95
iterations <- 4

h <- forecast_periods
j <- iterations

for(i in 1:cantRutas){
	posPaxReal <- match(paste(Rutas[i],"Pax_Real",sep = " "),Consolidado[,6])
	posPaxOutlier <- match(paste(Rutas[i],"Pax_Outlier",sep = " "),Consolidado[,6])

	if(is.na(Consolidado[posPaxReal,5]) & !is.na(Consolidado[posPaxOutlier,5])){
		MetodoPax <- Consolidado[posPaxOutlier,2]
		TipoPax <- Consolidado[posPaxOutlier,3]
	} else if(!is.na(Consolidado[posPaxReal,5]) & is.na(Consolidado[posPaxOutlier,5])){
		MetodoPax <- Consolidado[posPaxReal,2]
		TipoPax <- Consolidado[posPaxReal,3]
	} else {
		if(Consolidado[posPaxReal,5] < Consolidado[posPaxOutlier,5]){
			MetodoPax <- Consolidado[posPaxReal,2]
			TipoPax <- Consolidado[posPaxReal,3]
		} else if(Consolidado[posPaxReal,5] > Consolidado[posPaxOutlier,5]){
			MetodoPax <- Consolidado[posPaxOutlier,2]
			TipoPax <- Consolidado[posPaxOutlier,3]
		} else {
		MetodoPax <- Consolidado[posPaxReal,2]
		TipoPax <- Consolidado[posPaxReal,3]
		}
	}
	
	divisionPax <- as.data.frame(strsplit(as.character(TipoPax),"_"))[2,1]
	Ruta_Pax <- paste("C:/Users/cdavalos/Documents/Pronostico TM/R/Prueba/Datos Data Splitting/",MetodoPax,"/Datos_PAX_",divisionPax,".csv",sep = "")
	InputPax <- read.table(toString(Ruta_Pax), sep=" ", header=TRUE, fileEncoding="latin1")
	colnames(InputPax) <- c("PPaxUno","PPaxDos","PPaxTres","LEG_OW")
	
	if(TipoPax == "Pax_Outlier"){
		Ruta_Pax_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/PAX_Outliers.csv"
	} else Ruta_Pax_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/PAX_Real.csv"
	InputPax_Input <- read.table(Ruta_Pax_Input, sep=" ", header=TRUE, fileEncoding="latin1")
	
	
	posRevUSDReal <- match(paste(Rutas[i],"REV_USD_Real",sep = " "),Consolidado[,6])
	posRevUSDOutlier <- match(paste(Rutas[i],"REV_USD_Outliers",sep = " "),Consolidado[,6])
	
	posRevCOPReal <- match(paste(Rutas[i],"REV_COP_Real",sep = " "),Consolidado[,6])
	posRevCOPOutlier <- match(paste(Rutas[i],"REV_COP_Outliers",sep = " "),Consolidado[,6])
	
	if(is.na(posRevUSDReal) & is.na(posRevUSDOutlier) & !is.na(posRevCOPReal) & !is.na(posRevCOPOutlier)){
		if(Consolidado[posRevCOPReal,5] < Consolidado[posRevCOPOutlier,5]){
			MetodoRev <- Consolidado[posRevCOPReal,2]
			TipoRev <- Consolidado[posRevCOPReal,3]
		} else if(Consolidado[posRevCOPReal,5] > Consolidado[posRevCOPOutlier,5]){
			MetodoRev <- Consolidado[posRevCOPOutlier,2]
			TipoRev <- Consolidado[posRevCOPOutlier,3]
		} else{
			MetodoRev <- Consolidado[posRevCOPReal,2]
			TipoRev <- Consolidado[posRevCOPReal,3]
		}		
	} else if (is.na(posRevCOPReal) & is.na(posRevCOPOutlier) & !is.na(posRevUSDReal) & !is.na(posRevUSDOutlier)){
		if(Consolidado[posRevUSDReal,5] < Consolidado[posRevUSDOutlier,5]){
			MetodoRev <- Consolidado[posRevUSDReal,2]
			TipoRev <- Consolidado[posRevUSDReal,3]
		} else if(Consolidado[posRevUSDReal,5] > Consolidado[posRevUSDOutlier,5]){
			MetodoRev <- Consolidado[posRevUSDOutlier,2]
			TipoRev <- Consolidado[posRevUSDOutlier,3]
		} else{
			MetodoRev <- Consolidado[posRevUSDReal,2]
			TipoRev <- Consolidado[posRevUSDReal,3]
		}		
	} else if(is.na(posRevUSDReal) & is.na(posRevUSDOutlier) & !is.na(posRevCOPReal) & is.na(posRevCOPOutlier)){
		MetodoRev <- Consolidado[posRevCOPReal,2]
		TipoRev <- Consolidado[posRevCOPReal,3]
	} else if(is.na(posRevUSDReal) & is.na(posRevUSDOutlier) & is.na(posRevCOPReal) & !is.na(posRevCOPOutlier)){
		MetodoRev <- Consolidado[posRevCOPOutlier,2]
		TipoRev <- Consolidado[posRevCOPOutlier,3]
	} else if(is.na(posRevCOPReal) & is.na(posRevCOPOutlier) & !is.na(posRevUSDReal) & is.na(posRevUSDOutlier)){
		MetodoRev <- Consolidado[posRevUSDReal,2]
		TipoRev <- Consolidado[posRevUSDReal,3]	
	} else if(is.na(posRevCOPReal) & is.na(posRevCOPOutlier) & is.na(posRevUSDReal) & !is.na(posRevUSDOutlier)){
		MetodoRev <- Consolidado[posRevUSDOutlier,2]
		TipoRev <- Consolidado[posRevUSDOutlier,3]	
	}
	
	divisionRev <- as.data.frame(strsplit(as.character(TipoRev),"_"))[2,1]
	Ruta_Rev <- paste("C:/Users/cdavalos/Documents/Pronostico TM/R/Prueba/Datos Data Splitting/",MetodoRev,"/Datos_",TipoRev,".csv",sep = "")
	InputRev <- read.table(toString(Ruta_Rev), sep=" ", header=TRUE, fileEncoding="latin1")
	colnames(InputRev) <- c("PRevUno","PRevDos","PRevTres","LEG_OW")
	
	if(TipoRev == "REV_COP_Outliers"){
		Ruta_Rev_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_COP_Outliers.csv"
	} else if(TipoRev == "REV_COP_Real"){
		Ruta_Rev_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_COP_Real.csv"
	} else if(TipoRev == "REV_USD_Real"){
		Ruta_Rev_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_USD_Real.csv"
	} else 	Ruta_Rev_Input <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/REV_USD_Outliers.csv"
	InputRev_Input <- read.table(Ruta_Rev_Input, sep=" ", header=TRUE, fileEncoding="latin1")
	
	
	pronosticosPax <- subset(InputPax,LEG_OW == as.vector(Rutas[i]))
	pronosticosRev <- subset(InputRev,LEG_OW == as.vector(Rutas[i]))
	
	agregadoDatosTM <- data.frame()

	for(j in 1:4){
		for(g in 1:3){
			agregadoDatosTM[j,g] <- pronosticosRev[j,g]/pronosticosPax[j,g]
		}	
	agregadoDatosTM[j,4] <- Rutas[i]
	}
	
	AGREGADOTM <- rbind2(AGREGADOTM,agregadoDatosTM)
	
	posPax_Input <- match(Rutas[i],colnames(InputPax_Input))
	posRev_Input <- match(Rutas[i],colnames(InputRev_Input))
	
	Input_Pax <- as.data.frame(InputPax_Input[posPax_Input][-c(1:(dim(InputPax_Input[posPax_Input])[1]-forecast_periods-iterations+1)),])
	rownames(Input_Pax) <- as.data.frame(rownames(InputPax_Input))[-c(1:(length(rownames(InputPax_Input))-forecast_periods-iterations+1)),]
	colnames(Input_Pax) <- Rutas[i]
	
	Input_Rev <- as.data.frame(InputRev_Input[posRev_Input][-c(1:(dim(InputRev_Input[posRev_Input])[1]-forecast_periods-iterations+1)),])
	rownames(Input_Rev) <- as.data.frame(rownames(InputRev_Input))[-c(1:(length(rownames(InputRev_Input))-forecast_periods-iterations+1)),]
	colnames(Input_Rev) <- Rutas[i]
	
	if(i == 1){
	INPUTPAXC <- rbind(INPUTPAXC,Input_Pax)
	INPUTREVC <- rbind(INPUTREVC,Input_Rev)
	} else {
	INPUTPAXC <- cbind(INPUTPAXC,Input_Pax)
	INPUTREVC <- cbind(INPUTREVC,Input_Rev)
	}

	
}

for(i in 1:cantRutas){
	leg <- Rutas[i]
	posRuta <- match(leg,rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]
	
	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))
	
	serieTM <- INPUTREVC[match(leg,colnames(INPUTREVC))]/INPUTPAXC[match(leg,colnames(INPUTPAXC))]
	añoInicioInput <- as.data.frame(strsplit(as.character(rownames(serieTM)[1]),"-"))[1,1]
	mesInicioInput <- as.data.frame(strsplit(as.character(rownames(serieTM)[1]),"-"))[2,1]
	añoFinInput <- as.data.frame(strsplit(as.character(rownames(serieTM)[forecast_periods+iterations-1]),"-"))[1,1]
	mesFinInput <- as.data.frame(strsplit(as.character(rownames(serieTM)[forecast_periods+iterations-1]),"-"))[2,1]
	x <- ts(serieTM, frequency = 12, start = c(as.numeric(as.character(añoInicioInput)),as.numeric(as.character(mesInicioInput))), end= c(as.numeric(as.character(añoFinInput)),as.numeric(as.character(mesFinInput))))
	
	mape <- double()
	me <- double()
	rmse <- double()
	mase <- double()
	mae <- double()

	
	for(g in 1:j){

		test.start <- time(x)[1]
		test <- window(x,start=test.start)
		fit <- as.ts(t(subset(AGREGADOTM,V4 == as.vector(leg))[g,][,-4]))
   
		YYinicial <- start(test)[1]
		MMinicial <- start(test)[2]
   
		a <- ts(fit,start=c(YYinicial,MMinicial), frequency = 12)
		
		YYFinal<-start(a)[1] 
		MMFinal<-start(a)[2]

			if (MMFinal + j-2> 12){
				fit_ts <- ts( a, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
				test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal+1, MMFinal + j -2-12))
			} else {
				fit_ts <- ts( a, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
				test_ts <- ts( test, frequency = 12, start = c(YYFinal, MMFinal), end= c(YYFinal, MMFinal+j-2))
			}
  
		mape <- c(mape, accuracy(fit_ts,test_ts)[1,5])
		me <- c(me, accuracy(fit_ts,test_ts)[1,1])
		rmse <- c(rmse, accuracy(fit_ts,test_ts)[1,2])
		mae <- c(mae, accuracy(fit_ts,test_ts)[1,3])
	}
	RESULTADO[i,1] <- leg
	RESULTADO[i,2] <- mean(me)
	RESULTADO[i,3] <- mean(rmse)
	RESULTADO[i,4] <- mean(mae)
	RESULTADO[i,5] <- mean(mape)
}

