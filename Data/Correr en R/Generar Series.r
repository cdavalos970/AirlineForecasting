library(seasonal)
library(zoo)
options(warn=-1)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Input")
#Lee el archivo
InputFile <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Consolidado Fixed.txt"	
RAW_DATA <- read.table(InputFile, sep=" ", header=TRUE, fileEncoding="latin1")
Input_MINMAX <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"
MINMAX <- read.table(Input_MINMAX, sep=" ", header=TRUE, fileEncoding="latin1")
Input_REGIONES <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Regiones.txt"
REGIONES <- read.table(Input_REGIONES, sep=";", header=TRUE, fileEncoding="latin1")

Input_RUTASPUROS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Rutas Puros.txt"
RUTAS_PUROS <- read.table(Input_RUTASPUROS, sep=";", header=TRUE, fileEncoding="latin1")


RAW_DATA_SORT <- subset(RAW_DATA, RAW_DATA$LEG_OW %in% rownames(MINMAX))


legs <- unique(RAW_DATA_SORT$LEG_OW)
cantLegs <- length(legs)

matriz_pax <- data.frame()
matriz_rev_usd <- data.frame()
matriz_rev_cop <- data.frame()

data_pax <- data.frame()
data_rev_usd <- data.frame()
data_rev_cop <- data.frame()

matriz_cant_datos <- data.frame(RUTA = character(), MIN = character(), MAX = character(), stringsAsFactors=F)

dataOutput_pax <- data.frame()
dataOutput_rev_usd <- data.frame()
dataOutput_rev_cop <- data.frame()

COP <- data.frame(RUTA = character())
USD <- data.frame(RUTA = character())

ref <- data.frame(FECHAS = seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1")))

	ntrpax <- c(25,75,83,107)
	ntrusd <- c(73,75,170)
	ntrcop <- c(175)

for(g in 1:cantLegs) {
	moneda <- REGIONES[match(legs[g],REGIONES$RUTA),3]
	
	seriepax <- subset(RAW_DATA,LEG_OW == legs[g])$PAX 
	if(moneda == "USD") serierev_usd <- subset(RAW_DATA,LEG_OW == legs[g])$REV_USD
	if(moneda == "USD") RUTA_USD <- data.frame(RUTA = legs[g])
	if(moneda == "USD") USD <- rbind(USD,RUTA_USD)
	if(moneda == "COP") serierev_cop <- subset(RAW_DATA,LEG_OW == legs[g])$REV_COP
	if(moneda == "COP") RUTA_COP <- data.frame(RUTA = legs[g])
	if(moneda == "COP") COP <- rbind(COP,RUTA_COP)

	cantSeriepax <- length(seriepax)

	matriz_pax <- rbind(matriz_pax,seriepax)
	if(moneda == "USD") matriz_rev_usd <- rbind(matriz_rev_usd,serierev_usd)
	if(moneda == "COP") matriz_rev_cop <- rbind(matriz_rev_cop,serierev_cop)

	matriz_cant_datos <- rbind(matriz_cant_datos,data.frame(RUTA = legs[g], MIN = 1, MAX = cantSeriepax, stringsAsFactors=F))
}  
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier")

data_pax <- as.data.frame(t(matriz_pax))
data_rev_usd <- as.data.frame(t(matriz_rev_usd))
data_rev_cop <- as.data.frame(t(matriz_rev_cop))

colnames(data_pax) <- legs
colnames(data_rev_usd) <- as.factor(USD$RUTA)
colnames(data_rev_cop) <- as.factor(COP$RUTA)

rownames(data_pax) <- seq(as.Date("2008/1/1"),by = "month",to = as.Date("2015/1/1"))
rownames(data_rev_usd) <- seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1"))
rownames(data_rev_cop) <- seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1"))

for(g in 1:cantLegs){

	añoInicio <- MINMAX[g,1]
	mesInicio <- MINMAX[g,3]
	añoFin <- MINMAX[g,2]
	mesFin <- MINMAX[g,4]

	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	pos <- match(inicio,ref$FECHA)

	seriepaxesp <- data_pax[g][which(rownames(data_pax[g])>=inicio & rownames(data_pax[g]) <= fin),]

	Data_ts_pax <- ts(seriepaxesp, frequency = 12, start = c(añoInicio, mesInicio), end= c(añoFin, mesFin))
	
	if(!(g %in% ntrpax)) x13_model_Pax_Outlier <- seas(Data_ts_pax,na.action = na.x13)

	if(!(g %in% ntrpax)) Correccion_Pax <- series(x13_model_Pax_Outlier,"b1")

	
	for(i in 1:length(seriepaxesp)){
		if(!(g %in% ntrpax))dataOutput_pax[pos+i-1,g] <- Correccion_Pax[i]
	}
	for(i in 1:85){
		if((g %in% ntrpax))dataOutput_pax[i,g] <- 0
	}
	
}

for(g in 1:length(as.factor(USD$RUTA))){


	posRuta <- match(USD$RUTA[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]

	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	pos <- match(inicio,ref$FECHA)
	
	data_rev_usdesp <- data_rev_usd[g][which(rownames(data_rev_usd[g])>=inicio & rownames(data_rev_usd[g]) <= fin),]
	Data_ts_rev_usd <- ts(data_rev_usdesp, frequency = 12, start = c(añoInicio, mesInicio), end= c(añoFin, mesFin))
	if(!(posRuta %in% ntrusd)) x13_model_Rev_USD_Outlier <- seas(Data_ts_rev_usd,na.action = na.x13)
	if(!(posRuta %in% ntrusd)) Correccion_Rev_USD <- series(x13_model_Rev_USD_Outlier,"b1")
	
	for(i in 1:length(data_rev_usdesp)){
		if(!(posRuta %in% ntrusd))dataOutput_rev_usd[pos+i-1,g] <- Correccion_Rev_USD[i]
	}
	for(i in 1:85){
		if((posRuta %in% ntrusd))dataOutput_rev_usd[i,g] <- 0
	}
}

for(g in 1:length(as.factor(COP$RUTA))){

	posRuta <- match(COP$RUTA[g],rownames(MINMAX))
	añoInicio <- MINMAX[posRuta,1]
	mesInicio <- MINMAX[posRuta,3]
	añoFin <- MINMAX[posRuta,2]
	mesFin <- MINMAX[posRuta,4]

	inicio <- as.Date(paste(añoInicio,mesInicio,"1",sep ="/"))
	fin <- as.Date(paste(añoFin,mesFin,"1",sep ="/"))

	pos <- match(inicio,ref$FECHA)
	
	data_rev_copesp <- data_rev_cop[g][which(rownames(data_rev_cop[g])>=inicio & rownames(data_rev_cop[g]) <= fin),]
	Data_ts_rev_cop <- ts(data_rev_copesp, frequency = 12, start = c(añoInicio, mesInicio), end= c(añoFin, mesFin))
	if(!(posRuta %in% ntrcop)) x13_model_Rev_COP_Outlier <- seas(Data_ts_rev_cop)
	if(!(posRuta %in% ntrcop)) Correccion_Rev_COP <- series(x13_model_Rev_COP_Outlier,"b1")
	
	for(i in 1:length(data_rev_copesp)){
		if(!(posRuta %in% ntrcop))dataOutput_rev_cop[pos+i-1,g] <- Correccion_Rev_COP[i]
	}
	for(i in 1:85){
		if((posRuta %in% ntrcop))dataOutput_rev_cop[i,g] <- 0
	}
}

colnames(dataOutput_pax) <- legs
colnames(dataOutput_rev_usd) <- as.factor(USD$RUTA)
colnames(dataOutput_rev_cop) <- as.factor(COP$RUTA)

rownames(dataOutput_pax) <- seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1"))
rownames(dataOutput_rev_usd) <- seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1"))
rownames(dataOutput_rev_cop) <- seq(as.Date("2008/1/1"),by = "month", to = as.Date("2015/1/1"))

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier")

write.table(file="PAX_Real.csv",data_pax)
write.table(file="REV_USD_Real.csv",data_rev_usd)
write.table(file="REV_COP_Real.csv",data_rev_cop)

write.table(file="PAX_Outliers.csv",dataOutput_pax)
write.table(file="REV_USD_Outliers.csv",dataOutput_rev_usd)
write.table(file="REV_COP_Outliers.csv",dataOutput_rev_cop)

legsNoIncluidos <- as.data.frame(RUTAS_PUROS[is.na(sapply(RUTAS_PUROS,legs,FUN = match))])

write.table(file="Legs No Pronosticados.csv",legsNoIncluidos)


