PRX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/PAX_Real.csv"	
POX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/PAX_Outliers.csv"	
RURX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/REV_USD_Real.csv"	
RUOX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/REV_USD_Outliers.csv"	
RCRX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/REV_COP_Real.csv"	
RCOX13 <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/X13/REV_COP_Outliers.csv"

PRA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/PAX_Real.csv"	
POA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/PAX_Outliers.csv"	
RURA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/REV_USD_Real.csv"	
RUOA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/REV_USD_Outliers.csv"	
RCRA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/REV_COP_Real.csv"	
RCOA <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima/REV_COP_Outliers.csv"

PRHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/PAX_Real.csv"	
POHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/PAX_Outliers.csv"	
RURHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/REV_USD_Real.csv"	
RUOHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/REV_USD_Outliers.csv"	
RCRHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/REV_COP_Real.csv"	
RCOHW <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/HW/REV_COP_Outliers.csv"

PRSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/PAX_Real.csv"	
POSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/PAX_Outliers.csv"	
RURSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/REV_USD_Real.csv"	
RUOSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/REV_USD_Outliers.csv"	
RCRSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/REV_COP_Real.csv"	
RCOSTS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/STS/REV_COP_Outliers.csv"

PRETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/PAX_Real.csv"	
POETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/PAX_Outliers.csv"	
RURETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/REV_USD_Real.csv"	
RUOETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/REV_USD_Outliers.csv"	
RCRETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/REV_COP_Real.csv"	
RCOETS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/ETS/REV_COP_Outliers.csv"

PRDT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/PAX_Real.csv"	
PODT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/PAX_Outliers.csv"	
RURDT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/REV_USD_Real.csv"	
RUODT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/REV_USD_Outliers.csv"	
RCRDT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/REV_COP_Real.csv"	
RCODT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Damped Trend/REV_COP_Outliers.csv"

PRT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/PAX_Real.csv"	
POT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/PAX_Outliers.csv"	
RURT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/REV_USD_Real.csv"	
RUOT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/REV_USD_Outliers.csv"	
RCRT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/REV_COP_Real.csv"	
RCOT <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta/REV_COP_Outliers.csv"

Rutas <- data.frame(RUTA = character(), METODO = character(), stringsAsFactors=F)

Rutas <- rbind(Rutas,data.frame(RUTA = PRX13, METODO = "X13", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POX13, METODO = "X13", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURX13, METODO = "X13", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOX13, METODO = "X13", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRX13, METODO = "X13", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOX13, METODO = "X13", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRA, METODO = "Arima", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POA, METODO = "Arima", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURA, METODO = "Arima", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOA, METODO = "Arima", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRA, METODO = "Arima", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOA, METODO = "Arima", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRHW, METODO = "HW", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POHW, METODO = "HW", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURHW, METODO = "HW", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOHW, METODO = "HW", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRHW, METODO = "HW", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOHW, METODO = "HW", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRSTS, METODO = "STS", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POSTS, METODO = "STS", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURSTS, METODO = "STS", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOSTS, METODO = "STS", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRSTS, METODO = "STS", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOSTS, METODO = "STS", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRETS, METODO = "ETS", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POETS, METODO = "ETS", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURETS, METODO = "ETS", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOETS, METODO = "ETS", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRETS, METODO = "ETS", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOETS, METODO = "ETS", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRDT, METODO = "Damped Trend", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = PODT, METODO = "Damped Trend", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURDT, METODO = "Damped Trend", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUODT, METODO = "Damped Trend", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRDT, METODO = "Damped Trend", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCODT, METODO = "Damped Trend", DATO = "REV_COP_Outliers"))

Rutas <- rbind(Rutas,data.frame(RUTA = PRT, METODO = "Theta", DATO = "Pax_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = POT, METODO = "Theta", DATO = "Pax_Outlier")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RURT, METODO = "Theta", DATO = "REV_USD_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RUOT, METODO = "Theta", DATO = "REV_USD_Outliers"))
Rutas <- rbind(Rutas,data.frame(RUTA = RCRT, METODO = "Theta", DATO = "REV_COP_Real")) 
Rutas <- rbind(Rutas,data.frame(RUTA = RCOT, METODO = "Theta", DATO = "REV_COP_Outliers"))

cantRutas <- nrow(Rutas)

Agregado <- data.frame(LEG_OW = character(), METODO = character(),TIPODATO = character(), FECHA1 = character(), FECHA2 = character(), FECHA3 = character(), stringsAsFactors=F)

for (z in 1:cantRutas){
	
	if(file.info(toString(Rutas[z,1]))$size > 0){
		InputData <- t(read.table(toString(Rutas[z,1]), sep=" ", header=TRUE, fileEncoding="latin1"))
		cantFilas <- nrow(InputData)
	
		for(j in 1:cantFilas){
	
			Vector <- data.frame(LEG_OW = rownames(InputData)[j], METODO = toString(Rutas[z,2]), TIPODATO = toString(Rutas[z,3]), FECHA1 = InputData[j,1], FECHA2 = InputData[j,2],FECHA3 = InputData[j,3])
			Agregado <- rbind(Agregado, Vector)
		
		}
	}
}	

colnames(Agregado)[4] <- colnames(InputData)[1]
colnames(Agregado)[5] <- colnames(InputData)[2]
colnames(Agregado)[6] <- colnames(InputData)[3]

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R")

write.table(file="Pronostico_Consolidado.csv",Agregado, sep = ",")


