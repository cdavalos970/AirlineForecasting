library(forecast)
library(seasonal)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Theta")

Input_Clasificacion <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Clasificacion.csv"

Input_Pax_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Real.csv"
Input_Rev_USD_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Real.csv"
Input_Rev_COP_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Real.csv"

Input_Pax_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Outliers.csv"
Input_Rev_USD_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Outliers.csv"
Input_Rev_COP_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Outliers.csv"

matriz_Pax_Real_X13 <- data.frame()

Clasificacion <- read.table(Input_Clasificacion, sep=",", header=TRUE, fileEncoding="latin1") 

Pax_Real <- read.table(Input_Pax_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Real <- read.table(Input_Rev_USD_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_COP_Real <- read.table(Input_Rev_COP_Real, sep=" ", header=TRUE, fileEncoding="latin1") 

Pax_Outliers <- read.table(Input_Pax_Outliers, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Outliers<- read.table(Input_Rev_USD_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")
Rev_COP_Outliers<- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")

matriz_Pax_Real <- data.frame()
matriz_Rev_USD_Real <- data.frame()
matriz_Rev_COP_Real <- data.frame()

matriz_Pax_Outlier <- data.frame()
matriz_Rev_USD_Outlier <- data.frame()
matriz_Rev_COP_Outlier <- data.frame()

theta_forecast_Pax_Real <- data.frame()
theta_forecast_USD_Real <- data.frame()
theta_forecast_COP_Real <- data.frame()

theta_forecast_Pax_Outlier <- data.frame()
theta_forecast_Rev_USD_Outlier <- data.frame()
theta_forecast_Rev_COP_Outlier <- data.frame()

Pax_Real_Leg <- data.frame()
Pax_Outlier_Leg <- data.frame()
REV_USD_Real_Leg <- data.frame()
REV_USD_Outliers_Leg <- data.frame()
Rev_COP_Real_Leg <- data.frame()
Rev_COP_Outliers_Leg <- data.frame()

forecast_periods <- 3
confidence_level <- 95
periods <- c(1,2,3)

for(i in 1:dim(Clasificacion)[1]){
x <- "Theta"
y <- Clasificacion$METODO[i]
if(y == x){
posLeg <- match(Clasificacion$LEG_OW[i],colnames(Pax_Real))
cantDatos <- nrow(Pax_Real[posLeg])

	if(Clasificacion$TIPODATO[i] == "Pax_Real"){
	Pax_Real_Leg <- c(Pax_Real_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Pax_Real)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Pax_Real)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Pax_Real)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Pax_Real)[cantDatos]),format = "%m"))
	
	ts_Pax_Real <- ts(Pax_Real[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Pax_Real <- thetaf(ts_Pax_Real)
	matriz_Pax_Real <- rbind(matriz_Pax_Real,as.data.frame(forecast(theta_Pax_Real))[periods,1])
		
	} else if(Clasificacion$TIPODATO[i] == "Pax_Outlier"){
	Pax_Outlier_Leg <- c(Pax_Outlier_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Pax_Outliers)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Pax_Outliers)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Pax_Outliers)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Pax_Outliers)[cantDatos]),format = "%m"))
	
	ts_Pax_Outlier <- ts(Pax_Outliers[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Pax_Outlier <- thetaf(ts_Pax_Outlier)
	matriz_Pax_Outlier <- rbind(matriz_Pax_Outlier,as.data.frame(forecast(theta_Pax_Outlier))[periods,1])

	} else if(Clasificacion$TIPODATO[i] == "REV_USD_Real"){
	REV_USD_Real_Leg <- c(REV_USD_Real_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Rev_USD_Real)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Rev_USD_Real)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Rev_USD_Real)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Rev_USD_Real)[cantDatos]),format = "%m"))
	
	ts_Rev_USD_Real <- ts(Rev_USD_Real[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Rev_USD_Real <- thetaf(ts_Rev_USD_Real)
	matriz_Rev_USD_Real <- rbind(matriz_Rev_USD_Real,as.data.frame(forecast(theta_Rev_USD_Real))[periods,1])
	
	}else if(Clasificacion$TIPODATO[i] == "REV_USD_Outliers"){
	REV_USD_Outliers_Leg <- c(REV_USD_Outliers_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Rev_USD_Outliers)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Rev_USD_Outliers)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Rev_USD_Outliers)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Rev_USD_Outliers)[cantDatos]),format = "%m"))
	
	ts_Rev_USD_Outlier <- ts(Rev_USD_Outliers[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Rev_USD_Outlier <- thetaf(ts_Rev_USD_Outlier)
	matriz_Rev_USD_Outlier <- rbind(matriz_Rev_USD_Outlier,as.data.frame(forecast(theta_Rev_USD_Outlier))[periods,1])
	
	} else if(Clasificacion$TIPODATO[i] == "REV_COP_Real"){
	Rev_COP_Real_Leg <- c(Rev_COP_Real_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Rev_COP_Real)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Rev_COP_Real)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Rev_COP_Real)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Rev_COP_Real)[cantDatos]),format = "%m"))
	
	ts_Rev_COP_Real <- ts(Rev_COP_Real[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Rev_COP_Real <- thetaf(ts_Rev_COP_Real)
	matriz_Rev_COP_Real <- rbind(matriz_Rev_COP_Real,as.data.frame(forecast(theta_Rev_COP_Real))[periods,1])
	} else {
	Rev_COP_Outliers_Leg <- c(Rev_COP_Outliers_Leg,toString(Clasificacion$LEG_OW[i]))
	a <- as.integer(format(as.Date(rownames(Rev_COP_Outliers)[1]),format = "%Y"))
	b <- as.integer(format(as.Date(rownames(Rev_COP_Outliers)[1]),format = "%m"))
	c <- as.integer(format(as.Date(rownames(Rev_COP_Outliers)[cantDatos]),format = "%Y"))
	d <- as.integer(format(as.Date(rownames(Rev_COP_Outliers)[cantDatos]),format = "%m"))
	
	ts_Rev_COP_Outlier <- ts(Rev_COP_Outliers[posLeg] , frequency = 12, start = c(a,b), end= c(c,d))
	theta_Rev_COP_Outlier <- thetaf(ts_Rev_COP_Outlier)
	matriz_Rev_COP_Outlier <- rbind(matriz_Rev_COP_Outlier,as.data.frame(forecast(theta_Rev_COP_Outlier))[periods,1])
	
	
	}
	}
}

theta_forecast_Pax_Real <- t(matriz_Pax_Real)
theta_forecast_USD_Real <- t(matriz_Rev_USD_Real)
theta_forecast_COP_Real <- t(matriz_Rev_COP_Real)

theta_forecast_Pax_Outlier <- t(matriz_Pax_Outlier)
theta_forecast_Rev_USD_Outlier <- t(matriz_Rev_USD_Outlier)
theta_forecast_Rev_COP_Outlier <- t(matriz_Rev_COP_Outlier)

colnames(theta_forecast_Pax_Real) <- as.character(Pax_Real_Leg)
colnames(theta_forecast_USD_Real) <- as.character(REV_USD_Real_Leg)
colnames(theta_forecast_COP_Real) <- as.character(Rev_COP_Real_Leg)

colnames(theta_forecast_Pax_Outlier) <- as.character(Pax_Outlier_Leg)
colnames(theta_forecast_Rev_USD_Outlier) <- as.character(REV_USD_Outliers_Leg)
colnames(theta_forecast_Rev_COP_Outlier) <- as.character(Rev_COP_Outliers_Leg)

rownames(theta_forecast_Pax_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(theta_forecast_USD_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(theta_forecast_COP_Real) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

rownames(theta_forecast_Pax_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(theta_forecast_Rev_USD_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)
rownames(theta_forecast_Rev_COP_Outlier) <- seq(as.Date("2015/1/1"),by = "month", length.out = forecast_periods)

write.table(file="PAX_Real.csv",theta_forecast_Pax_Real)
write.table(file="REV_USD_Real.csv",theta_forecast_USD_Real)
write.table(file="REV_COP_Real.csv",theta_forecast_COP_Real)

write.table(file="PAX_Outliers.csv",theta_forecast_Pax_Outlier)
write.table(file="REV_USD_Outliers.csv",theta_forecast_Rev_USD_Outlier)
write.table(file="REV_COP_Outliers.csv",theta_forecast_Rev_COP_Outlier)