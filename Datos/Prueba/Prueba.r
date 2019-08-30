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

Prueba_Pax_Real <- data.frame()
Prueba_Pax_Real <- rbind(Prueba_Pax_Real,as.data.frame(rownames(Pax_Real)))

niPaxReal <- vector()

for(i in 1:length(colnames(Pax_Real))){
if(!any(is.na(Pax_Real[i])) & !any(Pax_Real[i]==0)) Prueba_Pax_Real <- cbind(Prueba_Pax_Real,Pax_Real[i])
else niPaxReal <- c(niPaxReal,colnames(Pax_Real[i]))
}

Prueba_USD_Real <- data.frame()
Prueba_USD_Real <- rbind(Prueba_USD_Real,as.data.frame(rownames(Rev_USD_Real)))

niUSD_Real <- vector()

for(i in 1:length(colnames(Rev_USD_Real))){
if(!any(is.na(Rev_USD_Real[i])) & !any(Rev_USD_Real[i]==0)) Prueba_USD_Real <- cbind(Prueba_USD_Real,Rev_USD_Real[i])
else niUSD_Real <- c(niUSD_Real,colnames(Rev_USD_Real[i]))
}


Prueba_COP_Real <- data.frame()
Prueba_COP_Real <- rbind(Prueba_COP_Real,as.data.frame(rownames(Rev_COP_Real)))

niCOP_Real <- vector()

for(i in 1:length(colnames(Rev_COP_Real))){
if(!any(is.na(Rev_COP_Real[i])) & !any(Rev_COP_Real[i]==0)) Prueba_COP_Real <- cbind(Prueba_COP_Real,Rev_COP_Real[i])
else niCOP_Real <- c(niCOP_Real,colnames(Rev_COP_Real[i]))
}

Prueba_Pax_Outliers <- data.frame()
Prueba_Pax_Outliers <- rbind(Prueba_Pax_Outliers,as.data.frame(rownames(Pax_Outliers)))

niPax_Outliers <- vector()

for(i in 1:length(colnames(Pax_Outliers))){
if(!any(is.na(Pax_Outliers[i])) & !any(Pax_Outliers[i]==0)) Prueba_Pax_Outliers <- cbind(Prueba_Pax_Outliers,Pax_Outliers[i])
else niPax_Outliers <- c(niPax_Outliers,colnames(Pax_Outliers[i]))
}

Prueba_USD_Outliers <- data.frame()
Prueba_USD_Outliers <- rbind(Prueba_USD_Outliers,as.data.frame(rownames(Rev_USD_Outliers)))

niUSD_Outliers <- vector()

for(i in 1:length(colnames(Rev_USD_Outliers))){
if(!any(is.na(Rev_USD_Outliers[i])) & !any(Rev_USD_Outliers[i]==0)) Prueba_USD_Outliers <- cbind(Prueba_USD_Outliers,Rev_USD_Outliers[i])
else niUSD_Outliers <- c(niUSD_Outliers,colnames(Rev_USD_Outliers[i]))
}

Prueba_COP_Outliers <- data.frame()
Prueba_COP_Outliers <- rbind(Prueba_COP_Outliers,as.data.frame(rownames(Rev_COP_Outliers)))

niCOP_Outliers <- vector()

for(i in 1:length(colnames(Rev_COP_Outliers))){
if(!any(is.na(Rev_COP_Outliers[i])) & !any(Rev_COP_Outliers[i]==0)) Prueba_COP_Outliers <- cbind(Prueba_COP_Outliers,Rev_COP_Outliers[i])
else niCOP_Outliers <- c(niCOP_Outliers,colnames(Rev_COP_Outliers[i]))
}

	rownames(Prueba_Pax_Real) <- (Prueba_Pax_Real[,1])
	Prueba_Pax_Real <- Prueba_Pax_Real[,-1]

	rownames(Prueba_USD_Real) <- (Prueba_USD_Real[,1])
	Prueba_USD_Real <- Prueba_USD_Real[,-1]

	rownames(Prueba_COP_Real) <- (Prueba_COP_Real[,1])
	Prueba_COP_Real <- Prueba_COP_Real[,-1]

	rownames(Prueba_Pax_Outliers) <- (Prueba_Pax_Outliers[,1])
	Prueba_Pax_Outliers <- Prueba_Pax_Outliers[,-1]

	rownames(Prueba_USD_Outliers) <- (Prueba_USD_Outliers[,1])
	Prueba_USD_Outliers <- Prueba_USD_Outliers[,-1]

	rownames(Prueba_COP_Outliers) <- (Prueba_COP_Outliers[,1])
	Prueba_COP_Outliers <- Prueba_COP_Outliers[,-1]

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba")

write.table(file="PAX_Real.csv",Prueba_Pax_Real)
write.table(file="REV_USD_Real.csv",Prueba_USD_Real)
write.table(file="REV_COP_Real.csv",Prueba_COP_Real)

write.table(file="PAX_Outliers.csv",Prueba_Pax_Outliers)
write.table(file="REV_USD_Outliers.csv",Prueba_USD_Outliers)
write.table(file="REV_COP_Outliers.csv",Prueba_COP_Outliers)

write.table(file="No Incluidos Pax Real.txt",niPaxReal)
write.table(file="No Incluidos USD Real.txt",niUSD_Real)
write.table(file="No Incluidos COP Real.txt",niCOP_Real)
write.table(file="No Incluidos Pax Outliers.txt",niPax_Outliers)
write.table(file="No Incluidos USD Outliers.txt",niUSD_Outliers)
write.table(file="No Incluidos COP Outliers.txt",niCOP_Outliers)
