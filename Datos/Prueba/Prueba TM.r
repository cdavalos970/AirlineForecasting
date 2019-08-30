library(forecast)
setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Pronosticos/Arima")
#Lee el archivo

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
Rev_COP_Outliers <- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")

MINMAX <- read.table(Input_MINMAX, sep=" ", header=TRUE, fileEncoding="latin1")

legsPaxReal <- colnames(Pax_Real)
legsUSDReal <- colnames(Rev_USD_Real)
legsCOPReal <- colnames(Rev_COP_Real)
legsPaxOutliers <- colnames(Pax_Outliers)
legsUSDOutliers <- colnames(Rev_USD_Outliers)
legsCOPOutliers <- colnames(Rev_COP_Outliers)

PaxRealvsUSDReal <- Pax_Real[legsPaxReal %in% legsUSDReal]
USDRealvsPaxReal <- Rev_USD_Real[legsUSDReal %in% legsPaxReal]
TMPAXREALvsUSDREAL <- data.frame()

for(i in 1:dim(PaxRealvsUSDReal)[2]){
posicion <- match(colnames(PaxRealvsUSDReal)[i],colnames(USDRealvsPaxReal))
serie <- USDRealvsPaxReal[posicion]/PaxRealvsUSDReal[i]
if(i == 1){
TMPAXREALvsUSDREAL <- rbind(TMPAXREALvsUSDREAL,serie)
} else TMPAXREALvsUSDREAL <- cbind(TMPAXREALvsUSDREAL,serie)
}

PaxRealvsCOPReal <- Pax_Real[legsPaxReal %in% legsCOPReal]
COPRealvsPaxReal <- Rev_COP_Real[legsCOPReal %in% legsPaxReal]
TMPAXREALvsCOPREAL <- data.frame()

for(i in 1:dim(PaxRealvsCOPReal)[2]){
posicion <- match(colnames(PaxRealvsCOPReal)[i],colnames(COPRealvsPaxReal))
serie <- COPRealvsPaxReal[posicion]/PaxRealvsCOPReal[i]
if(i == 1){
TMPAXREALvsCOPREAL <- rbind(TMPAXREALvsCOPREAL,serie)
} else TMPAXREALvsCOPREAL <- cbind(TMPAXREALvsCOPREAL,serie)
}

TMPAXREALvsREVREAL <- cbind2(TMPAXREALvsUSDREAL,TMPAXREALvsCOPREAL)

PaxOutliervsUSDOutlier <- Pax_Outliers[legsPaxOutliers %in% legsUSDOutliers]
USDOutliervsPaxOutlier <- Rev_USD_Outliers[legsUSDOutliers %in% legsPaxOutliers]
TMPAXOUTLIERvsUSDOUTLIER <- data.frame()

for(i in 1:dim(PaxOutliervsUSDOutlier)[2]){
posicion <- match(colnames(PaxOutliervsUSDOutlier)[i],colnames(USDOutliervsPaxOutlier))
serie <- USDOutliervsPaxOutlier[posicion]/PaxOutliervsUSDOutlier[i]
if(i == 1){
TMPAXOUTLIERvsUSDOUTLIER <- rbind(TMPAXOUTLIERvsUSDOUTLIER,serie)
} else TMPAXOUTLIERvsUSDOUTLIER <- cbind(TMPAXOUTLIERvsUSDOUTLIER,serie)
}

PaxOutliervsCOPOutlier <- Pax_Outliers[legsPaxOutliers %in% legsCOPOutliers]
COPOutliervsPaxOutlier <- Rev_COP_Outliers[legsCOPOutliers %in% legsPaxOutliers]
TMPAXOUTLIERvsCOPOUTLIER <- data.frame()

for(i in 1:dim(PaxOutliervsCOPOutlier)[2]){
posicion <- match(colnames(PaxOutliervsCOPOutlier)[i],colnames(COPOutliervsPaxOutlier))
serie <- COPOutliervsPaxOutlier[posicion]/PaxOutliervsCOPOutlier[i]
if(i == 1){
TMPAXOUTLIERvsCOPOUTLIER <- rbind(TMPAXOUTLIERvsCOPOUTLIER,serie)
} else TMPAXOUTLIERvsCOPOUTLIER <- cbind(TMPAXOUTLIERvsCOPOUTLIER,serie)
}

TMPAXOvsREVOUTLIER <- cbind2(TMPAXOUTLIERvsUSDOUTLIER,TMPAXOUTLIERvsCOPOUTLIER)

PaxRealvsUSDOutlier <- Pax_Real[legsPaxReal %in% legsUSDOutliers]
USDOutliervsPaxReal <- Rev_USD_Outliers[legsUSDOutliers %in% legsPaxReal]
TMPAXREALvsUSDOUTLIER<- data.frame()

for(i in 1:dim(PaxRealvsUSDOutlier)[2]){
posicion <- match(colnames(PaxRealvsUSDOutlier)[i],colnames(USDOutliervsPaxReal))
serie <- USDOutliervsPaxReal[posicion]/PaxRealvsUSDOutlier[i]
if(i == 1){
TMPAXREALvsUSDOUTLIER <- rbind(TMPAXREALvsUSDOUTLIER,serie)
} else TMPAXREALvsUSDOUTLIER <- cbind(TMPAXREALvsUSDOUTLIER,serie)
}

PaxRealvsCOPOutlier <- Pax_Real[legsPaxReal %in% legsCOPOutliers]
COPOutliervsPaxReal <- Rev_COP_Outliers[legsCOPOutliers %in% legsPaxReal]
TMPAXREALvsCOPOUTLIER<- data.frame()

for(i in 1:dim(PaxRealvsCOPOutlier)[2]){
posicion <- match(colnames(PaxRealvsCOPOutlier)[i],colnames(COPOutliervsPaxReal))
serie <- COPOutliervsPaxReal[posicion]/PaxRealvsCOPOutlier[i]
if(i == 1){
TMPAXREALvsCOPOUTLIER <- rbind(TMPAXREALvsCOPOUTLIER,serie)
} else TMPAXREALvsCOPOUTLIER <- cbind(TMPAXREALvsCOPOUTLIER,serie)
}

TMPAXREALvsREVOUTLIER <- cbind2(TMPAXREALvsUSDOUTLIER,TMPAXREALvsCOPOUTLIER)

PaxOutliervsUSDReal <- Pax_Outliers[legsPaxOutliers %in% legsUSDReal]
USDRealvsPaxOutlier<- Rev_USD_Real[legsUSDReal %in% legsPaxOutliers]
TMPAXOUTLIERvsUSDREAL <- data.frame()

for(i in 1:dim(PaxOutliervsUSDReal)[2]){
posicion <- match(colnames(PaxOutliervsUSDReal)[i],colnames(USDRealvsPaxOutlier))
serie <- USDRealvsPaxOutlier[posicion]/PaxOutliervsUSDReal[i]
if(i == 1){
TMPAXOUTLIERvsUSDREAL <- rbind(TMPAXOUTLIERvsUSDREAL,serie)
} else TMPAXOUTLIERvsUSDREAL <- cbind(TMPAXOUTLIERvsUSDREAL,serie)
}

PaxOutliervsCOPReal <- Pax_Outliers[legsPaxOutliers %in% legsCOPReal]
COPRealvsPaxOutlier<- Rev_COP_Real[legsCOPReal %in% legsPaxOutliers]
TMPAXOUTLIERvsCOPREAL <- data.frame()

for(i in 1:dim(PaxOutliervsCOPReal)[2]){
posicion <- match(colnames(PaxOutliervsCOPReal)[i],colnames(COPRealvsPaxOutlier))
serie <- COPRealvsPaxOutlier[posicion]/PaxOutliervsCOPReal[i]
if(i == 1){
TMPAXOUTLIERvsCOPREAL <- rbind(TMPAXOUTLIERvsCOPREAL,serie)
} else TMPAXOUTLIERvsCOPREAL <- cbind(TMPAXOUTLIERvsCOPREAL,serie)
}

TMPAXOUTLIERvsREVREAL <- cbind2(TMPAXOUTLIERvsUSDREAL,TMPAXOUTLIERvsCOPREAL)


setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/Prueba/TM")

write.table(file="TMPAXREALvsREVREAL.csv",TMPAXREALvsREVREAL)
write.table(file="TMPAXOvsREVOUTLIER.csv",TMPAXOvsREVOUTLIER)
write.table(file="TMPAXREALvsREVOUTLIER.csv",TMPAXREALvsREVOUTLIER)
write.table(file="TMPAXOUTLIERvsREVREAL.csv",TMPAXOUTLIERvsREVREAL)

