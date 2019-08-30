library("zoo")

Input_Consolidado <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Consolidado Total.txt"
RAW_DATA <- read.table(Input_Consolidado, sep=";", header=TRUE, fileEncoding="latin1")

Input_RUTASPUROS <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Rutas Puros.txt"
RUTAS_PUROS <- read.table(Input_RUTASPUROS, sep=";", header=TRUE, fileEncoding="latin1")

RAW_DATA_SORT <- subset(RAW_DATA, RAW_DATA$LEG_OW %in% RUTAS_PUROS$LEG_OW)

RAW_DATA_SORT <- RAW_DATA_SORT[order(RAW_DATA_SORT[,3],RAW_DATA_SORT[,1],RAW_DATA_SORT[,2]),]

nuevo_RAW_DATA <- RAW_DATA_SORT


legs <- unique(RAW_DATA_SORT$LEG_OW)
cantLegs <- length(legs)
cantDatosTotales <- dim(RAW_DATA_SORT)[1]

Min_Max <- data.frame(MINAÑO = character(), MAXAÑO = character(), MINMES = character(), MAXMES = character(), stringsAsFactors=F)
faltantes <- data.frame()

legsPronos <- data.frame()
AÑOMES_MATRIZ <- data.frame(FECHA = character(), stringsAsFactors=F)

for(i in 1:cantDatosTotales){
AÑOMES <- data.frame(FECHA = as.numeric(paste(RAW_DATA_SORT[i,1],RAW_DATA_SORT[i,2],sep ="")))
AÑOMES_MATRIZ <- rbind(AÑOMES_MATRIZ, AÑOMES)
}

AGRE <- cbind(AÑOMES_MATRIZ,RAW_DATA_SORT)

for(i in 1:cantLegs){

subs <- subset(AGRE, LEG_OW == legs[i])

minimoAÑO <- min(subset(RAW_DATA_SORT, LEG_OW == legs[i])[1])
maximoAÑO <- max(subset(RAW_DATA_SORT, LEG_OW == legs[i])[1])

minimoMES <- min(subset(RAW_DATA_SORT, (LEG_OW == legs[i] & YY == minimoAÑO))[2])
maximoMES <- max(subset(RAW_DATA_SORT, (LEG_OW == legs[i] & YY == maximoAÑO))[2])

Vector <- data.frame(MINAÑO = minimoAÑO, MAXAÑO = maximoAÑO, MINMES = minimoMES, MAXMES = maximoMES)


Min_Max <- rbind(Min_Max,Vector)

inicio <- paste(minimoAÑO,minimoMES,"1",sep ="/")
fin <- paste(maximoAÑO,maximoMES,"1",sep ="/")

serie <- t(seq(from = as.Date(inicio), to = as.Date(fin),by = "month"))
cantDatosSerie  <- length(serie)

	for(j in 1:cantDatosSerie){
	
	YY <- as.integer(format(serie[j],format = "%Y"))
	MM <- as.integer(format(serie[j],format = "%m"))
	
	pos <- match(as.numeric(paste(YY,MM,sep ="")),subs$FECHA)
	
	if (is.na(pos)){
	faltantes[i,j] <- as.Date(paste(YY,MM,"1",sep ="/"))
	} else faltantes[i,j] <- NA
	}
 }

rownames(Min_Max) <- legs
rownames(faltantes) <- legs
cantDatosVector <- data.frame(DATO = character())
cantDatosFaltantes <- data.frame()

for(i in 1:cantLegs){

vectorFaltantes <- faltantes[i,][!is.na(faltantes[i,])]
cantFaltantes <- length(vectorFaltantes)
cantDatosFaltantes <- rbind(cantDatosFaltantes,cantFaltantes)
	if(cantFaltantes > 0) for(j in 1:cantFaltantes){
	Vector <- data.frame(YY = as.integer(format(as.Date(vectorFaltantes[j]),format = "%Y")), MM = as.integer(format(as.Date(vectorFaltantes[j]),format = "%m")), LEG_OW = legs[i], REV_USD = NA, REV_COP = NA, PAX = NA)
	nuevo_RAW_DATA<- rbind(nuevo_RAW_DATA, Vector)
	}
	
cantDatos <- data.frame(DATO = dim(subset(nuevo_RAW_DATA, LEG_OW == legs[i]))[1])

cantDatosVector <- rbind(cantDatosVector,cantDatos-cantFaltantes)

}

rownames(cantDatosVector) <- legs

nuevo_RAW_DATA_PRUEBA <- data.frame()
Min_Max_def <- data.frame()

for(i in 1:cantLegs){
	if(cantDatosVector[i,]>35 ){
	subs <- subset(nuevo_RAW_DATA, LEG_OW == legs[i])
	nuevo_RAW_DATA_PRUEBA <- rbind(nuevo_RAW_DATA_PRUEBA,subs)
	Min_Max_def<- rbind(Min_Max_def,Min_Max[i,])
	}
}

elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
add.months <- function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

inicioCore <- paste(2008,1,"1",sep ="/")
legsFinal <- unique(nuevo_RAW_DATA_PRUEBA$LEG_OW)
cantLegsFinales <- length(unique(nuevo_RAW_DATA_PRUEBA$LEG_OW))
nuevo_RAW_DATA_FINAL <- nuevo_RAW_DATA_PRUEBA

for(i in 1:cantLegsFinales){

	pos <- match(legsFinal[i], rownames(Min_Max_def))
	
	finCore <- paste(Min_Max_def[pos,1],Min_Max_def[pos,3],"1",sep ="/")
	if(inicioCore < finCore){
		meses <- elapsed_months(finCore,inicioCore)
		for(j in 1:meses){
			if(j == 1){
			Vector <- data.frame(YY = as.integer(format(as.Date(inicioCore),format = "%Y")), MM = as.integer(format(as.Date(inicioCore),format = "%m")), LEG_OW = legsFinal[i], REV_USD = NA, REV_COP = NA, PAX = NA)
			nuevo_RAW_DATA_FINAL<- rbind(nuevo_RAW_DATA_FINAL, Vector)
			}else{
			Vector <- data.frame(YY = as.integer(format(add.months(as.Date(inicioCore),j-1),format = "%Y")), MM = as.integer(format(add.months(as.Date(inicioCore),j-1),format = "%m")), LEG_OW = legsFinal[i], REV_USD = NA, REV_COP = NA, PAX = NA)
			nuevo_RAW_DATA_FINAL<- rbind(nuevo_RAW_DATA_FINAL, Vector)
			}
				
		}
	}
}

setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Input")

nuevo_RAW_DATA_FINAL <- nuevo_RAW_DATA_FINAL[order(nuevo_RAW_DATA_FINAL[,3],nuevo_RAW_DATA_FINAL[,1],nuevo_RAW_DATA_FINAL[,2]),]

write.table(file="Consolidado Fixed.txt",nuevo_RAW_DATA_FINAL)
write.table(file="Faltantes.txt",faltantes)
write.table(file="Min Max Fechas.txt",Min_Max_def)



