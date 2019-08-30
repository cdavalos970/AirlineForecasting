setwd("C:/Users/cdavalos/Documents/Pronostico TM/R/Input")
Input_MinMax <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Min Max Fechas.txt"
MINMAX <- read.table(Input_MinMax, sep=" ", header=TRUE, fileEncoding="latin1")

Input_Correccion <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Correcion.txt"
CORRECCION <- read.table(Input_Correccion, sep=";", header=TRUE, fileEncoding="latin1")

Input_Eliminar <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Input/Eliminar.txt"
ELIMINAR <- read.table(Input_Eliminar, sep=";", header=TRUE, fileEncoding="latin1")

legs <- rownames(MINMAX)
cantCorregir <- dim(CORRECCION)[1]
cantEliminar <- dim(ELIMINAR)[1]

for(i in 1:cantCorregir){
posRuta <- match(CORRECCION[i,1],legs)
MINMAX[posRuta,1] <- CORRECCION[i,2]
MINMAX[posRuta,3] <- CORRECCION[i,3]

}

for(i in 1:cantEliminar){
pos <- match(ELIMINAR[i,1],rownames(MINMAX))
MINMAX <- MINMAX[-pos,]
}

write.table(file="Min Max Fechas.txt",MINMAX)
