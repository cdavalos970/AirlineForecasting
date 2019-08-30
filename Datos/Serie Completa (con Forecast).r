Input_Pronostico <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Pronostico_Consolidado.csv"	

Pronostico <- read.table(Input_Pronostico, sep=",", header=TRUE, fileEncoding="latin1") 

Input_Pax_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Real.csv"
Input_Rev_USD_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Real.csv"
Input_Rev_COP_Real <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Real.csv"

Input_Pax_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/PAX_Outliers.csv"
Input_Rev_USD_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_USD_Outliers.csv"
Input_Rev_COP_Outliers <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Output Outlier/REV_COP_Outliers.csv"

Pax_Real <- read.table(Input_Pax_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Real <- read.table(Input_Rev_USD_Real, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_COP_Real <- read.table(Input_Rev_COP_Real, sep=" ", header=TRUE, fileEncoding="latin1") 

Pax_Outliers <- read.table(Input_Pax_Outliers, sep=" ", header=TRUE, fileEncoding="latin1") 
Rev_USD_Outliers<- read.table(Input_Rev_USD_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")
Rev_COP_Outliers<- read.table(Input_Rev_COP_Outliers, sep=" ", header=TRUE, fileEncoding="latin1")


Pax_Real_forecast <- subset(Pronostico,TIPODATO == "Pax_Real" )
		
x <- dim(Pax_Real_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(Pax_Real_forecast$LEG_OW[j],colnames(Pax_Real))
		
		Vector[1,posLeg] = Pax_Real_forecast[j,4]
		Vector[2,posLeg] = Pax_Real_forecast[j,5]
		Vector[3,posLeg] = Pax_Real_forecast[j,6]
		
	}
		
Pax_No_Outliers = rbind(Pax_Real, Vector)
		
		
Pax_Outlier_forecast <- subset(Pronostico,TIPODATO == "Pax_Outlier")
		
x <- dim(Pax_Outlier_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(Pax_Outlier_forecast$LEG_OW[j],colnames(Pax_Real))
		
		Vector[1,posLeg] = Pax_Outlier_forecast[j,4]
		Vector[2,posLeg] = Pax_Outlier_forecast[j,5]
		Vector[3,posLeg] = Pax_Outlier_forecast[j,6]
		
	}
		
Pax_Outliers = rbind(Pax_Real, Vector)


REV_USD_Real_forecast <- subset(Pronostico,TIPODATO == "REV_USD_Real")
		
x <- dim(REV_USD_Real_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(REV_USD_Real_forecast$LEG_OW[j],colnames(Rev_USD_Real))
		
		Vector[1,posLeg] = REV_USD_Real_forecast[j,4]
		Vector[2,posLeg] = REV_USD_Real_forecast[j,5]
		Vector[3,posLeg] = REV_USD_Real_forecast[j,6]
		
	}
		
REV_USD_No_Outliers = rbind(Rev_USD_Real, Vector)


REV_USD_Outliers_forecast <- subset(Pronostico,TIPODATO == "REV_USD_Outliers")
		
x <- dim(REV_USD_Outliers_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(REV_USD_Outliers_forecast$LEG_OW[j],colnames(Rev_USD_Real))
		
		Vector[1,posLeg] = REV_USD_Outliers_forecast[j,4]
		Vector[2,posLeg] = REV_USD_Outliers_forecast[j,5]
		Vector[3,posLeg] = REV_USD_Outliers_forecast[j,6]
		
	}
		
REV_USD_Outliers = rbind(Rev_USD_Real, Vector)

REV_COP_Real_forecast <- subset(Pronostico,TIPODATO == "REV_COP_Real")
		
x <- dim(REV_COP_Real_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(REV_COP_Real_forecast$LEG_OW[j],colnames(Rev_COP_Real))
		
		Vector[1,posLeg] = REV_COP_Real_forecast[j,4]
		Vector[2,posLeg] = REV_COP_Real_forecast[j,5]
		Vector[3,posLeg] = REV_COP_Real_forecast[j,6]
		
	}
		
REV_COP_No_Outliers = rbind(Rev_COP_Real, Vector)


REV_COP_Outliers_forecast <- subset(Pronostico,TIPODATO == "REV_COP_Outliers")
		
x <- dim(REV_COP_Outliers_forecast)[1]
Vector <- data.frame(BOGMAD = character(), BOGMDE = character(), BOGMIA = character(), MADBOG = character(),MDEBOG = character(), MIABOG = character(),stringsAsFactors=F)
	for(j in 1:x){
					
		posLeg <- match(REV_COP_Outliers_forecast$LEG_OW[j],colnames(Rev_COP_Real))
		
		Vector[1,posLeg] = REV_COP_Outliers_forecast[j,4]
		Vector[2,posLeg] = REV_COP_Outliers_forecast[j,5]
		Vector[3,posLeg] = REV_COP_Outliers_forecast[j,6]
		
	}
		
REV_COP_Outliers = rbind(Rev_COP_Real, Vector)
		

		
		
		
			


