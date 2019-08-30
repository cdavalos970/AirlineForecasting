library(ggplot2)

Input_TMserie <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Datos Graficas/Tm serie.txt"	
TMserie <- read.table(Input_TMserie, sep=",", header=TRUE, fileEncoding="latin1")

Input_TMgenerada <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Datos Graficas/TM generada.txt"	
TMgenerada <- read.table(Input_TMgenerada, sep=",", header=TRUE, fileEncoding="latin1")

Input_PyR <- "C:/Users/cdavalos/Documents/Pronostico TM/R/Datos Graficas/Pax y Rev.txt"	
PyR  <- read.table(Input_PyR, sep=",", header=TRUE, fileEncoding="latin1")




ggplot(TMserie, aes(x=MAPE)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(TMgenerada, aes(x=MAPE)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(PyR, aes(x=Pax)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(PyR, aes(x=Rev)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
