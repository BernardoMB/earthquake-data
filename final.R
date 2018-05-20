# Instalamos los paquetes a utilizar
install.packages("evd")
library(evd)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Extreme values in R
# ftp://cran.r-project.org/pub/R/web/packages/evir/evir.pdf
install.packages("evir")
library(evir)

install.packages("fExtremes")
library(fExtremes)
install.packages("ggplot2")
library(ggplot2)
install.packages("POT")
library(POT)
install.packages("PtProcess")
library(PtProcess)
install.packages('copula')
library('copula')
install.packages('Kendall')
library('Kendall')

# Read data
data <-read.csv("terremotos.csv")

# Change data frame names
names(data) <- c("time","magnitudee")

# Grafica de an?lidis exploratorio de data (Serie de tiempo)
barplot(data$magnitude,xlab="Terremotos en Orden Cronologico\n1950-2015",ylab="Magnitud")


#Histograma en log
hist(log(data$magnitude),breaks=30,xlab="magnitude del terremoto\nEn escala logar?tmica",ylab="Frecuencia",main="Histograma")
plot(density(log(data$magnitude)),main="Densidad de la log")


#QQ plot exponencial vs data
qqplot(data$magnitude ,rexp(2000,1), main="QQ plot",ylab="Exponencial",xlab="magnitude del terremoto")
abline(0,.15)


#Distribuci?n empirica
ECDF<- ecdf(data$magnitude)
plot(ECDF,main="Distribucion Empirica de los data")



#Medias en exceso
meplot(data$magnitude,main="Mean excess plot")





#Distribuciones con las que vamos a comparar
LOG<-rlnorm(45, meanlog = 0, sdlog = 1)
P<- ecdf(LOG)
plot(P,col="blue",main="Distribucion de la lognormal")

PARETO<- rpareto(45, 1, 1)
PA<- ecdf(PARETO)
plot(PA,col="red",main="Distribucion de la Pareto")

GPD<- rgpd(45, .497, 10,6.98)
G<- ecdf(GPD)
plot(G,col="green",main="Distribucion de la GDP")

par(mfrow=c(2,2))
plot(P,col="blue",main="Distribucion de la lognormal")
plot(PA,col="red",main="Distribucion de la Pareto")
plot(G,col="green",main="Distribucion de la GDP")



