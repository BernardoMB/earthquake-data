rm(list=ls())

#Instalamos los paquetes a utilizar
install.packages("evd")
install.packages("evir")
install.packages("fExtremes")
install.packages("ggplot2")
install.packages("POT")
install.packages("PtProcess")
install.packages("PerformanceAnalytics")
library(evd)
library(PerformanceAnalytics)
library(evir)
library(fExtremes)
library(ggplot2)
library(POT)
library(PtProcess)
install.packages('copula')
library('copula')
install.packages('Kendall')
library('Kendall')

#Leemos nuestros datos desde excel en formato csv
Datos <-read.csv("terremotos.csv")

Datos$Index<-c(1:9220)
names(Datos)<-c("Tiempo","Magnitud")

#Grafica de análidis exploratorio de datos
barplot(Datos$Magnitud,xlab="Terremotos en Orden Cronológico\n1950-2015",ylab="Magnitud")
#Histograma en log
hist(log(Datos$Magnitud),breaks=30,xlab="Magnitud del terremoto\nEn escala logarítmica",ylab="Frecuencia",main="Histograma")
plot(density(log(Datos$Magnitud)),main="Densidad de la log")
#QQ plot exponencial vs Datos
qqplot(Datos$Magnitud,rexp(2000,1),main="QQ plot",ylab="Exponencial",xlab="Magnitud del terremoto")
abline(0,.15)
#Distribución empirica
ECDF<- ecdf(Datos$Magnitud)
plot(ECDF,main="Distribucion Empirica de los Datos")
#Medias en exceso
meplot(Datos$Magnitud,main="Mean excess plot")

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

install.packages('copula')
library('copula')
install.packages('Kendall')
library('Kendall')


