rm(list=ls())

library(xts)
library(qrmtools)
library(zoo)
require(openxlsx)
library(ggplot2)
library(export)
library(datasets)

### DATOS ###

setwd("D:\\Desktop\\Ingeniería Civil\\Semilleros-Investigación\\Finanzas\\Semestre 2\\Risk Theory\\Homework 2")

PFDAVVND <- read.xlsx("Acciones_BDI.xlsx", sheet=1, colNames=T)
PFDAVVND[,1] <- convertToDate(PFDAVVND[,1])
PFDAVVND <- read.zoo(PFDAVVND)

BOGOTA <- read.xlsx("Acciones_BDI.xlsx", sheet=2, colNames=T)
BOGOTA[,1] <- convertToDate(BOGOTA[,1])
BOGOTA <- read.zoo(BOGOTA)

ISA <- read.xlsx("Acciones_BDI.xlsx", sheet=3, colNames=T)
ISA[,1] <- convertToDate(ISA[,1])
ISA <- read.zoo(ISA)

### PRECIOS ###

PFDAVVND.P <- PFDAVVND[,1, drop=FALSE]
BOGOTA.P <- BOGOTA[,1, drop=FALSE]
ISA.P <- ISA[,1, drop=FALSE]

### RETORNOS DIARIOS ###

PFDAVVND.R <- returns(PFDAVVND.P,method = c("logarithmic"))
BOGOTA.R <- returns(BOGOTA.P,method = c("logarithmic"))
ISA.R <- returns(ISA.P,method = c("logarithmic"))

### GRAFICOS ###

## PRECIOS

Prices <- fortify(cbind(PFDAVVND.P, BOGOTA.P, ISA.P), melt=F, names=c("Time", "Data"))

PFDAVVND.GP <- ggplot(Prices, aes(x=Prices[,1], y=Prices[,2])) + geom_line() + xlab("Periodo") + ylab("Precio") + ggtitle("SERIE DE PRECIOS PREFERENCIAL DAVIVIENDA") +
               theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                axis.title.x = element_text(size=11, face="bold"),
                axis.title.y = element_text(size=11, face="bold"))
ggsave("PFDAVVND_GP.png", plot=PFDAVVND.GP, dpi=600)

BOGOTA.GP <- ggplot(Prices, aes(x=Prices[,1], y=Prices[,3])) + geom_line() + xlab("Periodo") + ylab("Precio") + ggtitle("SERIE DE PRECIOS BANCO BOGOTA") +
              theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                axis.title.x = element_text(size=11, face="bold"),
                axis.title.y = element_text(size=11, face="bold"))
ggsave("BOGOTA_GP.png", plot=BOGOTA.GP, dpi=600)


ISA.GP <- ggplot(Prices, aes(x=Prices[,1], y=Prices[,4])) + geom_line() + xlab("Periodo") + ylab("Precio") + ggtitle("SERIE DE PRECIOS ISA") +
            theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                  axis.title.x = element_text(size=11, face="bold"),
                  axis.title.y = element_text(size=11, face="bold"))
ggsave("ISA_GP.png", plot=ISA.GP, dpi=600)

## RETORNOS

Returns <- fortify(cbind(PFDAVVND.R, BOGOTA.R, ISA.R), melt=F, names=c("Time", "Data"))

PFDAVVND.GR <- ggplot(Returns, aes(x=Returns[,1], y=Returns[,2])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS PREFERENCIAL DAVIVIENDA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("PFDAVVND_GR.png", plot=PFDAVVND.GR, dpi=600)

BOGOTA.GR <- ggplot(Returns, aes(x=Returns[,1], y=Returns[,3])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS BANCO BOGOTA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("BOGOTA_GR.png", plot=BOGOTA.GR, dpi=600)


ISA.GR <- ggplot(Returns, aes(x=Returns[,1], y=Returns[,4])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS ISA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("ISA_GR.png", plot=ISA.GR, dpi=600)

## DENSIDADES

PFDAVVND.GD <- ggplot(Returns, aes(x=Returns[,2])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS PREFERENCIAL DAVIVIENDA") +
                theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                      axis.title.x = element_text(size=11, face="bold"),
                      axis.title.y = element_text(size=11, face="bold")) 
ggsave("PFDAVVND_GD.png", plot=PFDAVVND.GD, dpi=600)

BOGOTA.GD <- ggplot(Returns, aes(x=Returns[,3])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS BANCO BOGOTA") +
              theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                    axis.title.x = element_text(size=11, face="bold"),
                    axis.title.y = element_text(size=11, face="bold")) 
ggsave("BOGOTA_GD.png", plot=BOGOTA.GD, dpi=600)


ISA.GD <- ggplot(Returns, aes(x=Returns[,4])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS ISA") +
            theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
                  axis.title.x = element_text(size=11, face="bold"),
                  axis.title.y = element_text(size=11, face="bold")) 
ggsave("ISA_GD.png", plot=ISA.GD, dpi=600)

## LOWER TAILS

PFDAVVND.GD.LT <- ggplot(Returns, aes(x=Returns[,2])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA INFERIOR DE LOS LOG-RETORNOS PREFERENCIAL DAVIVIENDA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(NA,-0.03)
ggsave("PFDAVVND_GD_LT.png", plot=PFDAVVND.GD.LT, dpi=600)

BOGOTA.GD.LT <- ggplot(Returns, aes(x=Returns[,3])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA INFERIOR DE LOS LOG-RETORNOS BANCO BOGOTA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(NA,-0.1)
ggsave("BOGOTA_GD_LT.png", plot=BOGOTA.GD.LT, dpi=600)


ISA.GD.LT <- ggplot(Returns, aes(x=Returns[,4])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA INFERIOR DE LOS LOG-RETORNOS ISA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(NA,-0.04)
ggsave("ISA_GD_LT.png", plot=ISA.GD.LT, dpi=600)

## UPPER TAILS

PFDAVVND.GD.UT <- ggplot(Returns, aes(x=Returns[,2])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA SUPERIOR DE LOS LOG-RETORNOS PREFERENCIAL DAVIVIENDA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(0.04, NA)
ggsave("PFDAVVND_GD_UT.png", plot=PFDAVVND.GD.UT, dpi=600)

BOGOTA.GD.UT <- ggplot(Returns, aes(x=Returns[,3])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA SUPERIOR DE LOS LOG-RETORNOS BANCO BOGOTA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(0.01, NA)
ggsave("BOGOTA_GD_UT.png", plot=BOGOTA.GD.UT, dpi=600)


ISA.GD.UT <- ggplot(Returns, aes(x=Returns[,4])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("COLA SUPERIOR DE LOS LOG-RETORNOS ISA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) + xlim(0.05, NA)
ggsave("ISA_GD_UT.png", plot=ISA.GD.UT, dpi=600)

### RETORNOS MENSUALES ###

PFDAVVND.m <- apply.monthly(PFDAVVND.R, FUN = colSums)
BOGOTA.m <- apply.monthly(BOGOTA.R, FUN = colSums)
ISA.m <- apply.monthly(ISA.R, FUN = colSums)

## RETORNOS

ReturnsM <- fortify(cbind(PFDAVVND.m, BOGOTA.m, ISA.m), melt=F, names=c("Time", "Data"))

PFDAVVND.GRM <- ggplot(ReturnsM, aes(x=ReturnsM[,1], y=ReturnsM[,2])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS MENSUALES PREFERENCIAL DAVIVIENDA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("PFDAVVND_GRM.png", plot=PFDAVVND.GRM, dpi=600)

BOGOTA.GRM <- ggplot(ReturnsM, aes(x=ReturnsM[,1], y=ReturnsM[,3])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS MENSUALES BANCO BOGOTA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("BOGOTA_GRM.png", plot=BOGOTA.GRM, dpi=600)


ISA.GRM <- ggplot(ReturnsM, aes(x=ReturnsM[,1], y=ReturnsM[,4])) + geom_line() + xlab("Periodo") + ylab("Retorno") + ggtitle("SERIE DE LOG-RETORNOS MENSUALES ISA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"))
ggsave("ISA_GRM.png", plot=ISA.GRM, dpi=600)

## DENSIDADES

PFDAVVND.GDM <- ggplot(ReturnsM, aes(x=ReturnsM[,2])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS MENSUALES PREFERENCIAL DAVIVIENDA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) 
ggsave("PFDAVVND_GDM.png", plot=PFDAVVND.GDM, dpi=600)

BOGOTA.GDM <- ggplot(ReturnsM, aes(x=ReturnsM[,3])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS MENSUALES BANCO BOGOTA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) 
ggsave("BOGOTA_GDM.png", plot=BOGOTA.GDM, dpi=600)


ISA.GDM <- ggplot(ReturnsM, aes(x=ReturnsM[,4])) + geom_density(fill="gray") + xlab("Retorno") + ylab("Densidad") + ggtitle("DISTRIBUCIÓN DE LOS LOG-RETORNOS MENSUALES ISA") +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold")) 
ggsave("ISA_GDM.png", plot=ISA.GDM, dpi=600)

### CORRELACIONES ###

X <- merge(PFDAVVND = PFDAVVND.R, BOGOTA = BOGOTA.R, ISA=ISA.R, all = FALSE)

ACF12 <- acf(X[,c(1,2)])
ACF13 <- acf(X[,c(1,3)])
ACF23 <- acf(X[,c(2,3)])

graph2bitmap(x=ACF13)

CORR12 <- cor(X[,c(1,2)])
CORR13 <- cor(X[,c(1,3)])
CORR23 <- cor(X[,c(2,3)])

ACF12_A <- acf(abs(X[,c(1,2)]))
ACF13_A <- acf(abs(X[,c(1,3)]))
ACF23_A <- acf(abs(X[,c(2,3)]))

###CHANGE #1
