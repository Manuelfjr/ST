# Instalando pacotes
setwd('R/')
getwd()

rm(list=ls())
install.packages("expsmooth")
#install.packages("fastR")
install.packages("fpp")
library(expsmooth)
library(fma)
library(forecast)
library(tseries)
#library(fastR)
library(fpp)

url = '/home/manuel/Área de trabalho/UFPB/ST/R/data'
names = c('data_icms_igpdi.csv','ipeadata_icms.csv', 'ipeadata_igpdi.csv')

# Importando dados
data = read.csv(paste(url, names[1], sep = '/'), sep = ',')
data = data[seq(25, dim(data)[1]),]
View(data)
# Processando 
igpdi = data[,2]
icms = data[,3]

str(igpdi)
str(icms)

tam = length(igpdi)
tam_icms = length(icms)

igpdi_d = igpdi/igpdi[tam]
igpdi_d

icms_d = icms/igpdi_d
icms_d

# Formatando como serie temporal
icms_d.ts = ts(icms_d,start=c(1995,1),frequency=12)

# Estatística descritiva
summary(icms_d.ts)
sqrt(var(icms_d.ts))

# Gráficos
plot(icms_d.ts)
acf(icms_d.ts)
pacf(icms_d.ts)

## Decomposicao da serie temporal
icms.stl = decompose( icms_d.ts )

png(".img")
plot(icms.stl)
dev.off()

icms.stl$trend

