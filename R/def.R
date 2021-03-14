# Instalando pacotes
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
data = data[seq(13, dim(data)[1]),]
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
plot(icms.stl)
icms.stl$trend


# test --------------------------------------------------------------------


months = c('01','02','03','04','05','06','07','08','09','10','11','12')
values = numeric(0)
k = 1
m = 1
for (i in 1995:2020){
  for (j in 1:12){
    values[k] = paste(i, months[m], sep = '.')
    m = m + 1
    k = k + 1
  }
  m = 1
}
length(values)
dim(data)
data[seq(25,(dim(data)[1]) - 6),]
