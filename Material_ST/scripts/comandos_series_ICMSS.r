# Periodo JAN/95 - JUN/2020
# Preço constante - JUN/2020
rm(list=ls())
install.packages("expsmooth")
install.packages("fastR")
install.packages("fpp")
library(expsmooth)
library(fma)
library(forecast)
library(tseries)
library(fastR)
library(fpp)




dados = read.table("igpdi_icms_pb.txt", header=T, stringsAsFactors = FALSE)
igpdi=dados[,2]
icmspb=dados[,3]
str(igpdi)
str(icmspb)
tam = length(igpdi)
tam_icmspb = length(icmspb)

# Aqui dividimos todas as observações da série de preços
# por seu ultimo valor. Dessa forma, a série deflacionada
# fica a precos constantes da ultima observaçãoo.
igpdi_d = igpdi/igpdi[tam]
igpdi_d

# Agora dividimos os valores da arrecadados do ICMS
# pelo IGP-DI a fim de obter uma serie livre de
# dinamina inflacionaria
icmspb_d = icmspb/igpdi_d

# Aqui formatamos os dados como uma sÃ©rie temporal
icmspb_d.ts = ts(icmspb_d,start=c(1995,1),frequency=12)

# Estatística descritiva
summary(icmspb_d.ts)
sqrt(var(icmspb_d.ts))

# Gráficos
plot(icmspb_d.ts)
acf(icmspb_d.ts)
pacf(icmspb_d.ts)

## Decomposicao da serie temporal
icms.stl = decompose( icmspb_d.ts )
plot(icms.stl)
icms.stl$trend


