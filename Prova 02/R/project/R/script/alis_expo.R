### Projeto 01 #################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais

# Duvida 01: Erros muito altos da series, problema ?!
# Duvida 02: Suposições aparentemente violadas para ambos bic e aic
# Duvida 03: resultado horrivel do alisamento exponencial


# Pacotes --------------------------
library(expsmooth)
library(fma)
library(ses)
library(forecast)
library(tseries)
library(fpp)
library(astsa)
library(tseries)
library(lmtest)
library(nortest)
library(uroot)
library(ggplot2)

# Carregando os dados --------------
url.c.br = 'https://raw.githubusercontent.com/Manuelfjr/ST/main/R/project/R/data/confirmed/data_brasil_new_confirmed.csv'
data.c.br = read.csv(url.c.br, header=T)
data.c.br = data.c.br[-1,]; colnames(data.c.br) = c('Data', 'BR')
data.c.br = data.c.br[-seq(492, 497, 1),]
data.c.br['BR'] = as.numeric(data.c.br$BR)
rownames(data.c.br) = seq(1, dim(data.c.br)[1])
head(data.c.br);tail(data.c.br)

# Alisamento exponencial -----------
l = length(data.c.br$BR)

m = ses(data.c.br$BR)

z=m$fitted

plot(ts(data.c.br$BR,frequency=365),main="",xlab="",ylab='Casos')
par(new=T)
plot(m$fitted, col='red', xlab='', ylab='',axes=F, ann=F)
legend(x = "topleft",          # Position
       legend = c("Alisamento exponencial", "Série temporal"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c('red', 'black'),           # Line colors
       lwd = 2)
grid()

# Estrategia de previsão -----------

## Excluindo [7] ultimas observações
covid.br.7.ts=ts(data.c.br$BR[-((l - 6):l)])
covid.br.7.ts

m = ses(covid.br.7.ts)
summary(m)
z=m$fitted

plot(covid.br.7.ts,main="",xlab="Dia",ylab='Casos')
par(new=T)
plot(m$fitted, col='red', xlab='', ylab='',axes=F, ann=F)
legend(x = "topleft",          # Position
       legend = c("Alisamento exponencial", "Série temporal"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c('red', 'black'),           # Line colors
       lwd = 2)
grid()

# Previsão ------------------
# Valores previstos
p.1_7=ses(covid.br.7.ts, h=1)
p.5_7=ses(covid.br.7.ts, h=5)
p.7_7=ses(covid.br.7.ts, h=7)

# Valores reais
h.1_7= data.c.br[l-7 + 1,]
dim(h.1_7)
h.5_7= data.c.br[(l-7+1):(l - 7 + 5),]
dim(h.5_7)
h.7_7= data.c.br[(l-7+1):(l - 7 + 7),]
dim(h.7_7)

# Métricas
metricasLIS = rbind(accuracy(p.1_7$mean,  h.1_7$BR)[1,c(1,2,3)],accuracy(p.5_7$mean,  h.5_7$BR)[1,c(1,2,3)],
                              accuracy(p.7_7$mean,  h.7_7$BR)[1,c(1,2,3)])
colnames(metricasLIS) = c('ME-AIC', 'RMSE-AIC','MAE-AIC')
rownames(metricasLIS) = c("h=1", "h=5", "h=7")
metricasLIS
