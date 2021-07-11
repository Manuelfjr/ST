### Projeto 01 #################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais

# Duvida 01: Erros muito altos da series, problema ?!
# Duvida 02: Suposições aparentemente violadas para ambos bic e aic
# Duvida 04: problema com o teste de estacioiniariedade

# Pacotes --------------------------
library(expsmooth)
library(fma)
library(forecast)
library(tseries)
library(fpp)
library(astsa)
library(tseries)
library(lmtest)
library(nortest)
library(uroot)

# Carregando os dados --------------
url.c.br = 'https://raw.githubusercontent.com/Manuelfjr/ST/main/R/project/R/data/confirmed/data_brasil_new_confirmed.csv'
data.c.br = read.csv(url.c.br, header=T)
data.c.br = data.c.br[-1,]; colnames(data.c.br) = c('Data', 'BR')
data.c.br = data.c.br[-seq(492, 497, 1),]
data.c.br['BR'] = as.numeric(data.c.br$BR)
rownames(data.c.br) = seq(1, dim(data.c.br)[1])
head(data.c.br);tail(data.c.br)
# Plotando a série temporal --------
plot.ts(data.c.br$BR, xlab='Data', ylab='Casos',
        main='Série Temporal') 
grid()
par(mfrow=c(1,2))
acf(data.c.br$BR, main='Função de autocorrelação',
    xlab='defasagem',ylab='autocorrelações')
pacf(data.c.br$BR, main='Função de autocorrelação 
     parcial',
     xlab='defasagem',ylab='autocorrelações')

# Teste de estacionariedade --------
PP.test(data.c.br$BR)
adf.test(data.c.br$BR, alternative='stationary')

# Selecionando modelo --------------
model.aic = auto.arima(data.c.br$BR, ic="aic")
s.AIC = summary(model.aic);s.AIC

model.bic = auto.arima(data.c.br$BR, ic="bic")
s.BIC = summary(model.bic);s.BIC


# Estrategia de previsão -----------
l = length(data.c.br$BR)

## Excluindo [7] ultimas observações
covid.br.7.ts=ts(data.c.br$BR[-((l - 6):l)])
covid.br.7.ts


# Modelo com séries truncadas ------
auto.arima(covid.br.7.ts, ic="aic")
#ARIMA(3,1,3)

auto.arima(covid.br.7.ts, ic="bic")
#ARIMA(2,1,2)


# Ajuste (Serie truncada) ----------
AIC.ajuste.covid.br.7.ts = arima(covid.br.7.ts , order =c(3 ,1 ,3))
summary(AIC.ajuste.covid.br.7.ts)

BIC.ajuste.covid.br.7.ts = arima(covid.br.7.ts , order =c(2 ,1 ,2))
summary(BIC.ajuste.covid.br.7.ts)

## Residuos (AIC) ------------------
#residuos
tsdiag(AIC.ajuste.covid.br.7.ts)
AIC.std.7.resid = (AIC.ajuste.covid.br.7.ts$residuals - mean(AIC.ajuste.covid.br.7.ts$residuals))/sd(AIC.ajuste.covid.br.7.ts$residuals)

# std resid
plot(AIC.std.7.resid, type='h', xlab = 'Tempo', ylab='Resíduo padronizado',
     main='Resíduos padronizados')

# acf resid
acf(AIC.std.7.resid, main='ACF dos resíduos')


rd.std = (AIC.ajuste.covid.br.7.ts$residuals - mean(AIC.ajuste.covid.br.7.ts$residuals))/sd(AIC.ajuste.covid.br.30.ts$residuals)
qqnorm(rd.std); qqline(rd.std, col = 2); grid()
lillie.test(rd.std)
# Rejeitamos h0

## Residuos (BIC) ------------------
#residuos
tsdiag(BIC.ajuste.covid.br.7.ts)
BIC.std.7.resid = (BIC.ajuste.covid.br.7.ts$residuals - mean(BIC.ajuste.covid.br.7.ts$residuals))/sd(BIC.ajuste.covid.br.7.ts$residuals)
# std resid
plot(BIC.std.7.resid, type='h', xlab = 'Tempo', ylab='Resíduo padronizado',
     main='Resíduos padronizados')
decompose(data.c.br$BR)
# acf resid
acf(BIC.std.7.resid, main='ACF dos resíduos')

rd.std = (BIC.ajuste.covid.br.7.ts$residuals - mean(BIC.ajuste.covid.br.7.ts$residuals))/sd(AIC.ajuste.covid.br.30.ts$residuals)
qqnorm(rd.std); qqline(rd.std, col = 2); grid()
lillie.test(rd.std)
# Rejeitamos h0

# Rejeitamos h0
## Previsão (AIC) ------------------
# Valores previstos
p.1_7=forecast(AIC.ajuste.covid.br.7.ts, h=1)
p.5_7=forecast(AIC.ajuste.covid.br.7.ts, h=5)
p.7_7=forecast(AIC.ajuste.covid.br.7.ts, h=7)
#p.30_30=forecast(AIC.ajuste.covid.br.7.ts, h=30)

# Valores reais
h.1_7= data.c.br[l-7 + 1,]
dim(h.1_7)
h.5_7= data.c.br[(l-7+1):(l - 7 + 5),]
dim(h.5_7)
h.7_7= data.c.br[(l-7+1):(l - 7 + 7),]
dim(h.7_7)
#h.30_30= data.c.br[(l - 30 + 1):l,]
#dim(h.30_30)

### Comparando valores -------------
?accuracy
accuracy(p.1_7$mean,  h.1_7$BR)
accuracy(p.5_7$mean,  h.5_7$BR)
accuracy(p.7_7$mean,  h.7_7$BR)
#accuracy(p.30_30$mean,  h.30_30$BR)

metricaAIC.covid.br.7 = rbind(accuracy(p.1_7$mean,  h.1_7$BR)[1,c(1,2,3)],accuracy(p.5_7$mean,  h.5_7$BR)[1,c(1,2,3)],
                             accuracy(p.7_7$mean,  h.7_7$BR)[1,c(1,2,3)])
colnames(metricaAIC.covid.br.7) = c('ME-AIC', 'RMSE-AIC','MAE-AIC')
rownames(metricaAIC.covid.br.7) = c("h=1", "h=5", "h=7")
metricaAIC.covid.br.7


## Previsão (BIC) ------------------
# Valores previstos
p.1_7=forecast(BIC.ajuste.covid.br.7.ts, h=1)
p.5_7=forecast(BIC.ajuste.covid.br.7.ts, h=5)
p.7_7=forecast(BIC.ajuste.covid.br.7.ts, h=7)
#p.30_30=forecast(AIC.ajuste.covid.br.7.ts, h=30)

# Valores reais
h.1_7= data.c.br[l-7 + 1,]
dim(h.1_7)
h.5_7= data.c.br[(l-7+1):(l - 7 + 5),]
dim(h.5_7)
h.7_7= data.c.br[(l-7+1):(l - 7 + 7),]
dim(h.7_7)
#h.30_30= data.c.br[(l - 30 + 1):l,]
#dim(h.30_30)

### Comparando valores -------------
?accuracy
accuracy(p.1_7$mean,  h.1_7$BR)
accuracy(p.5_7$mean,  h.5_7$BR)
accuracy(p.7_7$mean,  h.7_7$BR)
#accuracy(p.30_30$mean,  h.30_30$BR)

metricaBIC.covid.br.7 = rbind(accuracy(p.1_7$mean,  h.1_7$BR)[1,c(1,2,3)],accuracy(p.5_7$mean,  h.5_7$BR)[1,c(1,2,3)],
                              accuracy(p.7_7$mean,  h.7_7$BR)[1,c(1,2,3)])
colnames(metricaBIC.covid.br.7) = c('ME-BIC', 'RMSE-BIC','MAE-BIC')
rownames(metricaBIC.covid.br.7) = c("h=1", "h=5", "h=7")
metricaBIC.covid.br.7
  
