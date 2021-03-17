### Projeto 01 #################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais
setwd("/home/manuel/Área de trabalho/UFPB/ST/R/project/R")
getwd()
# Importando os dados
url.c = 'data/confirmed/data_brasil_new_confirmed.csv'
url.d = 'data/deaths/data_brasil_new_deaths.csv'

## Novos casos
data.c = read.csv(url.c, header = TRUE)[-1,]
colnames(data.c) = c('Data', 'BR')
data.c = data.c[-c(1,2,3,4),]

## Novas mortes
data.d = read.csv(url.d, header = TRUE)[-1,]
colnames(data.d) = c('Data', 'BR')
data.d = data.d[-c(1,2,3,4),]

# Processando
## Serie temporal 
### Novos casos confirmados
y.c = as.integer(data.c$BR)
y.c.ts = ts(y.c, start=c(2020,3), frequency = 367)

png(".img/data_brasil_new_confirmed.png", width=480, height = 270)
plot(y.c.ts, xlab = 'tempo', ylab = 'casos')
title('Novos casos confirmados no Brasil (01/03/2020 á 16/03/2021)')
grid()
dev.off()

acf(y.c.ts)
pacf(y.c.ts)

### Novas mortes confirmados
y.d = as.integer(data.d$BR)
y.d.ts = ts(y.d, start=c(2020,3), frequency = 367)

png(".img/data_brasil_new_deaths.png", width=480, height = 270)
plot(y.d.ts, xlab = 'tempo', ylab = 'mortes')
title('Novas mortes confirmados no Brasil (01/03/2020 á 16/03/2021)')
grid()
dev.off()

acf(y.d.ts)
pacf(y.d.ts)

