### Projeto 01 #################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais
setwd("R")
getwd()
library(urca)
# BRASIL (BR) -------------------------------------------------------------
# Importando os dados
url.c = '/home/manuel/Área de Trabalho/git/UFPB/ST/R/project/R/data/confirmed/data_brasil_new_confirmed.csv'
url.d = '/home/manuel/Área de Trabalho/git/UFPB/ST/R/project/R/data/deaths/data_brasil_new_deaths.csv'

y =arima.sim(list(order = c(1,0,0), ar = 0.7), n = 100)

y
y[1:10]
#defasagem igual a 1, ou seja, y_{t-1}
y.1 = lag(y,-1)
y.1[1:10]
#defasagem igual a 2, ou seja, y_{t-2}
y.2 = lag(y,-2)
y.2[1:10]
#defasagem igual a 3, ou seja, y_{t-3}
y.3 = lag(y,-3)
y.3[1:10]

# concatenando a serie temporal e
# as defasagens
cbind(y, y.1, y.2, y.3)

cor(y[1:99], y.1[2:100])
cor(y[1:98], y.2[3:100])
cor(y[1:97], y.2[4:100])
cor(y[1:96], y.2[5:100])
cor(y[1:95], y.2[6:100])

# Processando

## Novos casos
data.c = read.csv(url.c, header = TRUE)[-1,]

rownames(data.c) = seq(dim(data.c)[1])
colnames(data.c) = c('Data', 'BR')
data.c['BR'] = as.integer(data.c$BR)
#data.c = data.c[-c(1,2,3,4),]
summary(data.c)
## Novas mortes
data.d = read.csv(url.d, header = TRUE)[-1,]

rownames(data.d) = seq(dim(data.d)[1])
colnames(data.d) = c('Data', 'BR')
data.d['BR'] = as.integer(data.d$BR)
summary(data.d)
#data.d = data.d[-c(1,2,3,4),]

## Serie temporal 
### Novos casos confirmados
y.c = as.integer(data.c$BR)

d <- seq(as.Date("2020-02-26"), as.Date("2021-04-24"), "day")
months <- seq(min(d), max(d), "month")

png("/home/manuel/Área de Trabalho/git/UFPB/ST/R/project/R/.img/data_brasil_new_confirmed.png", width=480, height = 270)
plot(y.c~ d, type = 'l',xaxt = "n", xlab = 'tempo', ylab = 'casos')

title(
  paste('Novos casos confirmados no Brasil (',data.d$Data[1], ' á ',data.d$Data[length(data.d)], ')',sep=' ')
  )
# draw X axis
axis(1, months, format(months, "\n%Y\n%b"))
grid()
dev.off()
graphics.off()

par(mfrow=c(1,2))
acf(y.c, main='Função de auto correlação',
    xlab="defasagem",ylab="autocorrelacoes")
pacf(y.c, main='Função de auto correlação parcial',
     xlab="defasagem",ylab="autocorrelacoes")

PP.test(y.c)
### Novas mortes confirmados
y.d = as.integer(data.d$BR)
y.d.ts = ts(y.d, start=c(2020.3,1), frequency = 365)

png("'/home/manuel/Área de Trabalho/git/UFPB/ST/R/project/R/.img/data_brasil_new_deaths.png", width=480, height = 270)
plot(y.d.ts, xlab = 'tempo', ylab = 'mortes')
title('Novas mortes confirmados no Brasil (01/03/2020 á 24/04/2021)')
grid()

# Teste
a = tail(data.d,24)
y = ts(as.integer(a$BR), start=c(2021.3,1),frequency = 365)
plot(y)
data.d[max(y.d) == y.d,]

acf(y.d.ts)
pacf(y.d.ts)

# Regiões do Brasil (NE, S, SE, CO, N) ------------------------------------


## Importando os dados -----------------------------------------------------
url.c = 'data/confirmed/data_regions_new_confirmed.csv'
url.d = 'data/deaths/data_regions_new_deaths.csv'

## Novos casos
data.c = read.csv(url.c, header = TRUE)[-1,]
View(data.c[,-2])

## Processando
colnames(data.c)[1] = c('Data')
data.c = data.c[-c(1,2,3,4),]

## Novas mortes
data.d = read.csv(url.d, header = TRUE)[-1,]
View(data.c[,-2])

## Processando
colnames(data.d)[1] = c('Data')
data.d = data.d[-c(1,2,3,4),]


## Series temporais --------------------------------------------------------

### Novos casos -------------------------------------------------------------------
y.ne.c = as.integer(data.c$Nordeste)
y.n.c = as.integer(data.c$Norte)
y.s.c = as.integer(data.c$Sul)
y.se.c = as.integer(data.c$Sudeste)
y.co.c = as.integer(data.c$Centro.Oeste)

y.ne.c.ts = ts(y.ne.c, start=c(2020,3), frequency = 367)
y.n.c.ts = ts(y.n.c, start=c(2020,3), frequency = 367)
y.s.c.ts = ts(y.s.c, start=c(2020,3), frequency = 367)
y.se.c.ts = ts(y.se.c, start=c(2020,3), frequency = 367)
y.co.c.ts = ts(y.co.c, start=c(2020,3), frequency = 367)

png(".img/data_country_new_confirmed.png", width=480, height = 270)
par(mfrow=c(2,3))
plot(y.ne.c.ts, xlab = 'tempo', ylab = 'casos')
title('Nordeste (NE)')
grid()
plot(y.n.c.ts, xlab = 'tempo', ylab = 'casos')
title('Norte (N)')
grid()
plot(y.s.c.ts, xlab = 'tempo', ylab = 'casos')
title('Sul (S)')
grid()
plot(y.se.c.ts, xlab = 'tempo', ylab = 'casos')
title('Suldeste (SE)')
grid()
plot(y.co.c.ts, xlab = 'tempo', ylab = 'casos', )
title('Centro Oeste (CO)')
grid()
dev.off()

acf(y.c.ts)
pacf(y.c.ts)

### Novas mortes ------------------------------------------------------------
y.ne.d = as.integer(data.d$Nordeste)
y.n.d = as.integer(data.d$Norte)
y.s.d = as.integer(data.d$Sul)
y.se.d = as.integer(data.d$Sudeste)
y.co.d = as.integer(data.d$Centro.Oeste)

y.ne.d.ts = ts(y.ne.d, start=c(2020,3), frequency = 367)
y.n.d.ts = ts(y.n.d, start=c(2020,3), frequency = 367)
y.s.d.ts = ts(y.s.d, start=c(2020,3), frequency = 367)
y.se.d.ts = ts(y.se.d, start=c(2020,3), frequency = 367)
y.co.d.ts = ts(y.co.d, start=c(2020,3), frequency = 367)

png(".img/data_country_new_deaths.png", width=480, height = 270)
par(mfrow=c(2,3))
plot(y.ne.d.ts, xlab = 'tempo', ylab = 'casos')
title('Nordeste (NE)')
grid()
plot(y.n.d.ts, xlab = 'tempo', ylab = 'casos')
title('Norte (N)')
grid()
plot(y.s.d.ts, xlab = 'tempo', ylab = 'casos')
title('Sul (S)')
grid()
plot(y.se.d.ts, xlab = 'tempo', ylab = 'casos')
title('Suldeste (SE)')
grid()
plot(y.co.d.ts, xlab = 'tempo', ylab = 'casos', )
title('Centro Oeste (CO)')
grid()
dev.off()

acf(y.d.ts)
pacf(y.d.ts)




# PARAIBA (PB) ------------------------------------------------------------
url.c = 'data/confirmed/data_states_new_confirmed.csv'
url.d = 'data/deaths/data_states_new_deaths.csv'

## Novos casos
data.c = read.csv(url.c, header = TRUE)[-1,]
View(data.c)

colnames(data.c)[1] = c('Data')
data.c = data.c[-c(1,2,3,4),]

## Novas mortes
data.d = read.csv(url.d, header = TRUE)[-1,]
View(data.d)

colnames(data.d)[1] = c('Data')
data.d = data.d[-c(1,2,3,4),]

## Serie temporal 
### Novos casos confirmados
y.c = as.integer(data.c$Paraíba)
y.c.ts = ts(y.c, start=c(2020,3), frequency = 367)

png(".img/data_paraiba_new_confirmed.png", width=480, height = 270)
plot(y.c.ts, xlab = 'tempo', ylab = 'casos')
title('Novos casos confirmados na Paraíba (01/03/2020 á 18/03/2021)')
grid()
dev.off()

acf(y.c.ts)
pacf(y.c.ts)

### Novas mortes confirmados
y.d = as.integer(data.d$Paraíba)
y.d.ts = ts(y.d, start=c(2020,3), frequency = 367)

png(".img/data_paraiba_new_deaths.png", width=480, height = 270)
plot(y.d.ts, xlab = 'tempo', ylab = 'mortes')
title('Novas mortes confirmados na Paraíba (01/03/2020 á 18/03/2021)')
grid()
dev.off()

acf(y.d.ts)
pacf(y.d.ts)

