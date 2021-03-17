### Projeto 01 #################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais
#setwd("/home/manuel/Área de trabalho/UFPB/ST/R/project/R")
getwd()

# BRASIL (BR) -------------------------------------------------------------
# Importando os dados
url.c = 'data/confirmed/data_brasil_new_confirmed.csv'
url.d = 'data/deaths/data_brasil_new_deaths.csv'

## Novos casos
data.c = read.csv(url.c, header = TRUE)[-1,]
View(data.c)
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

png(".img/data_region_new_confirmed.png", width=480, height = 270)
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

png(".img/data_region_new_deaths.png", width=480, height = 270)
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
