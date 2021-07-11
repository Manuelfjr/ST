### Lista 01 ###################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais

set.seed(0)
# Dúvida: Quando não informado valor inicial, começamos de 0 ?!
# Questao 01 --------------------------------------------------------------
e = rnorm(1000, 0, 1)
x = cumsum(e)
plot.ts(x, xlab = 'tempo', ylab = 'observações')


# Questão 02 --------------------------------------------------------------

yt = numeric(30)
yt[3] = 20
for (i in 4:30){
  yt[i] = yt[i-1]*0.8
}

yt_ts = ts(yt,start=c(1988,1),frequency=1)
plot.ts(yt_ts, xlab = 'tempo', ylab = 'observações')

# Questão 03 --------------------------------------------------------------

et = rnorm(60)
yt = cumsum(et + 1)
yt_ts = ts(yt, start=c(2000,1), frequency=12)

plot(yt_ts, xlab = 'tempo', ylab = 'observação')
# Add
z = diff(yt_ts)
plot(z,ylab="Observações", xlab="Tempo")

# Questão 04 (Dúvida: ACF)--------------------------------------------------------------

et = rnorm(100)
yt = cumsum(et)
plot.ts(yt)
# Add
acf(yt,main="",xlab="defasagem",ylab="autocorrelacoes")

# Questão 05 (Dúvida: ACF)--------------------------------------------------------------

et = rnorm(100, sd=0.1)
yt = cumsum(et)
plot.ts(yt)

#Add
acf(yt,main="",xlab="defasagem",ylab="autocorrelacoes")


# Questão 06 (Dúvida: Arima)--------------------------------------------------------------

et = rnorm(100)
yt = numeric(100)
yt[1] = et[1]
for (k in 2:100){
  yt[k] = 0.7*yt[k-1] + et[k]
}
yt = ts(yt)
plot.ts(yt,xlab='tempo', ylab='observação')
acf(yt,main='',xlab="defasagem",ylab="autocorrelacoes")

# Add
y=arima.sim(n=100,list(ar=0.7))
plot.ts(y,xlab="tempo",ylab="observacoes")
acf(y,main='',xlab="defasagem",ylab="autocorrelacoes")
