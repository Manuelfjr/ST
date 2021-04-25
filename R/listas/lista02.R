### Lista 02 ###################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais
set.seed(261100)
n=100
# Observações não correlacionadas a partir da distribuição N (0, 1) --------
xt = rnorm(n,0,1)
plot.ts(xt,xlab="tempo",ylab="observacoes")
par(mfrow=c(2,2))
acf(xt,
    main=expression(paste(X[t], '~', N(0,1), ' - acf')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(xt,
     main=expression(paste(X[t], '~', N(0,1), ' - pacf')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(xt,
        main=expression(paste(X[t], '~', N(0,1), ' - Serie Temporal')),
        xlab="tempo",ylab="observacoes")
grid()
# Perceba que a serie é estacionaria, ou seja, podemos notar uma variância constante
# e que a série esta centrada em 0. Além disso podemos observar que 
# as autocorrelações das defasagens encontram-se proximas de zero, não saindo do
# intervalo calculado.


# xt = xt−1 + et -----------------------------------------------------------
et = rnorm(n,0,0.1)
xt = cumsum(et)
par(mfrow=c(2,2))
acf(xt,
    main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(xt,
     main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t], '(pacf)')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(xt,
        main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
PP.test(xt)

# Note que, ao olharmos a serie temporal do passeio aleatorio explicitado,
# podemos observar um comportamento não estacionario, não apresentando uma variância
# constante e nem centrado em torno de uma média. Além disso, ao analisarmos a
# função de autocorrelação, podemos observar um decaimento expoencial, e para com
# a função de autocorrelação parcial, observa-se valores proximos de zero.

# Por fim, ao realizarmos o teste de Phillips-Perron, para testarmos hipotese
# de não estacionariedade, considerando um nivel de significancia de 5%, não 
# rejeitamos a hipotese nula, ou seja, não há evidências suficientes para afirmar
# que a serie é estacionaria.

# xt = 0.7xt−1 +et --------------------------------------------------------

xt = arima.sim(n=n,list(ar=0.7))
par(mfrow=c(2,2))
acf(xt,
    main=expression(paste(X[t], ' = ', 0.7*X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(xt,
     main=expression(paste(X[t], ' = ', 0.7*X[t-1], ' + ', e[t], '(pacf)')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(xt,
        main=expression(paste(X[t], ' = ', 0.7*X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
PP.test(xt)
# Considerando o modelo autoregressivo proposto, AR(1), podemos observar 
# um comportamento estacionario, ou seja, o modelo apresenta uma variancia
# constante ao decorrer do tempo, e é centrado em uma constante, oscilando sobre 
# ela. Além disso, ao analisarmos o gráfico referente a função de autocorrelação
# podemos observar um decaimento exponencial das autocorrelações ao longo 
# das defasagens.

# Por fim, a um nivel de significancia de 5%, utilizando o teste Phillips-Perron,
# rejeitamos a hipótese, ou seja, há evidências suficientes para afirmar que 
# a serie é estacionaria.


# xt = −0.8xt−1 +et -------------------------------------------------------
xt = arima.sim(n=n,list(ar=-0.8))
par(mfrow=c(2,2))
acf(xt,
    main=expression(paste(X[t], ' = ', -0.8*X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(xt,
     main=expression(paste(X[t], ' = ', -0.8*X[t-1], ' + ', e[t], '(pacf)')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(xt,
        main=expression(paste(X[t], ' = ', -0.8*X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
PP.test(xt)
# Ao analisarmos a serie temporal informada, podemos observar um comportamento 
#  estacionario, apresentando uma variancia  constante ao longo do tempo, 
# mas oscilando em certos periodos, mesmo aparentando um valor esperado centrado em 
# uma constante.

# Além disso, ainda analisando o modelo autoregressivo, AR(1), podemos perceber 
# uma função de aucorrelação com um decaimento exponencial das autocorrelações ao
# longo das defasagen. Dessa forma, analisando a função de autocorrelação parcial
# podemos observar um comportamento proximo ao zero ao longo das defasagens.

# Por fim, a um nivel de significancia de 5%, rejeitamos a hipótese nula de que
# a serie temporal não apresenta um comportamento estacionario ao longo do tempo.

# xt = xt−1 + et + 1 -----------------------------------------------
et = rnorm(n,0,1)
xt=cumsum(et + 1)
yt = ts(xt,start=c(1954,2),frequency=12)
par(mfrow=c(2,2))
acf(yt,
    main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t] + 1 , '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(yt,
     main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t] + 1, '(pacf)')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(yt,
        main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t] + 1, ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
plot.ts(diff(yt),
        main=expression(paste(X[t], ' = ', X[t-1], ' + ', e[t] + 1, ' (Serie temporal diff)')),
        xlab="tempo",ylab="observacoes")
grid()

PP.test(yt)
# Considerando o passeio aleatorio explicitado, podemos observar uma variancia
# não constante, das observações ao longo do tempo. Além disso, podemos notar que 
# o modelo não esta centrado em uma média, pelo contrario, apresenta um comportamento
# similar a uma reta crescente.

# Note que, ao analisarmos o grafico de autocorrelação, é possivel ver um decaimento não
# exponencial das autocorrelações, ao longo das defasagens. Além disso, para o gráfico
# referente a função de autocorrelação parcial, é possivel notar autocorrelações
# proximas de zero.

# Considerando o teste de Phillips-Perron, a um nivel de significancia de 5%, nãorejeitamos
# a hipótese nula de que a serie é não estacionaria.

PP.test(diff(yt))
# Perceba que ao realizarmos a diferença da serie, com d=1, podemos perceber uma correçaõ
# na serie, transformando-a em estacionaria. Podemos confirmar ao analisar o teste de Phillips-Perron,
# a um nivel de significancia de 5%, rejeitamos a hipotese nula, ou seja, a serie diferenciada
# apresenta estacionariedade.

# n = 200 -----------------------------------------------------------------
y = arima.sim(n=n+100, list(ar=0.8))
y1 = arima.sim(n=n+100, list(ar=-0.8))
par(mfrow=c(2,2))
plot(y,
     main=expression(paste(Y[t], ' = ', 0.8*Y[t-1], ' + ', e[t], ' (Serie temporal)')))
acf(y,
    main=expression(paste(Y[t], ' = ', 0.8*Y[t-1], ' + ', e[t], ' (acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
plot(y1,
     main=expression(paste(Y[t], ' = ', -0.8*Y[t-1], ' + ', e[t], ' (Serie temporal)')))
acf(y1,
    main=expression(paste(Y[t], ' = ', -0.8*Y[t-1], ' + ', e[t], ' (acf)')),
    xlab="defasagem",ylab="autocorrelacoes")

# filter ------------------------------------------------------------------
# A) yt = 1/3(et−1 +et +et+1)
et = rnorm(500,0,1)
yt=numeric(500)
for (i in 2:(length(yt)-1)){
  yt[i] = (1/3)*(et[i-1] + et[i] + et[i+1])
}
plot.ts(yt)
filter
# B) yt = yt−1 −0.9yt−2 +et
et = rnorm(500,0,1)
yt=numeric(500)
yt[1] = et[1]; yt[2] = et[2]
for (i in 3:(length(yt))){
  yt[i] = yt[i-1] - 0.9*yt[i-2] + et[i]
}
plot.ts(yt)

# 3) ----------------------------------------------------------------------
n=500
#AR(1)
xt = arima.sim(n=n,list(ar=0.9))
par(mfrow=c(2,2))
plot.ts(xt,
        main=expression(paste(X[t], ' = ', 0.9*X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
acf(xt,
    main=expression(paste(X[t], ' = ', 0.9*X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
xt = arima.sim(n=n,list(ar=-0.9))
plot.ts(xt,
        main=expression(paste(X[t], ' = ', -0.9*X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
acf(xt,
    main=expression(paste(X[t], ' = ', -0.9*X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()

# MA(1)
xt = arima.sim(n=n,list(ma=0.9))
par(mfrow=c(2,2))
plot.ts(xt,
        main=expression(paste(X[t], ' = ', 0.9*e[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
acf(xt,
    main=expression(paste(X[t], ' = ', 0.9*e[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
xt = arima.sim(n=n,list(ar=-0.9))
plot.ts(xt,
        main=expression(paste(X[t], ' = ', -0.9*e[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
acf(xt,
    main=expression(paste(X[t], ' = ', -0.9*e[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()


a <- arima.sim(n = 100, list(ma=0.9))
par(mfrow=c(1,2))
plot(a)
plot(lag(a,1))
plot(a, lag(a,1))
# xt = 3xt−1 +et -------------------------------------------------------
xt = numeric(200)
et = rnorm(200)
for (i in 2:200){
  xt[i] = 3*xt[i-1] + et[i]
}
plot.ts(xt)
par(mfrow=c(2,2))
acf(xt,
    main=expression(paste(X[t], ' = ', 3*X[t-1], ' + ', e[t], '(acf)')),
    xlab="defasagem",ylab="autocorrelacoes")
grid()
pacf(xt,
     main=expression(paste(X[t], ' = ', 3*X[t-1], ' + ', e[t], '(pacf)')),
     xlab="defasagem",ylab="autocorrelacoes")
plot.ts(xt,
        main=expression(paste(X[t], ' = ', 3*X[t-1], ' + ', e[t], ' (Serie temporal)')),
        xlab="tempo",ylab="observacoes")
grid()
PP.test(xt)
# Ao analisarmos a serie temporal informada, podemos observar um comportamento 
#  estacionario, apresentando uma variancia  constante ao longo do tempo, 
# mas oscilando em certos periodos, mesmo aparentando um valor esperado centrado em 
# uma constante.

# Além disso, ainda analisando o modelo autoregressivo, AR(1), podemos perceber 
# uma função de aucorrelação com um decaimento exponencial das autocorrelações ao
# longo das defasagen. Dessa forma, analisando a função de autocorrelação parcial
# podemos observar um comportamento proximo ao zero ao longo das defasagens.

# Por fim, a um nivel de significancia de 5%, rejeitamos a hipótese nula de que
# a serie temporal não apresenta um comportamento estacionario ao longo do tempo.
