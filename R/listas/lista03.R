### Lista 03 ###################################################################
# Aluno: Manuel Ferreira Junior
# Disciplina: Series Temporais
library(urca)
# DUVIDA 1 - SOBRE DEFASAGENS*************
  
  ##### defasagens
  
  #gerando modelo AR(1), \phi=0.7, tamanho da série igual a 100.
y =arima.sim(list(order = c(1,0,0), ar = 0.7), n = 100)
y = numeric(100)
yt = numeric(100)
et = rnorm(100)
for (i in 2:100){
  y[i] = 0.5*y[i-1] + et[i]
  yt[i] = 1*yt[i-1] + et[i]
}
par(mfrow = c(1,2))
plot(y[3:100], y[1:98])
plot(yt[3:100], yt[1:98])

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

### Agora vamos encontrar as correlações
#tamanho da série
n=length(y)
n
#inicio da serie
start.y=1
start.y

#correlação	(*)
cor(y[1:99], y.1[2:100])
cor(y[1:98], y.2[3:100])
cor(y[1:97], y.2[4:100])
cor(y[1:96], y.2[5:100])
cor(y[1:95], y.2[6:100])

#plot (**)
pacf(y)
#
acf(y)$acf
# valores das correlações (***)
pacf(y)$acf

# agora, pessoal, olhem os valores das correlações que
# estão em (*) e compare com os valores de (**) e (***)

# Notem ainda, pessoal, que os valores que estão na
# linha pontilhada de (**) são os limites de confiança (aproximados)
# de 95%  (mais ou menos 2/sqrt(tamanho.da.serie)).
# Nesse exemplo, temos mais ou menos 2/sqrt(100)
  
  
  
# *********DUVIDA 2 - SOBRE GERAÇÃO DE MODELOS AR/MA/ARMA *************
    
    
arima.sim(n = 100, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)))
  
  
  
# *********DUVIDA 3 - modelo MA(2) -  covariância *************
    
#    (enviarei as contas)
  
  