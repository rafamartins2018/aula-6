#Aula 6 - Modelos ARMA

#Instalando os pacotes necess�rios -Instalar somente caso n�o tenha instalado anteriormente
install.packages("readxl")     #Instala o pacote Readxl
install.packages("ggplot2")


#Carregando os Pacotes
library(readxl)                 #Carrega o pacote Reaxl
library(ggplot2)


#Comandos da Aula

IPCA <- read_excel("c:/EconometriaA/IPCA.xls")
IPCA <- IPCA[,-1]                                     #Executar apenas uma vez
Inflacao <- ts(IPCA$IPCA, start = 2008-01, frequency = 12)
View(Inflacao)
plot(Inflacao, main="�ndie de Pre�os ao Consumidor Amplo")      #Novo comnando de criar gr�fico, inerente ao pacote ggplo2
Resumo_Estat�stico <- summary(Inflacao)
Resumo_Estat�stico

acf(IPCA)
pacf(IPCA)

AR1 <- arima(Inflacao, order = c(1,0,0))  #Gera a regress�o autoregressiva de ordem um, nomeada aqui de AR1    
AR1                                       #Exibe os resultados da Regress�o

MA3 <- arima(Inflacao,order=c(0,0,3))
MA3

ARMA13 <- arima(Inflacao,order = c(1,0,3))
ARMA13

Box.test(ARMA13$residuals,lag = 3, type = "Ljung")    #Teste de Ljung-Box para autocrrela��o de erros

TesteJB13 <- Box.test(ARMA13$residuals,lag = 3, type = "Ljung")
TesteJB

MA1 <- arima(Inflacao,order = c(0,0,1))
MA2 <- arima(Inflacao,order = c(0,0,2))
MA3 <- arima(Inflacao,order = c(0,0,3))
TesteJB1 <- Box.test(MA1$residuals,lag = 3, type = "Ljung")
TesteJB1
TesteJB2 <- Box.test(MA2$residuals,lag = 3, type = "Ljung")
TesteJB2
TesteJB3 <- Box.test(MA3$residuals,lag = 3, type = "Ljung")
TesteJB3

P_Valores <- c(TesteJB1$p.value,TesteJB2$p.value,TesteJB3$p.value)   #Cria um vetor(uma coluna de dados) com os valores p dos testes
Modelos <- c("MA1","MA2","MA3")                                      #Cria um vetor(uma coluna de dados) com os nomes dos modelos
Resultados <- data.frame(Modelos,P_Valores)
View(Resultados)

