
#carregar os dados
dados = read.csv('C:/Users/Amauri/Downloads/data/healthy_vs_laryngeal_disorder_classification.csv')

#teste de normalidade
shapiro.test(dados$Irregularity)

#carregar pacotes
pacman::p_load('glm','MASS', 'hnp','car','ggplot2', 'tidyverse')

#criação do modelo
modelo1 = glm(data = dados[,],formula = Irregularity ~ . -idx -SD_F0 -Max_F0 -mean_F0 -Min_F0-GNE - Diagnosis - Shimmer,family = Gamma(link= 'log'))
summary(modelo1)

#correlações
r = cor(dados[2:7])
#Grafico de correlação
corrplot::corrplot(r,type = 'upper')

#envelope
hnp(modelo1$residuals,sim = 99,resid.type = 'deviance',how.many.out = T,conf = 0.95,scale = T, main= 'Envelope do Modelo')


#graficos de diagnóstico
influenceIndexPlot(modelo1)

marginalModelPlots(modelo1)

influencePlot(modelo1)

#Grafico de predições
library(performance)
check_model(modelo1, theme  = "theme_minimal")[]

# Verificar multicolinearidade usando VIF
valorVIF <- car::vif(modelo1)
print(valorVIF)

#Gráfico de densidade
ggplot(data = dados, aes(x = Irregularity)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Gráfico de Densidade da irregularidade da voz",
       x = "Irregularidade na voz",
       y = "Densidade")+
  theme_minimal()

#resumo dos dados
glimpse(dados[2:7])
summary(dados[2:7])

#valores interpretaveis
exp(modelo1$coefficients)

#novo modelo 

modelo2 = glm(data = dados[-16,],formula = Irregularity ~ . -idx -SD_F0 -Max_F0 -mean_F0 -Min_F0-GNE - Diagnosis - Shimmer,family = Gamma(link= 'log'))
summary(modelo2)

#novo envelope
hnp(modelo2$residuals,sim = 99,resid.type = 'deviance',how.many.out = T,conf = 0.95,scale = T, main= 'Envelope do Modelo')

#graficos de diagnóstico
influenceIndexPlot(modelo2)

marginalModelPlots(modelo2)

influencePlot(modelo2)

#exponencial dos coeficientes
exp(modelo2$coefficients)

#simulações
check_model(modelo2)[]
