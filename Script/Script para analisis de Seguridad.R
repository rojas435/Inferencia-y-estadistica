library(tidyverse)
library(dplyr)
library(xts)
library(psych)
library(gmodels)
library(MASS)
library(fitdistrplus)
library(lmtest)
library(fdth)
library(readxl)
library(ggplot2)
library(plotly)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)
library(kableExtra)
library(pastecs)


#Importacion de Excel
datos <- read_excel("C:/Icesi/Semestre5/Inferencia/Proyecto Final/Infrencia/Docs/Datos filtrados, sin babosadas.xlsx")
View(datos)
attach(datos)

#Saber el % de mujeres y hombres que respondieron la encuesta
genero <- datos %>%
  group_by(`Pregunta 1`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

genero

#########
percepcion <- datos %>%
  group_by(`Pregunta 5`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

percepcion

#Lo mismo del genero pero ya graficado.
ggplot(genero , aes(x = `Pregunta 1`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 1`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Frecuencia de genero') +
  theme(axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank())



#¿Cuál sexo considera usted que es más propenso a ser victima de acoso dentro de la Universidad? 
robos_genero <- datos %>%
  group_by(`Pregunta 8`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

robos_genero


ggplot(robos_genero , aes(x = `Pregunta 8`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 8`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Frecuencia de genero') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#Analisis de hipotesis

#Planteamiento de hipotesis
#𝐻0: 𝑃≤ 0.4 − 𝑣𝑠 − 𝐻1: 𝑃 > 0.4

#5.1 Para la proporción de casos de personas que han sido acosadas en la universidad ICESI.

result <- prop.test(table(datos$`Pregunta 3`), p = 0.4, alternative = "greater", conf.level = 0.8)

result


cantidad_data <- length(`data_acoso_result`)
exito_acoso <- sum(data_acoso_result)

result1.1 <- prop.test(x=exito_acoso, n = cantidad_data, p=0.4, alternative = "greater", conf.level = 0.8)

result1.1

alpha <- 0.1
if (result$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}


#5.2 Para comparar la proporción de hombres y mujeres que han sido víctimas de inseguridad dentro de la universidad. 

masculino <- subset(datos, Genero == "Hombre", select = c(num_casos))
femenino <- subset(datos, Genero == "Mujer", select = c(num_casos))



n_hombres <- length(masculino$num_casos)
n_mujeres <- length(femenino$num_casos)
exito_M <- sum(femenino$num_casos)
exito_H <- sum(masculino$num_casos)


result2 <- prop.test(x=c(exito_M, exito_H), n=c(n_mujeres, n_hombres),
                     alternative="greater", conf.level = 0.8)

result2
  
alpha <- 0.1
if (result2$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}
  
  
  
#5.3 Para comparar la cantidad de veces por persona que ha sido vulnerada dentro de la universidad ICESI.

result3 <- t.test(`Pregunta 7`, mu = 2, alternative = "greater", conf.level = 0.8)
  
print(result3)

alpha <- 0.1
if (result3$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}


#5.4 Para comparar el promedio de veces que son víctimas de robo los hombres y el promedio de veces que son víctimas de robo las mujeres.

mujeres <- 35

hombres <- 60

var.test(x= )




alpha <- 0.1
if (result4$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}



