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
install.packages("plotly")

#Importacion de Excel
datos <- read_excel("C:/Icesi/Semestre5/Inferencia/Proyecto Final/Datos filtrados, sin babosadas.xlsx")
View(datos)
attach(datos)

#Saber el % de mujeres y hombres que respondieron la encuesta
genero <- datos %>%
  group_by(`Pregunta 1`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

genero

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

result <- prop.test(table(datos$`Pregunta 5`), p = 0.4, alternative = "greater")

result


