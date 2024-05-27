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
library(RColorBrewer)


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

#Yo lo planteo asi, y abajo el plantemiento de norbey, cualquiera de las dos sirve y DAN LO MISMO
result <- prop.test(table(datos$`Pregunta 3`), p = 0.4, alternative = "greater", conf.level = 0.8)

result

#SOLUCION DE NORBEY
cantidad_data <- length(`data_acoso_result`)
exito_acoso <- sum(data_acoso_result)

result1.1 <- prop.test(x=exito_acoso, n = cantidad_data, p=0.4, alternative = "greater", conf.level = 0.8)

result1.1

#Intervalos de confianza
intervalo_confianza <- prop.test(x=exito_acoso, n = cantidad_data, p=0.4, alternative = "two.sided", conf.level = 0.8)

intervalo_confianza

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

#Intervalo de Confianza

intervalo_confianza_2 <- prop.test(x=c(exito_M, exito_H), n=c(n_mujeres, n_hombres),
                     alternative="two.sided", conf.level = 0.8)

intervalo_confianza_2
  
alpha <- 0.1
if (result2$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}
  
  
  
#5.3 Para comparar la cantidad de veces por persona que ha sido vulnerada dentro de la universidad ICESI.

result3 <- t.test(`Pregunta 7`, mu = 2, alternative = "greater", conf.level = 0.8)
print(result3)


#Intervalo de Confianza
intervalo_confianza_3 <- t.test(`Pregunta 7`, mu = 2, alternative = "two.sided", conf.level = 0.8)
intervalo_confianza_3



alpha <- 0.1
if (result3$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}


#5.4 Para comparar el promedio de veces que son víctimas de robo los hombres y el promedio de veces que son víctimas de robo las mujeres.

#Para que le corran esta, tiene que darle en: "Session/set working directory/" y darle la ruta donde tienen el archivo de excel
datos1= read.xlsx("Datos filtrados, sin babosadas.xlsx",sheet="Hoja1",
                 startRow = 1,colNames = T)

attach(datos1)


result4 <- var.test(x=`Hombres`, y=`Mujeres`, null.value = 1, alternative="two.sided", conf.level=0.8)

result4

alpha <- 0.1
if (result4$p.value < alpha) {
  cat("El valor p es menor que alpha. Se rechaza H0.\n")
} else {
  cat("El valor p no es menor que alpha. No se rechaza H0.\n")
}

t.test(x=Hombres, y=Mujeres, alternative="less",
       mu=0, paired=FALSE, var.equal=TRUE, conf.level=0.8)

#Norbey me dijo que literal con estos 3 codigos ya queda resuelta la hipotesis 

t.test(x=Hombres, y=Mujeres, alternative="two.sided",
       mu=0, paired=FALSE, var.equal=TRUE, conf.level=0.8)

#por el -0.41 por debajo del promedio de las mujeres 0.01 por encima de las mujeres en el robo








##############
#Codigos para graficas

#Saber el % de mujeres y hombres que respondieron la encuesta
genero <- datos %>%
  group_by(`Pregunta 1`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

genero

ggplot(genero , aes(x = `Pregunta 1`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 1`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Genero') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#¿Usted se siente seguro dentro de la Universidad?
seguridad <- datos %>%
  group_by(`Pregunta 2`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

seguridad

ggplot(seguridad , aes(x = `Pregunta 2`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 2`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = '¿Se siente seguro dentro del campus?') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#¿Usted conoce a alguien que ha sido victima de robo en la Universidad?
conocimiento_victima <- datos %>%
  group_by(`Pregunta 3`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

conocimiento_victima

ggplot(conocimiento_victima , aes(x = `Pregunta 3`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 3`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Conocimiento de victima de robo de un 3ero en la universidad') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#¿Cuántas veces ha sido víctima de robo dentro de la universidad? (Digite un número)

victima_robo <- datos %>%
  group_by(`Pregunta 4`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

victima_robo

victima_robo$`Pregunta 4` <- factor(victima_robo$`Pregunta 4`)

generate_random_colors <- function(n) {
  random_palette <- sample(brewer.pal(n = 8, name = "Dark2"), n)
  return(random_palette)
}

# Obtener el número de categorías únicas en `Pregunta 4`
num_categories <- length(unique(victima_robo$`Pregunta 4`))

# Generar colores aleatorios para cada categoría
random_colors <- generate_random_colors(num_categories)

ggplot(victima_robo , aes(x = `Pregunta 4`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 4`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'N° de veces que ha sido victima dentro de la universidad') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = random_colors) 




#¿Usted se ha sentido acosado/acosada  por parte de personas conocidas/desconocidas dentro de la Universidad?

sentimiento_acoso <- datos %>%
  group_by(`Pregunta 5`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)


sentimiento_acoso

ggplot(sentimiento_acoso , aes(x = `Pregunta 5`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 5`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = '¿Se ha sentido acosada?') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())




#¿Usted conoce a alguien que ha sido acosado/acosada por parte de personas conocidas/desconocida dentro de la Universidad?

conocimiento_victima_acoso <- datos %>%
  group_by(`Pregunta 6`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)


conocimiento_victima_acoso

ggplot(conocimiento_victima_acoso , aes(x = `Pregunta 6`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 6`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Conocimiento de un 3ero que hasido acosado') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#¿Cuántas veces te has sentido vulnerado dentro de la Universidad? (Digite un número)

vulnerado <- datos %>%
  group_by(`Pregunta 7`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)


vulnerado

vulnerado$`Pregunta 7` <- factor(vulnerado$`Pregunta 7`)


generate_random_colors2 <- function(n) {
  random_palette2 <- sample(brewer.pal(n = 8, name = "Dark2"), n)
  return(random_palette2)
}

# Obtener el número de categorías únicas en `Pregunta 4`
num_categories2 <- length(unique(vulnerado$`Pregunta 7`))

# Generar colores aleatorios para cada categoría
random_colors2 <- generate_random_colors(num_categories2)


ggplot(vulnerado , aes(x = `Pregunta 7`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 7`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Frecuencia de veces vulnerado') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_manual(values = random_colors2) 



 
#¿Cuál sexo considera usted que es más propenso a ser victima de acoso dentro de la Universidad? 
robos_genero <- datos %>%
  group_by(`Pregunta 8`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

robos_genero


ggplot(robos_genero , aes(x = `Pregunta 8`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 8`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Frecuencia de genero en robos') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#¿Usted esta de acuerdo con la política de puertas abiertas que maneja la Universidad ?


puertas_abiertas <- datos %>%
  group_by(`Pregunta 9`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

puertas_abiertas

ggplot(puertas_abiertas , aes(x = `Pregunta 9`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 9`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Politica de puertas abiertas') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#¿Usted considera que implementar un sistema de entrada por huella digital mejoraria la seguridad dentro la  Universidad?
 

huellas <- datos %>%
  group_by(`Pregunta 10`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

huellas

ggplot(huellas , aes(x = `Pregunta 10`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 10`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Huella digital-seguridad') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())





#¿Usted considera al volver obligatorio mostrar el carnet estudiantil al entrar al campus, mejoraría la seguridad dentro de la Universidad?

carnet <- datos %>%
  group_by(`Pregunta 11`) %>%
  summarize(frec_abs = n()) %>%
  mutate(frec_rel=frec_abs / sum(frec_abs)*100)

carnet

ggplot(carnet , aes(x = `Pregunta 11`, y = frec_abs, label = paste0(frec_abs), color = `Pregunta 11`)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(vjust = 1.5)+
  labs(x= 'Genero', y = 'Frecuencia absoluta', title = 'Carnet obligatorio') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


 