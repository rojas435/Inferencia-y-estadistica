library(openxlsx)
library(BSDA)
library(nortest)
library(devtools)
install.packages("PASWR")
library(PASWR)

## para la prueba de hipotesis de una varianza se debe instalar

install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
library(stests)

#Importar datos Desde donde se encuentren

#-----------------------Hipotesis para una proporcion--------------------#

attach(datos)

#Prueba de hipotesis

prop.test(x = 50, n = 96, p = 0.4,
          conf.level=0.95, alternative = "greater" )

#Intervalo de confianza

prop.test(x = 50, n = 96, p = 0.4,
          conf.level=0.95, alternative = "two.sided" )

#-------------------Hipotesis para comparar 2 proporciones---------------------#

attach(datos)

#Prueba de hipotesis

prop.test(x=c(10, 7), n=c(36, 60), alternative='greater', conf.level=0.95, p=NULL)

#Intervalo de confianza

prop.test(x=c(10, 7), n=c(36, 60), alternative='two.sided', conf.level=0.95, p=NULL)


#---------------------Hipotesis para una media------------------------#

attach(datos)

#Prueba de hipotesis

t.test(`Pregunta 7`, alternative = "greater", mu=2,conf.level=0.95) 

#Intervalo de confianza

t.test(`Pregunta 7`, alternative = "two.sided", mu=2,conf.level=0.95)

#---------------------Hipotesis para comparar 2 medias------------------------#

#Prueba de igualdad de varianzas (No conocemos sigma poblacional)

var.test(x=Mujeres, y=Hombres, null.value=1,
         alternative="two.sided", conf.level=0.95)

#Las varianzas son iguales (Prueba de 2 medias para varaianzas iguales)

t.test(x=Mujeres, y=Hombres, alternative="greater",
       mu=0, paired=FALSE, var.equal=TRUE, conf.level=0.95)

#Intervalo de confianza

t.test(x=Mujeres, y=Hombres, alternative="two.sided",
       mu=0, paired=FALSE, var.equal=TRUE, conf.level=0.95)





