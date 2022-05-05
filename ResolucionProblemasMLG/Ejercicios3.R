library(tidyverse)
# Resolucion de parciales
# Algo.jpg
# Ejercicio 3:
datos <- tibble(
  sobrevivientes=c(107,31,156,84),
  total=c(240,240,240,240),
  longitud=c("corta","corta","larga","larga"),
  momento=c("inmediatamente","primavera","inmediatamente","primavera")#1inmediatamente,0mas adelante
)
attach(datos)
adecuacionPval <- function(fitglm) {
  # Ho: El modelo es adecuado
  return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

#Ajusta modelo
fit1<-glm(cbind(sobrevivientes,(total-sobrevivientes))~longitud+momento,family=binomial) 
summary(fit1)

adecuacionPval(fit1)
# El modelo es adecuado.

# Es adecuado el ajuste realizado:
# El modelo ajustado es adecuado como consecuencia de que se trata de datos en forma de proposiones, donde tenemos como dato los exitos respecto a un total por lo que es valido ajustar un modelo del tipo binomial
# La eleccion de la funcion de enlace es correcta como consecuencia del rango de valores posibles que toma pi y ademas permite interpretar con facilidad los resultados como cocientes de chances a traves de una transformacion, algo que los otros enlaces no permiten.
# Tambien se podria haber optado por un modelo con distribucion poisson como consecuencia:
# Los datos podrian ser tenidos en cuenta como un conteo de estacas sobrevivinetes en el tiempo.
# Se podria haber optado por el enlace log(), que es el canonico como consecuencia del rango de variacion de los valores de la media y la facilidad de interpretacion.

# Como hubiese procedido
# 1) Ajusto modelo de independencia. Veo si es adecuado o no.
fit_indep<-glm(cbind(sobrevivientes,(total-sobrevivientes))~1,family=binomial) 
summary(fit_indep)
adecuacionPval(fit_indep) # El modelo es no adecuado.
# 2) Ajusto modelo de asociacion. Veo si es adecuado.
fit1<-glm(cbind(sobrevivientes,(total-sobrevivientes))~longitud+momento,family=binomial) 
summary(fit1)
adecuacionPval(fit1) # El modelo es adecuado
# 3) Analizo cambio en la deviance al adicionar los terminos.
anova(fit1,fit_indep,test="Chisq") # Cuando el phi es conocido
# Resulta que el modelo mas complejo es el adecuado.
# 4) Aporte de cada variable al modelo.
anova(fit1,test="Chisq")
# La adicion de ambas covariables es significativa.


#Interpretacion resutlados cociente de chances:
exp(coef(fit1))
# La chance de que la estaca sobreviva si la longitud es larga es 2.76 la chance de que la estaca sobreviva si la longitud es corta, manteniendo constantes las demas variables, es decir el tratamiento.

exp(-coef(fit1)) # cambia referencia
# La chance de que una estaca sobreviva si el momento de plantacion es inmediatamente es 4.16 la chance de que una estaca sobreviva si el momento de plantacion es primavera, manteniendo constantes las demas variables, es decir el tratamiento.


# Intervalo de confianza al 95% para el cociente de chances de tratamiento.
# Intervalo de confianza a mano:
level = 0.95
a <- (1 - level)/2
a <- c(a, 1 - a)
fac <- qnorm(a)
ses <- sqrt(diag(vcov(fit1)))
ci <- fit1$coefficients[2] + ses[2] %o% fac
exp_ci <- exp(ci);exp_ci

exp(confint.default(fit1))
# La chance de que una estaca sobreviva si la longitud es larga se encuentra ente (2.08 y 3.67) veces (con un 95% de confianza) respecto a la chance de que una estaca sobreviva si la longitud es corta, manteniendo constantes las demas variables.

# Cambiando a mano los numeros
#Interpretar cociente de chances
b=c(1.0177,-1.4278)
exp(b)
exp(-b)


level = 0.95
a <- (1 - level)/2
a <- c(a, 1 - a)
fac <- qnorm(a)
ses <- 0.1486
# intervalo de confianza para los b
ci <- 1.0177 + ses %o% fac
# interalo de confianza para el cociente de chances:
exp(ci)


#Probabilidad caso particular:
# Probabilidad de sobrevivir en estacas largas plantadas inmediatamente:
b=fit1$coefficients;b
x=c(1,1,0)
n=x%*%b
p=exp(n)/(1+exp(n));p

# La probabilidad de que sobreviva una estaca larga plantada inmediatamente es de 0.67.





#Ejercicio 4:
# Datos:
rm(list=ls());
datos <- tibble(
  aire=c(rep("baja",6),rep("alta",6)),
  trabajo=c(rep("no",3),rep("si",3),rep("no",3),rep("si",3)),
  fumar=rep(c("no","ex","actual"),4),
  rta1=c(158,167,307,26,38,94,94,67,184,32,39,77),
  rta2=c(9,19,102,5,12,48,7,8,65,3,11,48),
  rta3=c(5,5,83,5,4,46,5,4,33,6,4,39),
  rta4=c(0,3,68,1,4,60,1,3,36,1,2,51),
  total=c(177,294,560,37,58,248,107,82,318,42,56,215)
)
attach(datos)

library(VGAM)
fit <- vglm(cbind(rta1,rta2,rta3,rta4)~aire+trabajo+fumar, family=cumulative(parallel = TRUE))
summary(fit)

# Prueba de hipotesis ajuste del modelo:
pchisq(29.1512 ,29 ,lower.tail=F)
#P valor es alto, por lo que la muestra no reune evidencias suficientes para rechazar la hipotesis nula de que el modelo es adecuado a un nivel de significancia del 5%, por lo que el modelo ajustado seria adecuado.

qchisq(0.95,29) # 29.1512 < 42.55, pertenece a la zona de no rechazo. 
# La muestra reune evidencias suficientes a un nivel de significancia del 5% para no rechazar la hipotesis nula, por lo que el modelo es adecuado.

# Procedimiento llevado a cabo:
# 1) Ajuste del modelo
# 2) Prueba hipotesis sobre el ajuste del modelo (residual deviance)
# 3) Prueba hipotesis sobre los b = 0.
# 4) Interpretacion coeficientes como cociente de chances. 

# Interpretacion
# Prueba para cada uno de los coeficientes beta:
# Para todos los coeficientes la muestra reune evidencias suficientes para rechazar la hipotesis nula, por lo que estos se asumen distintos de cero. Exepto el coeficiente para la contaminacion del aire baja.

#Interpretacion
fitted.values(fit)# da indicio de cual es el orden 1 a 4
# 1 menos severo que 4 que es el peor caso de enfermedad.

# Los intercepts son los cutpoints no se interpretan.
exp(coef(fit))
#fumarex 4.27
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es ex fumador es 4.27 veces esa chance que cuando es actualmente fumador.
#fumarno 6.37
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es no fumador es 6.37 veces esa chance que cuando es actualmente fumador.

exp(-coef(fit)) # cambia la categoria de referencia.
# airebaja 0.9614697: cuando hago exp(-coef) 
# airealta 1.0400744:
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando la contaminacion del aire es alta es 1.05 veces esa chance cuando la contaminacion del aire es baja.
# Esto quiere decir que las chances para esta variable son practicamente similares, por lo que se refuerza la idea de que el coeficiente beta para la misma no sea significativamente distinto de cero.

# trabajosi 0.42 cuando hago exp(-coef)
# trabajosi 2.37
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando no hay contaminacion en el trabajo es 2.37 veces esa chance cuando si hay contaminacion en el trabajo.

# HACER GRAFICOS DE RESIDUOS ESTUDENTIZADOS Y LEVERAGES:

# Residuos estandarizados (de deviance) vs valores predichos
plot(fitted.values(fit)[,-4], fit@residuals);
# Si el modelo es adecuado el grafico no deberia mostrar ninguna tendencia (patron nulo), como en este caso.
# No se puede apreciar un patron determinado, quiza pequeña forma de embudo pero imperceptible.


### Residuos estandarizados vs. caso. 
# Permite identificar cual observación tiene cual residuo, entonces buscamos esa directamente y corregimos. 
# Residuos estandarizados (de deviance) vs valores predichos
indice=rep(seq(1,nrow(fit@residuals)),3)
plot(indice, fit@residuals);
# Todos los residuos se encuentran entre 2 y -2, ninguno es demaciado elevado.

# Procedimiento que seguiria:
#### Ajusto modelo de independencia y modelo sin covariable para ver cual es el cambio en la deviance y el que mejora resulta:
fit2 <- vglm(cbind(rta1,rta2,rta3,rta4)~1, family=cumulative(parallel = TRUE))
summary(fit2)
pchisq((383.2075 - 29.1512), (33 - 29),lower.tail=F)
# El cambio en la deviance es significativo al agregar los terminos por lo que se concluye que el modelo con mayor cantidad de parametros es con el que nos debemos quedar.
# Modelo mas complejo es adecuado.

#Hago lo mismo pero sacando la variable no signifcativa, contam aire:
fit3 <- vglm(cbind(rta1,rta2,rta3,rta4)~trabajo+fumar, family=cumulative(parallel = TRUE))
summary(fit3)
pchisq((30.174 - 29.1512), (30 - 29),lower.tail=F)
# El cambio en la deviance es no significativo por lo que nos quedamos con el modelo mas parsimonioso, sin incluir la ultima variable.
# Modelo que no incluye aire.

# Interpreto:
# No cambian practicamente nada las interpretaciones.
exp(coef(fit3))
#fumarex 4.27
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es ex fumador es 4.27 veces esa chance que cuando es actualmente fumador.
#fumarno 6.37
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es no fumador es 6.37 veces esa chance que cuando es actualmente fumador.

exp(-coef(fit3))
# airebaja 0.9614697: cuando hago exp(-coef) 
# airealta 1.0400744:
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando la contaminacion del aire es alta es 1.05 veces esa chance cuando la contaminacion del aire es baja.
# Esto quiere decir que las chances para esta variable son practicamente similares, por lo que se refuerza la idea de que el coeficiente beta para la misma no sea significativamente distinto de cero.

# trabajosi 0.42 cuando hago exp(-coef)
# trabajosi 2.37
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando no hay contaminacion en el trabajo es 2.37 veces esa chance cuando si hay contaminacion en el trabajo.




# Parcial agosto20:
# Mismo ejercicio fumar con una variable menos
# Datos:
datos <- tibble(
  contam_trabajo=c(rep("no",3),rep("si",3)),
  hab_fumar=rep(c("no","ex","actual"),2),
  stat1=c(252,234,491,58,76,171),
  stat2=c(16,27,267,8,23,96),
  stat3=c(10,9,116,6,8,85),
  stat4=c(1,6,104,2,6,111)
  )
attach(datos)

# Ajusto modelo, estamos en presencia de una variable ordinal
library(VGAM)
# Ordinal
fit <- vglm(cbind(stat1,stat2,stat3,stat4)~contam_trabajo+hab_fumar, family=cumulative(parallel = TRUE))
summary(fit)

# Analizis de deviance del modelo ajustado
pchisq(23.1048, 12,lower.tail=F) 
# El p valor es muy bajo, la muestra reune evidencias suficientes para rechazar la hipotesis nula, por lo que el modelo no es adecuado.
# Rechazar la hipotesis nula no quiere decir que las diferencias no existan, si no que la muestra recopilada reune evidencias significativas para rechazar la hipotesis nula. Aumentando el numero de datos o bien tomando otra muestra podria llegarse a la conlucion de que el modelo es adecuado.

# Analizis del test de wald:
# Prueba de hipotesis sobre los coeficientes:
# Ho = B = 0
# H1 = B != 0 
# Todos los coeficientes exepto el intercepto 1 son distintos de cero. 


# Ajustamos modelo de indepdendencia para ver si las variables son independientes:
fit2 <- vglm(cbind(stat1,stat2,stat3,stat4)~1, family=cumulative(parallel = TRUE))
summary(fit2)
pchisq((421.7212), (15),lower.tail=F)
# El p valor es muy bajo por lo que la muestra reune evidencias suficientes para rechazar la hipotesis nula a un nivel de significancia del 5%, por lo que el modelo no es adecuado, es decir que las variables no son independientes.

pchisq((421.7212 - 23.1048), (15 - 12),lower.tail=F)
# El cambio en la deviance es significativo por lo que se concluye que el modelo con mayor cantidad de parametros es adecuado.

# Prueba de hipotesis sobre la diferencia de deviance:
# Ho = La adicion del nuevo termino no aporta a lo que explica el modelo, por lo que no es necesario añadirlo. Cambio en la deviance no es significativo con la adicion del nuevo temrino.
# H1 = La adicion del nuevo termino aporta a lo que explica el modelo, por lo que es necesario añadirlo. Cambio en la deviance es significativo con la adicion del nuevo termino.

# Se concluye que la adicion de los nuevos temrinos aportan a lo que explica el modelo y es necesario añadirlos.


# Interpretacion de los coeficientes:
fitted.values(fit)# da indicio de cual es el orden 1 a 4

exp(coef(fit))
#hab_fumarex 4.83
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es ex fumador es 4.83 veces esa chance que cuando es actualmente fumador.
#hab_fumarno 8.22
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando es no fumador es 8.22 veces esa chance que cuando es actualmente fumador.

exp(-coef(fit))
# contam_trabajosi 0.45: cuando hago exp(-coef) 
# contam_trabajosi 2.19:
# La chance de tener status de enfermedad en la direccion 1 en vez de la 4 cuando no hay contaminacion en el trabajo es 2.37  veces esa chance que cuando es contaminacion en el trabajo.





# Parcial1_06_11_2
#ejercicio 3
datos<- tibble(
  fav = c(34,61,106,99,11,37,23,19,31,62,83,57,51,142,51,64),
  desfav = c(16,24,143,110,6,11,15,25,8,23,94,54,8,37,35,21),
  escolaridad = c(rep(1,8),rep(0,8)),
  claseobr = c(rep(1,4),rep(0,4),rep(1,4),rep(0,4)),
  estable = c(1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0),
  miembro = c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
)

attach(datos)

### Ajuste modelo enlace logit:
fit1<-glm(cbind(fav,desfav)~escolaridad+claseobr+estable+miembro,family=binomial) 
summary(fit1)
### Prueba de hipotesis sobre los beta
#Prueba de hipotesis sobre los coeficientes Pr(>|t|):
#Ho = B = 0
#H1 = B != 0 
#Ambos coeficientes son distintos de 0. 

# Para el modelo de asociasion fit1 todos los coeficientes estimados son significativamente distintos de 0, exepto el coeficiente de miembro

adecuacionPval <- function(fitglm) {
  # Ho: El modelo es adecuado
  return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}
adecuacionPval(fit1);

# Prueba de hipotesis sobre la adecuacion del modelo con deviance escalada:
# Ho = El modelo es adecuado
# H1 = El modelo mas general es adecuado
# La deviance escalada es el estadistico de cociente de verosimilitud para testear la hipotesis nula de que el modelo es adecuado contra la alternativa de que un modelo mas general es el adecuado. 
# 0.168 el modelo resulta adecuado

#Ajusta modelo 2 con interaccion
fit2<-glm(cbind(fav,desfav)~escolaridad+claseobr*estable+miembro,family=binomial) 
summary(fit2)

adecuacionPval(fit2)
# 0.166 modelo adecuado; miembro e interaccion no significativos.

#### Se evalua la adicion de el nuevo termino al modelo:
#Se analiza si el cambio en la deviance es significativo luego de añadir una covariable nueva.
anova(fit2, test="Chisq") # Binomial y poisson
# El cambio en la deviance luego de añadir miembro y la interaccion son no singificativos, por lo tanto su adicion al modelo no tendria sentido.

# Procedimiento que yo hubiese seguido:
# 1) Ajusto modelo de idependencia:
# Modelo de independencia BINOMIAL
fit0 <-glm(cbind(fav,desfav)~1,family=binomial)
summary(fit0)
adecuacionPval(fit0)

#Ho en el modelo de independencia establece que las respuestas favorables y desfavorables son independientes, pero el modelo es no significativo.
# La muestra reune evidencias suficientes para rechazar la hipotesis de que las variables son independientes, por lo que se concluye que no lo son.
# Tiene sentido ajustar un modelo con variables explicativas.

# 2) Ajusto modelo de asociacion luego de rechazar el de independencia
fit1 <-glm(cbind(fav,desfav)~escolaridad+claseobr+estable+miembro,family=binomial)
summary(fit1)
adecuacionPval(fit1)

# El modelo es adecuado pero hay una variable que no es significativa, debemos evaluar si tiene sentido dejarla incluida en el modelo.
# Ajustamos modelo sin esta variable
fit2 <-glm(cbind(fav,desfav)~escolaridad+claseobr+estable,family=binomial)
summary(fit2)
adecuacionPval(fit2)

# 3) Procedemos a analizar si la adicion de miembro genera un cambio significativo en la deviance:
### Diferencia de deviance entre modelos ajustados

anova(fit1,fit2,test="Chisq")

#Prueba de hipotesis sobre la diferencia de deviance:
#Ho = El modelo mas corto es adecuado
#H1 = El modelo mas largo es adecuado
#Se puede chequear la utilidad del modelo M2 (largo), en relacion a M1 (corto) mediante la diferencia de deviance.
#Con un p valor de 0.2792 vemos que el cambio en la deviance luego de la adicion de la variable es no significativo.
# concluimos que la muestra no reune evidencias suficientes para rechazar la hipotesis nula por lo que se concluye que el modelo mas parsimonioso es adecuado.

# 4) Ahora bien si queremos analizar la interaccion la analizamos:
fit3 <-glm(cbind(fav,desfav)~escolaridad+claseobr*estable,family=binomial)
summary(fit3)
adecuacionPval(fit3)
# El termino de interaccion es no singificativo.

# Interpretacion de los parametros en el modelo que concidere mas adecuado de los que ajusto la profe:
fit1<-glm(cbind(fav,desfav)~escolaridad+claseobr+estable+miembro,family=binomial) 
summary(fit1)

# 5) Interpretaciones en terminos de cocientes de chances
exp(fit1$coefficients)
# La chance de tener una vision positiva para personas con trabajo estable es 2.64 veces la chance de tener una vision positiva para personas que no poseen un trabajo estable, manteniendo constantes las demas variables.

exp(-fit1$coefficients) # da vuelta categoria de referencia
# La chance de tener una vision positiva para personas sin escolaridad es 1.33 veces la chance de tener una vision positiva para personas que poseen escolaridad, manteniendo constantes las demas variables.
# La chance de tener una vision positiva para personas no pertenecen a la clase obrera es 1.57 veces la chance de tener una vision positiva para personas que pertenecen a la clase obrera, manteniendo constantes las demas variables.
# La chance de tener una vision positiva para personas que no son miembro es 1.12 veces la chance de tener una vision positiva para personas que son miembro, manteniendo constantes las demas variables.





# Examen MLG PDF
# Ejercicio 3
datos <- tibble(
  si=c(14,32,11,12),
  no=c(93,81,52,43),
  raza=c(1,1,0,0),#1blanca,0negra
  tratamiento=c(1,0,1,0)#1inmediatamente,0mas adelante
)
attach(datos)
adecuacionPval <- function(fitglm) {
  # Ho: El modelo es adecuado
  return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}


#Ajusta modelo
fit1<-glm(cbind(si,no)~raza+tratamiento,family=binomial) 
summary(fit1)

adecuacionPval(fit1)
# El modelo es adecuado.

#Interpretacion resutlados cociente de chances:
exp(coef(fit1))
# La chance de que desarrolle sintomas una persona de raza blanca es 1.05 la chance de que desarrolle sintomas una persona de raza negra manteniendo constantes las demas variables, es decir el tratamiento.
# es decir que la chance de exito se mantiene constante al variar la raza y por ese motivo su coeficiente no es significativamente distinto de 0.

exp(-coef(fit1)) # cambia referencia
# La chance de que una persona que se trato mas adelante desarrolle sintomas es 2.05 veces la chance de que desarrolle sintomas una persona que se trato inmediantamente manteniendo constantes las demas variables.


# Intervalo de confianza al 95% para el cociente de chances de tratamiento.
# Intervalo de confianza a mano:
level = 0.95
a <- (1 - level)/2
a <- c(a, 1 - a)
fac <- qnorm(a)
ses <- sqrt(diag(vcov(fit1)))
ci <- fit1$coefficients[3] + ses[3] %o% fac
exp_ci <- exp(-ci)

exp(-confint.default(fit1))

# La chance de que una persona que se trato mas adelante desarrolle sintomas se encuentra entre (1.18 y 3.54) veces (con una confianza del 95%) respecto a la chance de que desarrolle sintomas una persona que se trato inmediantamente manteniendo constantes las demas variables.





# Parcial noviembre 20:
# Ejercicio 2: Nacimientos massachusetts:

datos<-tibble(
  fumo=c("si","no","si","no","si","no"),
  raza=c("blanca","blanca","negra","negra","otra","otra"),
  bajo_peso=c(19,4,6,5,5,20),
  total=c(52,44,10,16,12,55),
  prop=bajo_peso/total
)
attach(datos)

# A) Describir formalmente el modelo y justificar eleccion.
# 1- Componente aleatoria: 
# La variable aleatoria Y representa: 
# Y= el numero de bebes que nacen con bajo peso en n ensayos independientes
# Es una muestra aleatoria con distribucion B(n,p) de parametros n y p, donde n es el numero total de ensayos independientes y p la probabilidad del exito.
# Es decir que las componentes de Y son independientes e identicamente distribuidas con distribucion B(n,p).
# Ademas sabemos que esta distribucion pertenece a la familia exponencial en la forma canonica.
# 
# 2- Componente sistematico:
#   Las covariables medidas forman un predictor lineal. 
# n = sumatoria desde i=1 hasta n de las xi*bj.
# 
# 3- Funcion de enlace:
# Es la funcion que relaciona la componente aleatoria con la componente sistematica. 
# En este caso se toma como funcion de enlace a la funcion logit(p), que ademas es la funcion de enlace canonica.
# logit(p)=log(p/1-p)=n= sumatoria xi*bj=

# Justifico la eleccion del modelo 
# Es adecuado el ajuste realizado:
# El modelo ajustado es adecuado como consecuencia de que se trata de datos en forma de proposiones, donde tenemos como dato los exitos respecto a un total por lo que es valido ajustar un modelo del tipo binomial
# La eleccion de la funcion de enlace es correcta como consecuencia del rango de valores posibles que toma pi y ademas permite interpretar con facilidad los resultados como cocientes de chances a traves de una transformacion, algo que los otros enlaces no permiten.
# Tambien se podria haber optado por un modelo con distribucion poisson como consecuencia:
# Los datos podrian ser tenidos en cuenta como un conteo de estacas sobrevivinetes en el tiempo.
# Se podria haber optado por el enlace log(), que es el canonico como consecuencia del rango de variacion de los valores de la media y la facilidad de interpretacion.

adecuacionPval <- function(fitglm) {
  # Ho: El modelo es adecuado
  return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

# Ajusto modelo de independencia
fit_indep<-glm(cbind(bajo_peso,total-bajo_peso)~1,family=binomial) 
summary(fit_indep)
adecuacionPval(fit_indep) # Modelo no adecuado

# Ajusto modelo de asociacion
fit1<-glm(cbind(bajo_peso,total-bajo_peso)~fumo+raza,family=binomial) 
summary(fit1)

adecuacionPval(fit1)
# Con un p valor de 0.2 la muestra no reune evidencias suficientes para rechazar la hipotesis nula a un nivel de significancia del 5% por lo que el modelo se asume que es adecuado.
# El test de wald sobre los coeficientes permite concluir que son todos distintos de cero.


# Diferencia de deviance: 
anova(fit1,fit_indep,test="Chisq")
# Prueba de hipotesis sobre la diferencia de deviance:
# Ho = El modelo mas parsimonioso es adecuado. No es necesario añadir mas terminos.
# H1 = El modelo mas complejo es adecuado. Es necesario añadir mas terminos.
# Se puede chequear la utilidad del modelo M2 (largo o complejo), en relacion a M1 (corto o parsimonioso) mediante la diferencia de deviance.
# Con un p valor de 0.002 concluimos que la muestra reune evidencias suficientes para rechazar la hipotesis nula por lo que se concluye que el modelo mas largo o complejo es adecuado.

# Inclucion de variables al modelo
anova(fit1,test="Chisq") # Cuando el phi es conocido
# Prueba de hipotesis sobre la diferencia de deviance:
# Ho = La adicion del nuevo termino no aporta a lo que explica el modelo, por lo que no es necesario añadirlo. Cambio en la deviance no es significativo con la adicion del nuevo temrino.
# H1 = La adicion del nuevo termino aporta a lo que explica el modelo, por lo que es necesario añadirlo. Cambio en la deviance es significativo con la adicion del nuevo termino.

# La muestra reune evidencias suficientes para rechzar la hipotesis nula en ambos casos a un nivel de singificancia del 5% por lo que la adicion de las variables al modelo es necesaria.


# B) Interprete los resultados obtenidos, realizadno estimaciones de cociente de chances:
exp(coef(fit1))
# (Intercept)      fumosi   razanegra    razaotra
# 0.1587319     3.0526312   2.9567424   3.0300008
# La chance de que el bebe nazca con bajo peso si la madre fumo es 3 veces la chance de de que el bebe nazca con bajo peso si la madre no fumo manteniendo fija la raza.
# La chance de que el bebe nazca con bajo peso si la madre es de raza negra es 2.95 veces la chance de que el bebe nazca con bajo peso si la madre es de raza blanca, manteniendo fijo la variable fumo.
# La chance de que el bebe nazca con bajo peso si la madre es de otra raza es 3 veces la chance de que el bebe nazca con bajo peso si la madre es de raza blanca, manteniendo fijo la variable fumo.


# C)Obtenga un intervalo de confianza del 95% para el cociente de chances de un nacimiento con bajo peso en madre fumadora respecto de no fumadora, fija la raza.
# Intervalo de confianza a mano:
level = 0.95
a <- (1 - level)/2
a <- c(a, 1 - a)
fac <- qnorm(a)
ses <- sqrt(diag(vcov(fit1)))
ci <- fit1$coefficients[2] + ses[2] %o% fac
exp_ci <- exp(ci);exp_ci

exp(confint.default(fit1))

# La chance de de que el bebe nazca con bajo peso si la madre fumo se encuentra entre [1.48 ; 6.29] veces la chance de de que el bebe nazca con bajo peso si la madre no fumo con una confianza del 95%, cuando se mantiene fija la raza.
# Es decir que el verdadero valor del parametro se encuentra entre los limites del intervalo con una confianza del 95%, cuandos se mantiene fija la raza.


# D) Estime la probabilidad de que el bebé tenga bajo peso al nacer si la madre no fuma durante el embarazo y la raza es negra.
fit1$coefficients
b<-as.matrix(coef(fit1),nrow=4)
x<-as.matrix(c(1,0,1,0),nrow=4)
n<-t(x)%*%b #predictor lineal no fuma y raza negra

# Esta formula porque es modelo logit
exp(n)/(1+exp(n))
# La probabilidad de que el bebé tenga bajo peso al nacer si la madre no fuma durante el embarazo y la raza es negra es de 0.32.






# Ejercicio 3: tiempo de coagulacion
1-pchisq(2450.2,14)

# B) Interpretacion resultados para gamma complejo:
#Coeficientes modelo gamma
b<-as.matrix(c(-0.0165544,-0.0073541,0.0153431,0.0082561),nrow=4)
rownames(b)<- c("(Intercept)","agente2","log(conc)","agente2:log(conc)");b
conc<-c(5,10,15,20,30,40,60,80,100,5,10,15,20,30,40,60,80,100)
mean(conc) #40.

# Enlace canonico del modelo gamma g(m)=1/m

# Para el agente 1 con la concentracion media, se espera un tiempo promedio de:
x=c(1,0,log(40),0)
1/x%*%b #24.97 segundos

# Para el agente 2 con la concentracion media, se espera un tiempo promedio de:
x=c(1,1,log(40),log(40))
1/x%*%b #15.83 segundos

# Concluimos que la diferencia en el tiempo promedio de coagulacion para la concentracion media entre el agente 1 y 2 es de 24.97225-15.83629 = 9.13596
# Por lo que el tiempo promedio seria mayor para el agente 1 que para el agente 2.

# Cuando la concentracion aumenta en una unidad respecto de la concentracion promedio el tiempo promedio para el agente 1 disminuye en 0.23405.
x=c(1,0,log(40),0)
1/x%*%b #24.97 segundos

x=c(1,0,log(41),0)
1/x%*%b #24.7382 segundos
24.97225-24.7382

# Cuando la concentracion aumenta en una unidad respecto de la concentracion promedio el tiempo promedio para el agente 2 disminuye en 0.1448.
x=c(1,1,log(40),log(40))
1/x%*%b #15.83629 segundos

x=c(1,1,log(41),log(41))
1/x%*%b #15.69149 segundos
15.83629-15.69149

# Concluyo que el tiempo promedio de coagulacion disminuye en mayor proporcion con el agente 1 que con el agente 2 con cambios unitarios de la concentracion promedio.

# Para valores por encima de la conc de 2.39 la media del agente 1 va a ser mayor que la media del agente 2
x=c(1,0,log(2),0)
1/x%*%b #-168.9368 segundos

x=c(1,1,log(2),log(2))
1/x%*%b #-132.4366 segundos
# m ag 2 mayor m ag 1

x=c(1,0,log(5),0)
1/x%*%b #122.8597 segundos

x=c(1,1,log(5),log(5))
1/x%*%b #71.05832 segundos
# m ag 1 mayor a m ag 2

# Rango de valores de concetntracion > a 2.39 concluyo que la media del tiempo de coagulacion para el agente 1 es siempre mayor que para el agente 2.


# c) Prediga el tiempo medio de coagulación de la sangre para una concentración de plasma del 50% para cada uno de los agentes.
# Agente 1
x=c(1,0,log(50),0)
1/(x%*%b)
#23.00534

# Agente 2
x2=c(1,1,log(50),log(50))
1/(x2%*%b)
#14.61729
# El tiempo promedio de coagulacion es mayor para el agente 1 que para el agente 2 cuando la concentracion es de 50.
