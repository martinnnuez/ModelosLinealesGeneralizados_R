#Practico 20 05 21
setwd("C:/Users/marti/Desktop/Modelos Lineales Generalizados/Practico/datos")

#Ejercicio 9
raza= as.factor(c("blanca","blanca","negra","negra"))
azt= as.factor(c("inmediatamente","mas adelante","inmediatamente","mas adelante"))
sintomas= c(14,32,11,12)
nosintomas= c(93,81,52,43)
prop= (sintomas)/(sintomas+nosintomas)

#Primero concidera interaccion
fit <- glm(cbind(sintomas, nosintomas) ~  azt+raza+azt*raza, family="binomial");
summary(fit2)

anova(fit,test="Chisq")
#veo p valor de la interaccion
# por lo tanto la interaccion no provee nada, nos quedamos con los factores principales raza y azt

fit2 <- glm(cbind(sintomas, nosintomas) ~  azt+raza, family="binomial");
summary(fit2)

#Hace prueba formal
dev2=fit2$deviance

gl2=fit2$df.residual

pvalor2=1-pchisq(dev2,gl2)
pvalor2
#Valor mayor a 0.05, entonces el modelo es adecuado.

#La prueba de wald me dice que solo azt es significativa, me quedo con raza?
#Ajusta el modelo solo usando la raza y luego lo compara con el de usando las dos

fitr <- glm(cbind(sintomas, nosintomas) ~ raza, family="binomial");
summary(fitr)

d=deviance(fitr)-deviance(fit2)
d
#esto la da diferencia de deviance, que esta chequeando la utilidad de la variable azt
qchisq(0.95,1) #punto critico de la chi cuadrado

1-pchisq(d,1)# es significativo el cambio en la deviance por lo tanto la variabl azt aporta al modelo, por lo que debe ser incorporada

#ahora lo hacemos al reves 
fita <- glm(cbind(sintomas, nosintomas) ~ azt, family="binomial");
summary(fita)

d=deviance(fita)-deviance(fit2)
d

1-pchisq(d,1) #Me da no significativo el cambio en la deviance por lo que no seria necesario incorporar la raza al modelo. Nos quedamos con el modelo que solo incluye azt.
#Nos quedamos con el modelo fitr solo con la variable azt

#vamos a interpretar los coeficientes
coef(fita)
exp(coef(fita))
# La chance de desarrollar sintomas de los que reciben el tratamiento mas adelante es 2.06 veces la de los pacientes que reciben el tratamiento inmediatamente.
exp(-coef(fita))# se da vuelta interpretacion
# esto seria toda la parte de interpretacion

# Ahora pide intervalo de confianza
#c

b_2=-0.7218
eeb_2=0.2787
ic_i=b_2-1.96*eeb_2
ic_s=b_2+1.96*eeb_2
icb_2=cbind(ic_i,ic_s)
icb_2
#Ella hace asi

#Problema 12
rm(list=ls());

datos <- read.table("tabaco.txt", header=!F);
attach(datos)

#modelo log lineal de independencia
fit<- glm(conteo ~  grupo+tumor, family="poisson")
summary(fit)
deviance(fit)
fit$df.residual

1-pchisq(deviance(fit),fit$df.residual)
#asi se justifica
# El p valor menor a 0.05 entonces la muestra reune evidencias significativas para rechazar la hipotesis nula, por lo que se acepta la no adecuacion del modelo

#se procede al modelo saturado

fit2<- glm(conteo ~  grupo+tumor+grupo*tumor, family="poisson")
summary(fit2)
deviance(fit2)
fit$df.residual

1-pchisq(deviance(fit2),fit2$df.residual)
#La confianza en la hipotesis nula es muy alta por lo que la muestra no reune evidencais suficientes para rechazar la hipotesis nula por lo que se asume que el modelo es adecuado

b=coef(fit2)
exp(b[4])
#La chance de desarrollar tumor en el grupo tratado o expuesto es 7 veces la chance de desarrollar tumor en el grupo no expuesto
#asi se interpreta


#Planteamos modelo log lineal de poissson como modelo logit binomial
grupo <- c("trat", "control");
presente <- subset(datos, tumor=="presente")$conteo;
ausente  <- subset(datos, tumor=="ausente")$conteo;

fit3<-glm(cbind(presente, ausente) ~ 1, family="binomial")
summary(fit3)
1-pchisq(deviance(fit3),fit3$df.residual)
#El modelo de independencia no es adecuado.

#modelo con interaccion
fit4<-glm(cbind(presente, ausente) ~ grupo, family="binomial")
summary(fit4)
1-pchisq(deviance(fit4),fit3$df.residual)
#El modelo con interaccion es adecuado

exp(coef(fit4))
# da los mismos resultados
# La chance de que tenga un tumor presente es 7.18 veces en el grupo tratado respecto al grupo no tratado.


# Ejercicio 16 datos politomicos
rm(list=ls());

datos <- read.table("polit.txt", header=!F);
attach(datos)

library(VGAM);
#modelo log lineal de independencia
fit<- vglm(cbind(dem,rep,ind) ~  sexo+raza, family="multinomial")
#La ultima categoria en orden que indico es la que queda como referencia
summary(fit)

deviance(fit)
1-pchisq(deviance(fit),2)
#No hay evicencias para rechazar la adecuacion del modleo,ahora vamos a ver si alguna covariable no es necesaria

fit_raza<- vglm(cbind(dem,rep,ind) ~  raza, family="multinomial")
fit_sexo<- vglm(cbind(dem,rep,ind) ~  sexo, family="multinomial")
summary(fit_sexo)

difdev.sexo=deviance(fit_sexo)-deviance(fit)
difdev.raza=deviance(fit_raza)-deviance(fit)

1-pchisq(difdev.raza,2)
1-pchisq(difdev.sexo,2) #este 2 sale de la diferencia de los grados de libertad emtre el modelo con una sola covariable y el con todas

#Entonces ambas variables quedan retenidas en el modelo


# Interpretacion parametros

#Cuando aplico funcion exponencial obtengo los OR
exp(coef(fit))

exp(-coef(fit))

#1= democrata
#sexomasc:1 no es singificativo: da casi 1  quiere decir que la chance de ser democrata para el sexo masculino respecto de la categoria de referencia independencia es l amisma qeu la chance de ser democrata en lugar de independiente para el sexo fememnino fijando la raza constante.

#2=republicano
#sexomasc:2 la chance de ser republicano en lugar de independietne para el sexo masculino es 1.4 veces la chance de ser republicano en lugar de independiente para el sexo femenino manteniendo fija la raza. 

#razanegra:1 la chance de ser democrata en lugar de independiente para la raza negra es 3 veces la chance de ser democrata respecto de independiente para la raza blanca manteniendo fijo el sexo. 

# para los que dan menor de 1 hace el exp de - coef e interpreta de la otra forma.

# - razanegra:2 La chance de ser republicano (estoy en categoria 2) en lugar de independiente para la raza BLANCA es aproximadamente 3.2 veces la chance de ser republicano en lugar de independiente para la raza NEGRA fijado el sexo. 