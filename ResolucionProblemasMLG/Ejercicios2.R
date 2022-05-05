library(boot);
library(car);
library(GGally);
library(ggplot2);
# library(MASS);

setwd("~/Dropbox/Maestria/MLG/Practicos/");

###### Ejercicio 1
{
rm(list=ls());

datos <- read.table("datos/concreto.txt", header=!F);
attach(datos);

#### Inciso a
# ggpairs(datos);

#### Inciso b

fitglm <- glm(porosidad ~ peso); # por defecto es Normal con gc i.e. glm == lm
summary(fitglm);
# Uso distribucion Normal y por lo tanto n=g(u)=u

#### Inciso c
# Nota: all(fitglm$y - fitglm$fitted.values == fitglm$residuals)
# los residuos del fit son los crudos yi - ui

# plot(peso, porosidad);
# abline(fitglm$coef);

# Grafico de los valores predichos vs los observados
# plot(fitglm$fitted.values, porosidad);
# abline(0,1);
# Los valores predichos se observan bastante cercanos a la recta

# Grafico residuos estandarizados (de deviance) vs predichos
# plot(fitglm$fitted.values, glm.diag(fitglm)$rd);
# No se observa patron alguno

# Grafico residuos vs indice (u orden)
# plot(1:nrow(datos), glm.diag(fitglm)$rd);
# No se observa patron alguno

# Grafico residuos vs variable incluida en el modelo
# plot(peso, glm.diag(fitglm)$rd);
# No se observa patron alguno


# Adicionalmente, veamos la normalidad de los residuos de deviance
# qqnorm(glm.diag(fitglm)$rd); # residuos estandarizados de la deviance
# abline(0,1);

# Ho: Es normal
ks.test(glm.diag(fitglm)$rd, "pnorm"); # evaluamos normalidad de residuos
shapiro.test(glm.diag(fitglm)$rd); # evaluamos normalidad de residuos

#### Inciso d
xi <- 113;
if (range(peso)[1] <= xi && xi <= range(peso)[2]) {
    print(fitglm$coef[1] + fitglm$coef[2] * xi);
}
}

###### Ejercicio 2
{
rm(list=ls());

datos <- read.table("datos/psico.txt", header=!F);
attach(datos);

fitglm <- glm(y ~ edad);
summary(fitglm);
# Veamos si se rechaza la adecuacion del modelo
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglm);
# [1] 0.4334701 Resulta adecuado

# Grafico de los valores predichos vs los observados
# plot(fitglm$fitted.values, y);
# abline(0,1);
# Pareciera que los datos no ajustan del todo, se nota una tendencia cuadratica
}

###### Ejercicio 3
{
rm(list=ls());

# Es un modelo para datos binarios, con datos agrupados.
datos <- read.table("datos/peretroide.txt", header=!F); # sexo 1 == Macho
datos[datos$sexo == 1, "sexo"] <- "Macho"
datos[datos$sexo == 0, "sexo"] <- "Hembra"

attach(datos);
prop <- muertos / total;
datos$prop <- prop;

# ggp <- ggplot();
# ggp <- ggp + geom_point(data=datos, aes(x=dosis, y=prop, color=as.factor(sexo))); ggp

log2d <- log(dosis, 2);
datos$log2d <- log2d;
# ggp <- ggplot();
# ggp <- ggp + geom_point(data=datos, aes(x=log2d, y=prop, color=as.factor(sexo))); ggp
# Ahora estan mas lineales

fitglm <- glm(cbind(muertos, total-muertos) ~ log2d+sexo, family="binomial");
# se pide regresion logistica, lo cual es binomial con enlace logit (canonico)

# Veamos si se rechaza la adecuacion del modelo
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglm);
# [1] 0.6623957
# es mayor a 0.05 por lo tanto es adecuado

# Veamos como ajustan los datos y hacemos algunos graficos de diagnostico.
# plot(log2d, prop)
# lines(log2d, fitglm$fitted.values)
# Se nota que ajustan bastante bien los datos

# Grafico de los valores predichos vs los observados
# plot(fitglm$fitted.values, prop);
# abline(0,1);
# Los valores predichos se observan bastante cercanos a la recta

# Grafico residuos estandarizados (de deviance) vs predichos
# plot(fitglm$fitted.values, glm.diag(fitglm)$rd);
# No se observa patron alguno

# Grafico residuos vs indice (u orden)
# plot(1:nrow(datos), glm.diag(fitglm)$rd);
# No se observa patron alguno

summary(fitglm);
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.4732     0.4685  -7.413 1.23e-13 ***
# log2d         1.0642     0.1311   8.119 4.70e-16 ***
# sexoMacho     1.1007     0.3558   3.093  0.00198 ** 
# ---
## (Dispersion parameter for binomial family taken to be 1) --> esta es la estimacion de ø (fi), en binomial es siempre 1
# 
#     Null deviance: 124.8756  on 11  degrees of freedom
# Residual deviance:   6.7571  on  9  degrees of freedom
# AIC: 42.867

# cada Pr(>|z|) es lo mismo que hacer
# lht(fitglm,c(0,1,0),test="Chisq")

# La chance de morir siendo Macho es mayor a la de siendo hembra en
exp((fitglm$coefficients)[3]);
# sexoMacho 
#    3.0064
# Por cada Hembra, mueren 3 Machos
}

###### Ejercicio 4
{
rm(list=ls());

datos <- read.table("datos/trampas.txt", header=!F);
attach(datos);

fitglm <- glm(conteo ~ color+sexo, family="poisson");

# Veamos si se rechaza la adecuacion del modelo
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglm);
# [1] 0.9717516
# es mayor a 0.05 por lo tanto es adecuado, lo que indica que color y sexo son independientes

# g(u) = log(u)  = n = Xb = X * [bo color sexo]
# .:. u = exp(Xb)
summary(fitglm);
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      3.46216    0.14533   23.82  < 2e-16 ***
# coloranaranjada -0.62225    0.07644   -8.14 3.94e-16 ***
# sexomacho        2.66496    0.14775   18.04  < 2e-16 ***
# ---
#     Null deviance: 750.888715  on 3  degrees of freedom
# Residual deviance:   0.001254  on 1  degrees of freedom

# El OR de que sea trampa naranja vs amarilla es
exp((fitglm$coefficients)[2]);
# coloranaranjada 
#       0.5367347
1/exp((fitglm$coefficients)[2]);
# coloranaranjada 
#        1.863118
# Por cada trampa amarilla, tienen exito 0.53 trampas naranjas. Es decir, por cada naranja, tienen exito 1.86 amarillas
}

###### Ejercicio 5
{
rm(list=ls());

datos <- read.table("datos/rio.txt", header=!F);
datos[,1] <- as.factor(datos[,1]);
datos[,2] <- as.factor(datos[,2]);
attach(datos);

fitglm <- glm(conteo ~ temp+sol, family="poisson");
# La independecia de las dos variables categoricas es verdadera sii este modelo es adecuado.
# Veamos si se rechaza la adecuacion del modelo
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglm);
# es mayor a 0.05 por lo tanto es adecuado y por lo tanto las variables categoricas son independientes.

# Pruebas de hipotesis, en este ejemplo vemos si temp1=temp2 y sol1=sol2
Cmatrix <- matrix(c(0,1,-1,0,0, 0,0,0,1,-1), byrow=!F, nrow=2);
gammaVector <- c(0,0);

## todo: terminar
}

###### Ejercicio 6
{
rm(list=ls());

datos <- read.table("datos/tabaco.txt", header=!F);
attach(datos);

# Proporcion con tumor por cada grupo
by(datos, datos$grupo, function(x) x[x$tumor=="presente", "conteo"] / sum(x$conteo));

# Ajustamos un modelo log-lineal de independencia (para ver si es independiente presentar tumor y estar tratado)
fitglmloglin <- glm(conteo ~  grupo+tumor, family="poisson");
# La independecia de las dos variables categoricas es verdadera sii este modelo es adecuado.
# Veamos si se rechaza la adecuacion del modelo
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglmloglin);
# es menor a 0.05 por lo tanto NO es adecuado y por lo tanto las variables categoricas NO son independientes.

# Entonces agregamos la interaccion. Este modelo es saturado y por lo tanto tendre 0 grados de libertad para la deviance.
fitglmloglinInter <- glm(conteo ~  grupo*tumor, family="poisson");

#### Inciso iv
# Modelo logit equivalente al log-lineal SIN interaccion
grupo <- c("trat", "control");
presente <- subset(datos, tumor=="presente")$conteo;
ausente  <- subset(datos, tumor=="ausente")$conteo;

summary(glm(cbind(presente, ausente) ~ 1, family="binomial"));
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   0.9808     0.3028    3.24   0.0012 **
# ---
# Residual deviance: 7.6349  on 1  degrees of freedom
summary(fitglmloglin);
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     2.1665     0.2824   7.672 1.69e-14 ***
# grupotrat      -0.3302     0.2734  -1.208   0.2270    
# tumorpresente   0.9808     0.3028   3.240   0.0012 ** 
# ---
# Residual deviance:  7.6349  on 1  degrees of freedom

# Modelo logit equivalente al log-lineal CON interaccion
grupo <- c("trat", "control");
presente <- subset(datos, tumor=="presente")$conteo;
ausente  <- subset(datos, tumor=="ausente")$conteo;

summary(glm(cbind(presente, ausente) ~ grupo, family="binomial"));
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   0.3795     0.3599   1.054   0.2917  
# grupotrat     1.9719     0.8229   2.396   0.0166 *
summary(fitglmloglinInter);
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               2.5649     0.2774   9.248   <2e-16 ***
# grupotrat                -1.8718     0.7596  -2.464   0.0137 *  
# tumorpresente             0.3795     0.3599   1.054   0.2917    
# grupotrat:tumorpresente   1.9719     0.8229   2.396   0.0166 *  
}

###### Ejercicio 7
{
rm(list=ls());

datos <- read.table("datos/escarabajos.txt", header=!F);
datos$prop <- datos$muertos / datos$total;
attach(datos);

#### Inciso i
# plot(ldosis, prop);

#### Inciso ii
# Pareciera que es una binomial, ya que hay exito (muere) y fracasos (no muere)
# Asi que vamos a chequear las funciones de enlace, las mas utilizadas en binomial son logit, probit y cloglog
fitglm_logit  <- glm(cbind(muertos, total-muertos) ~ ldosis, family=binomial); # enlace canonico (logit)
fitglm_probit <- glm(cbind(muertos, total-muertos) ~ ldosis, family=binomial(link="probit"));
fitglm_cll    <- glm(cbind(muertos, total-muertos) ~ ldosis, family=binomial(link="cloglog"));

# Veamos si alguno no es adecuado
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(fitglm_logit);
# [1] 0.08145881
adecuacionPval(fitglm_probit);
# [1] 0.1196985
adecuacionPval(fitglm_cll);
# [1] 0.7510816
# No se rechaza ningun modelo. Entonces deberiamos quedarnos con el que mejor ajuste i.e. menor AIC
fitglm_logit$aic;
# [1] 41.43027
fitglm_probit$aic;
# [1] 40.3178
fitglm_cll$aic;
# [1] 33.64448

# Por lo tanto nos quedaremos con enlace cloglog
fitglm <- fitglm_cll;
summary(fitglm);
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -39.572      3.240  -12.21   <2e-16 ***
# ldosis        22.041      1.799   12.25   <2e-16 ***
# Residual deviance:   3.4464  on 6  degrees of freedom

#### Inciso iii
# estimemos la proporcion para ldosis=1.75
1-exp(-exp(c(1, 1.75) %*% fitglm$coefficients))
# [1,] 0.3077323 es la proporcion de escarabajos que mueren con ldosis == 1.75

#### Inciso iv
(log(-log(1-.5)) - fitglm$coefficients[1]) / fitglm$coefficients[2]
#    1.778753 es la ldosis para matar el 50% de los escarabajos y por lo tanto la dosis seria
exp((log(-log(1-.5)) - fitglm$coefficients[1]) / fitglm$coefficients[2]);
#    5.922467 
}

###### Ejercicio 8
## Aca deberia revisar, por que segun el Wood se deberia probar con ck^2 + ck^3. Podria haber chequeado escala con algun grafico.
{
rm(list=ls());

datos <- read.table("datos/ataques.txt", header=!F);
attach(datos);
prop <- si / (si+no);
datos$prop <- prop;

# plot(ck, prop);

#### Inciso i
# Se podria utilizar un modelo binomial. Probemos si ajusta

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

adecuacionPval(glm(cbind(si, no) ~ ck, family="binomial"));
# [1] 5.822516e-05 Se rechaza
adecuacionPval(glm(cbind(si, no) ~ ck, family="binomial"(link="probit")));
# [1] 1.766884e-07 Se rechaza
adecuacionPval(glm(cbind(si, no) ~ ck, family="binomial"(link="cloglog")));
# [1] 9.226312e-14 Se rehaza

# bueno asi que no vamos a poder ajustar con binomial asi como viene, probemos de hacer log(ck) y ver si se ajustan
# plot(log(ck), prop);
adecuacionPval(glm(cbind(si, no) ~ log(ck), family="binomial"));
# [1] 0.6241703 No se rechaza
adecuacionPval(glm(cbind(si, no) ~ log(ck), family="binomial"(link="probit")));
# [1] 0.4265094 No se rechaza
adecuacionPval(glm(cbind(si, no) ~ log(ck), family="binomial"(link="cloglog")));
# [1] 0.006327546 Se rechaza

# Asi que elijamos el que tenga menor AIC de logit y probit
fitglm_logit  <- glm(cbind(si, no) ~ log(ck), family="binomial");
fitglm_probit <- glm(cbind(si, no) ~ log(ck), family="binomial"(link="probit"));

fitglm_logit$aic;
# [1] 33.45305
fitglm_probit$aic;
# [1] 35.56598

fitglm <- fitglm_logit;

summary(fitglm);
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -12.1004     1.3837  -8.745   <2e-16 ***
# log(ck)       2.8289     0.3082   9.180   <2e-16 ***
# Residual deviance:   8.0478  on 10  degrees of freedom

#### Inciso ii
ckEstimar <- 120;
lnck <- log(ckEstimar);

# El p estimado para este valor de ck es
exp(c(1, lnck) %*% fitglm$coefficients) / (1+exp(c(1, lnck) %*% fitglm$coefficients));
# [1,] 0.8674999
}

###### Ejercicio 9
{
rm(list=ls());

datos <- read.table("datos/lagartijas.txt", header=!F);
datos$diam <- as.factor(datos$diam);
datos$altura <- as.factor(datos$altura);
attach(datos);

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

fitglm <- glm(cbind(g, o) ~ luz+diam+altura+hora, family="binomial");
summary(fitglm);
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.1718     0.3657   5.938 2.88e-09 ***
# luzsombra    -0.8473     0.3224  -2.628 0.008585 ** 
# diam         -0.7626     0.2113  -3.610 0.000306 ***
# altura1       1.1300     0.2571   4.395 1.11e-05 ***
# horata       -0.9639     0.2816  -3.423 0.000619 ***
# horate       -0.2271     0.2502  -0.908 0.363984    
# ---
# Residual deviance: 14.205  on 17  degrees of freedom
# AIC: 83.029
adecuacionPval(fitglm);
# [1] 0.6525718 Lo cual indicaria que no hay interaccion entre las variables (?)

}

###### Ejercicio 10
{
rm(list=ls());

datos <- read.table("datos/zanahorias.txt", header=!F);
datos$bloque <- c(rep("bloque1", 8), rep("bloque2", 8), rep("bloque3", 8));
attach(datos);
prop <- danadas / total;
datos$prop <- prop;

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

#### Inciso i
# ggp <- ggplot();
# ggp <- ggp + geom_point(data=datos, aes(x=lndosis, y=prop, color=bloque)); ggp

#### Inciso ii y iii
fitglm <- glm(cbind(danadas, total-danadas) ~ lndosis+bloque, family="binomial");
adecuacionPval(fitglm);
# [1] 0.002936602 No resulta adecuado el modelo.

#### Inciso iv
# Grafico residuos estandarizados (de deviance) vs estimados
# plot(fitglm$fitted.values, glm.diag(fitglm)$rd);
# No se observa patron alguno

#### Inciso v
# El enunciado dice que hay que sacar valores atipicos. Valores con alto Cook o Jackknife se consideran atipicos.
# plot(1:nrow(datos), (glm.diag(fitglm)$res)); # Jackknife
# Pareciera que el valor numero 14 fuera atipico, comprobemoslo con Cook
# plot(1:nrow(datos), (glm.diag(fitglm)$cook)); # Aqui se nota mucho mas atipico

# Removamos el dato 14 y volvamos a ajustar el modelo
datos <- datos[-14,];
attach(datos);

fitglm <- glm(cbind(danadas, total-danadas) ~ lndosis+bloque, family="binomial");
adecuacionPval(fitglm);
# [1] 0.1092213 Ahora el modelo si resulta adecuado.

summary(fitglm);

}

###### Ejercicio 11
{
rm(list=ls());

datos <- read.table("datos/sobrevida.txt", header=!F);
attach(datos);

# Me estaba olvidando que se usa la deviance escalada, para ello tengo que dividir por ø (fi)
adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

#### Inciso ii
fitglm_Norm <- glm(tiempo ~ tipo);
summary(fitglm_Norm);
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   211.59     162.39   1.303   0.1976    
# tipocol       245.82     229.65   1.070   0.2888    
# tipoestom      74.41     246.68   0.302   0.7640    
# tipomama     1184.32     259.08   4.571 2.53e-05 ***
# tipoovar      672.75     317.93   2.116   0.0386 *  
# ---
# (Dispersion parameter for gaussian family taken to be 448273.6)
# Residual deviance: 26448144  on 59  degrees of freedom
adecuacionPval(fitglm_Norm);
# [1] 0.475512 Resulta adecuado el modelo


#### Inciso iii
# Estudiamos la relacion media-varianza
by(datos$tiempo, datos$tipo, function(x) { c(Mean=mean(x), Cuad=mean(x)^2, Cub=mean(x)^3, Var=var(x)) } )
# Pareciera que esta mas cercana la media^2 a la varianza (lo que indicaria que es mas una Gamma que una Normal Inversa

# plot(density(datos$tiempo)); # Se nota que seria dificil ajustar con una normal, da alguna idea de Gamma
fitglm_Gamma <- glm(tiempo ~ tipo, family="Gamma");
adecuacionPval(fitglm_Gamma);
# [1] 0.3423124 # Resulta adecuado. Sin embargo probemos tambien con una Normal inversa

fitglm_NormInv <- glm(tiempo ~ tipo, family="inverse.gaussian");
adecuacionPval(fitglm_NormInv);
# [1] 3.889682e-06 # No resulta adecuada.

fitglm_Norm$aic;
# [1] 1021.26
fitglm_Gamma$aic;
# [1] 918.5006

fitglm <- fitglm_Gamma;

# Habria que hacer pruebas de hipotesis para ver si hay diferencias entre organos
summary(fitglm)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.004726   0.001183   3.994 0.000183 ***
# tipocol     -0.002540   0.001304  -1.948 0.056184 .  
# tipoestom   -0.001230   0.001550  -0.793 0.430779    
# tipomama    -0.004010   0.001204  -3.330 0.001503 ** 
# tipoovar    -0.003595   0.001276  -2.818 0.006563 ** 

# Hagamos pruebas de hipotesis para ver diferencias entre tipos de tumor
# Veamos algunos ejemplos simplemente.
# Veamos si son todos iguales
cMatrix <-  matrix(c(1,-1,0,0,0, 1,0,-1,0,0, 1,0,0,-1,0, 1,0,0,0,-1), ncol=5, byrow=!F); cMatrix
lht(fitglm, cMatrix, test="F");
#   Res.Df Df      F    Pr(>F)    
# 1     63                        
# 2     59  4 6.2711 0.0002833 ***
# Se rechaza Ho por lo tanto hay alguno diferente.

# Veamos si mama y ovario son iguales
cMatrix <-  matrix(c(0,0,0,1,-1), ncol=5, byrow=!F); cMatrix
lht(fitglm, cMatrix, test="F");
# No se rechaza Ho!

# Veamos ahora si mama es 4 veces estomago
cMatrix <-  matrix(c(0,0,4,-1,0), ncol=5, byrow=!F); cMatrix
lht(fitglm, cMatrix, test="F");
#   Res.Df Df      F Pr(>F)
# 1     60                 
# 2     59  1 0.0288 0.8658
# No se rechaza Ho!

# Veamos si mama y ovario son -0.007
cMatrix <-  matrix(c(0,0,0,1,1), ncol=5, byrow=!F); cMatrix
lht(fitglm, cMatrix, c(-0.007), test="F");
#   Res.Df Df      F Pr(>F)
# 1     60                 
# 2     59  1 0.0623 0.8038
# No se rechaza Ho!

}

###### Ejercicio 12
{
rm(list=ls());

datos <- read.table("datos/sida.txt", header=!F);
attach(datos);

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

#### Inciso i
plot(1980+t, casos);

#### Inciso ii
# Se va a ajustar con Poisson ya que son datos de conteos.
fitglm <- glm(casos ~ t, family="poisson");
adecuacionPval(fitglm);
# [1] 1.086916e-12 Se rechaza el modelo. Veamos si se encuentra algun outlier
# plot(fitglm$fitted, glm.diag(fitglm)$rd);
# abline(2,0);
# abline(-2,0);

# Veamos de agregar un termino cuadratico
fitglm <- glm(casos ~ t+I(t^2), family="poisson");
adecuacionPval(fitglm);
# [1] 0.5094652 # Este si resulta adecuado
# plot(fitglm$fitted, glm.diag(fitglm)$rd);
# abline(2,0);
# abline(-2,0);

summary(fitglm);
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.901459   0.186877  10.175  < 2e-16 ***
# t            0.556003   0.045780  12.145  < 2e-16 ***
# I(t^2)      -0.021346   0.002659  -8.029 9.82e-16 ***

# Asi se haria el fit de un x particular
# do.call(c,lapply(1:13, function(x) exp(c(1,x,x^2) %*% fitglm$coefficients)))

}

###### Ejercicio 13
{
rm(list=ls());

datos <- read.table("datos/germinacion.txt", header=!F);
attach(datos);
prop <- y/n;
datos$prop <- prop;

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

#### Inciso i
# ggp <- ggplot(data=datos);
# ggp + geom_boxplot(aes(y=prop, x=raiz, color=variedad));

#### Inciso ii
fitglm <- glm(cbind(y, n-y) ~ raiz+variedad, family="binomial");
adecuacionPval(fitglm);
# [1] 0.00230277 No es adecuado. Sospecho que hay interaccion entre raiz y variedad

# plot(fitglm$fitted, glm.diag(fitglm)$rd);
# abline(2,0);
# abline(-2,0);

# plot(1:nrow(datos), glm.diag(fitglm)$rd); # No se observan datos muy atipicos
# plot(1:nrow(datos), glm.diag(fitglm)$cook); # Aqui se observa que los datos 3 y 15 quizas sean outliers

# Veamos (por curiosidad) que sucede sacando estos dos datos
# aux <- cbind(y, n-y)[-c(3,15),];
# raiz2 <- raiz[-c(3,15)];
# variedad2 <- variedad[-c(3,15)];
# fitglm <- glm(aux ~ raiz2+variedad2, family="binomial");
# adecuacionPval(fitglm);
# [1] 0.03853582 # tampoco es adecuado

# fitglm <- glm(cbind(y, n-y) ~ raiz*variedad, family="binomial");
# adecuacionPval(fitglm);
# [1] 0.01039184 # No pasa nada! :(

# Veamos si hay problemas de superdispersion
# plot(fitglm$fitted, glm.diag(fitglm)$rd); # Hay varios valores superiores a 2
# plot(prop, glm.diag(fitglm)$rd);
# probemos de ajustar un modelo quasibinomial
fitglm <- glm(cbind(y, n-y) ~ raiz+variedad, family="quasibinomial");
adecuacionPval(fitglm);
# [1] 0.4139097 # Este si resulta adecuado

## Recordar que a los residuos ahora hay que ajustarlos dividiendo por fi
summary(fitglm);

}

###### Ejercicio 14
{
rm(list=ls());

datos <- read.table("datos/arbol.txt", header=!F);
datos$sitio <- as.factor(datos$sitio);
attach(datos);

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

fitglm <- glm(muertes ~ sitio+edad, family="poisson");
adecuacionPval(fitglm);

# plot(fitglm$fitted, glm.diag(fitglm)$rd); # Hay valores muy dispersos
# plot(muertes, glm.diag(fitglm)$rd);

# Pareciera que es un caso de superdispersion
fitglm <- glm(muertes ~ sitio+edad, family="quasipoisson");
adecuacionPval(fitglm);
# [1] 0.706677 # Este si ajusta, pero se puede utilizar la aproximacion de esta Deviance a X²??

summary(fitglm);
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.57030    0.35524   1.605  0.11011    
# sitio2      -0.59120    0.26923  -2.196  0.02934 *  
# sitio3       0.68245    0.20878   3.269  0.00129 ** 
# edad         0.30769    0.04339   7.091 2.73e-11 ***
# ---
## (Dispersion parameter for quasipoisson family taken to be 12.45771) # notar que la estimacion de ø fi ya no es 1
# Residual deviance: 2168.8  on 185  degrees of freedom

## Preguntar: parece que un quasipoisson se utiliza e interpreta exactamente igual que un poisson normal

}

###### Ejercicio 15
{
rm(list=ls());

datos <- read.table("datos/dilucion.txt", header=!F);
attach(datos);
prop <- positiva/total;
datos$prop <- prop;

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

fitglm <- glm(cbind(positiva, total-positiva) ~ dilucion, family="binomial");
adecuacionPval(fitglm);
# [1] 0.7774616 # Pareciera adecuado el modelo

# otra opcion seria 
# fitglm <- glm(cbind(positiva, total-positiva) ~ log(dilucion, 2), family="binomial");
# adecuacionPval(fitglm);
# [1] 0.5122579

}

###### Ejercicio 16
{
rm(list=ls());

datos <- read.table("datos/seguros.txt", header=!F);
datos$pa <- as.factor(datos$pa);
datos$va <- as.factor(datos$va);
attach(datos);

# datos$num es mijk

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}
}

###### Ejercicio 17
{
rm(list=ls());

datos <- read.table("datos/parcela.txt", header=!F);
datos$bloque <- as.factor(datos$bloque);
datos$trat <- as.factor(datos$trat);
attach(datos);

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}

# Estudiamos la relacion media-varianza
by(datos[,c("parc1","parc2")], datos$trat, function(x) { x <- c(x[,1], x[,2]); c(Mean=mean(x), Cuad=mean(x)^2, Cub=mean(x)^3, Var=var(x)) } )


}

###### Ejercicio 18
{
rm(list=ls());

datos <- read.table("datos/esr.txt", header=!F);
attach(datos);

adecuacionPval <- function(fitglm) {
	# Ho: El modelo es adecuado
	return(pchisq(fitglm$deviance/summary(fitglm)$dispersion, fitglm$df.residual, lower.tail=F));
}
}
###### Ejercicio 19
{
rm(list=ls());
library(VGAM);

datos <- read.table("datos/polit.txt", header=!F);
attach(datos);

fitglm <- vglm(cbind(dem,rep,ind) ~ sexo+raza, family=multinomial);

summary(fitglm);
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1  0.26996    0.11239   2.402  0.01631 *  
# (Intercept):2 -0.01726    0.12224  -0.141  0.88774    
# sexomasc:1    -0.22019    0.15825  -1.391  0.16412    
# sexomasc:2     0.35257    0.16509   2.136  0.03271 *  
# razanegra:1    1.11829    0.23352   4.789 1.68e-06 ***
# razanegra:2   -1.15985    0.38013  -3.051  0.00228 ** 
# Dispersion Parameter for multinomial family:   1
# Residual deviance: 0.1982 on 2 degrees of freedom
pchisq(0.1982/1, 2, lower.tail=F);
# [1] 0.9056521


}