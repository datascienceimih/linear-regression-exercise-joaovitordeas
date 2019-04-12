## Atividade Avaliativa do Regressão linear.
## ISLR pag 121.
## Exercicios 8, 9(a/b/c/d), 10, 13, 15.
## João Vitor de Andrade Santos.

# Carrega os dados Auto e o pacote do ISLR
library(ISLR)
data("Auto")

## 8a)
lm.fit <- lm(mpg ~ horsepower, data=Auto)
summary(lm.fit)
## 8a) i - Sim, pois o p valor é inferior a 0.05.

## 8a) ii - O dois apresentam uma boa relação visto que o R² apresenra valor de de 60%.

## 8a) iii - Negativo, visto que o coeficiente é negativo.

## 8a) iv - 

predict(lm.fit, data.frame("horsepower"=98), interval="confidence")

predict(lm.fit, data.frame("horsepower"=98), interval="prediction")

## 8b)
plot(Auto$horsepower, Auto$mpg)
abline(lm.fit, lwd=3, col="red")

## 8c)
par(mfrow=c(2,2))
plot(lm.fit)

## O primeiro plot apresenta um formato de U, com isso a relação do preditor com a resposta
## não é linear.
## No segundo plot mostra que o residual é distribuido normalmente.
## O terceito plot mostra a que variancia de erros e constante.
## O quarto plot mostra que não há pontos de alavancagem.

## 9a)
pairs(Auto)

## 9b) 
cor(Auto[, !(names(Auto)=="name")])

## 9c)
lm.fit <- lm(mpg ~ .-name, data=Auto)
summary(lm.fit)
## 9c) i - Sim, há relação visto que o p valor é menor que 0,05.
## 9c) ii - "Displacement", "weigth", "year" e "origin".
## 9c) iii - Sugere que a cada ano a mais é possivel fazer 0.75 galoes por litro a mais
## que um carro um ano mais novo.

## 9d)
par(mfrow=c(2,2))
plot(lm.fit)
## O primeiro plot não apresenta linearidade com os casos.
## O terceiro plot apresenta alguns Outliers.
## No quarto plot o ponto 14 foge completamente do padrão apresentado pleos outros pontos.

## 10)
data("Carseats")

## 10a)
lm.fit.a <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.fit.a)

##10b)
attach(Carseats)
str(data.frame(Price, Urban, US))

attach(Carseats)
contrasts(Urban)

contrasts(US)

## "UrbanYes" apresenta um p valor muito alto (0,936) e por isso não gera muita relevância
## e não deve ser usando como parâmetro.
## "Price" e "USYes" apresentam p valor menor que 0,5 e por isso seus resultados tem relevância.

##10c)
summary(Carseats)

##10d) Urban.

## 10e) 
lm.fit.e <- lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit.e)

## 10f)
anova(lm.fit.a, lm.fit.e)

## 10g)
confint(lm.fit.e)

## 10h) Nº 43.
par(mfrow=c(2,2))
plot(lm.fit.e)

hatvalues(lm.fit.e)[order(hatvalues(lm.fit.e), decreasing = T)][1]

## 13
set.seed(1)

## 13a)
x <- rnorm(100)

## 13b)
eps <- rnorm(100, 0, 0.25)

## 13c) 100
y = -1 + .5*x + eps
length(y)

##13d)
plot(x,y)+abline(coef = c(-1,0.5), col="red")
## Há uma distribuição bem uniforme dos dados, enquando x cresce, y tambem cresce de maneira parecida

## 13e) 
lm.fit.e <- lm(y ~ x)
summary(lm.fit.e)
## Os coeficientes são proximos dos originais, com isso a relação linear é bem parecida.

## 13f) 
plot(x,y)
abline(lm.fit.e, col="red", lwd=2)
legend(x=-2, y=.25, legend = "Linear Regression Model")

## 13g)
lm.fit.g <- lm(y ~ poly(x,2))
summary(lm.fit.g)
anova(lm.fit.e, lm.fit.g)
## Pelo fato do p valor ser 0.16, não há evidências de diferenças nos modelos.

## 13h)
x <- rnorm(100)
eps <- rnorm(100, 0, .05)
y = -1 + .5*x + eps

plot(x,y)

lm.fit.h <- lm(y ~ x)
summary(lm.fit.h)

abline(lm.fit.h, col="red", lwd=2)
legend(x=-2, y=.25, legend = "Linear Regression Model")
## Os coeficientes são bem proximos do original, mas o std error é menor.

## 13i)
x <- rnorm(100)
eps <- rnorm(100, 0, .5)
y = -1 + .5*x + eps

plot(x,y)

lm.fit.i <- lm(y ~ x)
summary(lm.fit.i)$coefficients

abline(lm.fit.i, col="red", lwd=2)
legend(x=-2, y=.25, legend = "Linear Regression Model")
## Novamente o coeficiente é parecido, mas nesse caso o std error é bem maior.

## 13j)
# original
confint(lm.fit.e)

# less noisy
confint(lm.fit.h)

# more noisy
confint(lm.fit.i)
# É possivel visualizar que a data "more noisy" tem um intervalo confiança mais amplo.

## 15
library(MASS)
data(Boston)

## 15a)
coefs <- data.frame("predictor"=character(0), "Estimate"=numeric(0), "Std.Error"=numeric(0), "t.value"=numeric(0), "Pr.t"=numeric(0), "r.squared"=numeric(0), stringsAsFactors = FALSE)
j <- 1
for(i in names(Boston)){
  if(i != "crim"){
    summ.lm.fit <- summary(lm(crim ~ eval(parse(text=i)), data=Boston))
    coefs[j,] = c(i, summ.lm.fit$coefficients[2,], summ.lm.fit$r.squared)
    j <- j+1
  }
}

coefs[,-1] <- lapply(coefs[,-1], FUN=function(x) as.numeric(x))
coefs <- coefs[order(coefs$r.squared, decreasing = T),]
print(coefs)

## Pelo p valor todos os resultados são estatisticamente relevantes.

## 15b)
lm.fit.b <- lm(crim ~ ., data=Boston)
summary(lm.fit.b)
## Podemos rejeitar a hipótese nula para zn, nox, dis, rad, black, lstat and medv.

## 15c)
df = data.frame("mult"=summary(lm.fit.b)$coefficients[-1,1])
df$simple <- NA
for(i in row.names(df)){
  df[row.names(df)==i, "simple"] = coefs[coefs[,1]==i, "Estimate"]
}
plot(df$simple, df$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Multiple Linear Regression")
text(x=df$simple, y=df$mult, labels=row.names(df), cex=.7, col="blue", pos=4)

## Melhorando o gráfico
df.clean = df[!(row.names(df)%in%"nox"),]
plot(df.clean$simple, df.clean$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Multiple Linear Regression")
text(x=df.clean$simple, y=df.clean$mult, labels=row.names(df.clean), cex=.7, col="blue", pos=4)

## 15d) 
coefs.poly <- data.frame("predictor"=character(0), "Estimate"=numeric(0), "Std.Error"=numeric(0), "t.value"=numeric(0), "Pr.t"=numeric(0), "r.squared"=numeric(0), stringsAsFactors = FALSE)
j <- 1
for(i in names(Boston)){
  if(!(i %in% c("crim", "chas"))){
    summ.lm.fit <- summary(lm(crim ~ poly(eval(parse(text=i)),3), data=Boston))
    coefs.poly[j,] = c(i, summ.lm.fit$coefficients[2,], summ.lm.fit$r.squared)
    j <- j+1
  }
}

coefs.poly[,-1] <- lapply(coefs.poly[,-1], FUN=function(x) as.numeric(x))
coefs.poly <- coefs.poly[order(coefs.poly$r.squared, decreasing = T),]
print(coefs.poly)

## Plot
df = data.frame("simple"=coefs[,2])
row.names(df) <- coefs[, 1]
df$poly <- NA
for(i in coefs.poly[,1]){
  df[row.names(df)==i, "poly"] <- coefs.poly[coefs.poly[,1]==i, "Estimate"]
}

plot(df$simple, df$poly, xlab="Coef for Simple Linear Regression", ylab="Coef for Poly Linear Regression")
text(x=df$simple, y=df$poly, labels=row.names(df), cex=.7, col="blue", pos=4)
#melhorando o gráfico
df.clean = df[!(row.names(df)%in%"nox"),]
plot(df.clean$simple, df.clean$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Poly Linear Regression")
text(x=df.clean$simple, y=df.clean$mult, labels=row.names(df.clean), cex=.7, col="blue", pos=4)

