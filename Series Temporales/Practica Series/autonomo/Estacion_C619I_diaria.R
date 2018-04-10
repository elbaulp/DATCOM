rm(list = ls(all = T))
require(tseries)
require(fpp2)
require(session)
require(Hmisc)
require(dplyr)
require(imputeTS)

### Preprocesamiento (por ejemplo: tratamiento de valores perdidos,
### agregación de datos para la serie de escala mensual, selección de
### datos para la serie de escala diaria, transformaciones de datos, otro
### preprocesamiento que el alumno estime relevante para proporcionar la
### mejor predicción, etc.).
(serie <- read.csv('./datos/EstacionC619I_diaria.txt',
                   sep = ';'))

### Seleccionar solo desde enero
serie <- serie %>% dplyr::slice(2021:nrow(serie))

### Para imputar valores perdidos se usará imputeTs, pero Tmax no tiene valores perdidos
## imputeTS::na.kalman(serieTs)
(serie <- imputeTS::na.ma(ts(serie)))
(serieTs <- ts(serie,
               frequency = 7))

###  Análisis inicial de cada una de las dos series: Qué se observa visualmente
### (tendencia o no, estacionalidad o no), justificando el análisis con datos
### objetivos (procedentes del análisis visual preliminar de la serie y sus
### componentes).

## Visualización de los datos
## No se observa tendencia
ggplot2::autoplot(serieTs[,"Tmax"])

## Descomposición de componentes
## Parece que la varianza no varia mucho, luego no se realizará ninguna transformación
## Patrón estacional fuerte
## No hay tendencia
ggplot2::autoplot(decompose(serieTs[,"Tmax"]))

## Dividir en train y test
semanasTrain <- c(8, 7)
nPred <- 7

(train <- window(serieTs[,"Tmax"],
                 end = semanasTrain))

(test <- window(serieTs[,"Tmax"],
                start = semanasTrain))

ggplot2::autoplot(train) +
    autolayer(test)

###  En el caso de existir tendencia, justificar qué modelo de tendencia se
### utiliza para eliminarla (filtros, aproximación funcional, diferenciación).
### En el caso de no existir tendencia, justificar igualmente por qué motivo
### se ha llegado a esta conclusión.

## No hay tendencia

###  En el caso de existir estacionalidad, justificar qué modelo se utiliza para
### eliminarla. En el caso de no existir estacionalidad, justificar igualmente
### por qué motivo se ha llegado a esta conclusión.
## Se supuso estacionalidad 7

k <- 7
seasson <- decompose(serieTs[,"Tmax"])$seasonal[1:k]
aux <- rep(seasson, length(train) / length(seasson))
train.no.seasson <- train - aux
test.no.seasson <- test - seasson

ggplot2::autoplot(train - decompose(train)$seasonal, series = "no-ss") +
    autolayer(test.no.seasson, series = "no-ss") +
    autolayer(train, series = "ss") +
    autolayer(test, series = "ss")

## Como el test falla, no es estacionaria, hay que convertirla
adf.test(train.no.seasson)

## PAra ver si hay estacionalidad miramos el acf
## Como no cae rápidamente el ACF, y por la forma de sus patrones,
## la serie no es estacionaria, para ello se intenta eliminar la
## estanionalidad mediente diferenciación por estacionalidad
forecast::ggAcf(train)
train.no.seasson.diff <- diff(train.no.seasson)
test.no.seasson.diff <- diff(test.no.seasson)

## Ahora ya es estacionaria
adf.test(train.no.seasson.diff)
Box.test(diff(train.no.seasson),
         type="Ljung-Box")

## No parece que sea necesario ajustar la varianza,
## Ahora es claramente estacionaria,
## para PACF muestra un posible AR(1) ARIMA(1, 1, 0)
ggtsdisplay(train.no.seasson.diff, main="")
## Modelamos un Arima(1,1,0)
### 2,1,0 ->. AICc 208
### 0,1,0 -> 216
### 1,1,0 -> 207
### 1,1,1 -> 207
fit <- Arima(train.no.seasson, order = c(1,1,0))
summary(fit)

## Los resultados del acf de los residuos
## muestran que los residuos se comportan como ruido blanco
## y el test Ljunj indica que es ruido blanco, porque es >> .05
## Ademas los residuos son normales
forecast::checkresiduals(fit)
ggplot2::autoplot(forecast(fit))

(train.no.seasson.adj <- train.no.seasson + fit$residuals)
(predictions <- forecast(fit, h = nPred))
(predicted.values <- predictions$mean)

(errorTr <- sum((fit$residuals)^2))
(errorTs <- sum((predicted.values - test.no.seasson)^2))

ggplot2::autoplot(train.no.seasson) +
    autolayer(train.no.seasson.adj, series = "Adjusted") +
    autolayer(test.no.seasson, series = "Test") +
    autolayer(predicted.values, series = "Predicted")

autoplot(train.no.seasson, series="Data") +
    autolayer(fitted(fit), series="Fitted") +
    autolayer(forecast(fit, h = nPred), series = "7-ahead") +
    autolayer(predict(fit, n.ahead = nPred)$pred, series = "predict") +
    xlab("Week") + ylab("") +
    ggtitle("Tmax") +
    guides(colour=guide_legend(title=" "))

### Explicación del procedimiento seguido para comprobar y conseguir la
### estacionariedad, en base a los ADF, ACF, PACF.

## Analisis de arriba

### Justificar la selección del modelo de predicción.

## Los arima

### Explicar cómo se ha validado el modelo ajustado.

## Los test de box y jarque

###  Describir, en el caso de existir varios modelos de predicción, qué criterio
### se ha escogido para seleccionar el mejor de ellos (AIC, MSE, etc.),
### justificando la elección del criterio.

###  Describir los pasos necesarios para conseguir la predicción real de los
### valores de la serie.

serie <- serieTs[,"Tmax"]
t <- time(serie)
aux <- ts(serie, frequency = 7)
aux <- decompose(aux)$seasonal
season <- as.numeric(aux[1:7])
aux <- rep(season, length(serie)/length(season))
serie.no.season <- serie - aux
fit <- Arima(serie.no.season, order=c(1,1,0))
adjusted.values <- serie.no.season + fit$residuals
pred <- forecast(fit, h = nPred)
pred.values <- pred$mean

adjusted.values <- adjusted.values + aux
pred.values <- pred.values + season

t.pred <- t + time(1:nPred)

ggplot2::autoplot(serie) +
    autolayer(adjusted.values, series = "adjusted") +
    autolayer(pred, series = "forecast") +
    autolayer(pred.values, series = "pred")

pred.values
