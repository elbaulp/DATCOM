rm(list = ls(all = T))
require(tseries)
require(fpp2)
require(session)
require(Hmisc)
require(dplyr)
require(imputeTS)
require(xts)

### Preprocesamiento (por ejemplo: tratamiento de valores perdidos,
### agregación de datos para la serie de escala mensual, selección de
### datos para la serie de escala diaria, transformaciones de datos, otro
### preprocesamiento que el alumno estime relevante para proporcionar la
### mejor predicción, etc.).
(serie <- read.csv('./datos/Estacion1111X_mensual.txt',
                   sep = ';'))

### Para imputar valores perdidos se usará imputeTs, pero Tmax no tiene valores perdidos
## imputeTS::na.kalman(serieTs)
serie <- imputeTS::na.kalman(serie)

### Imputar diciembre de 2016 y enero de 2017 con
### los valores de los años anteriores

(serie.xts <- xts(serie[,"Tmax"],
                  order.by = as.Date(serie[,"Fecha"]),
                  frequency = 1))

(serie.month <- apply.monthly(serie.xts,
                              FUN = median))

(serie.month <- ts(serie.month,
                      frequency = 12,
                      start = c(2013, 5),
                      end = c(2018,2)))

(serie.month <- window(serie.month,
                      start = c(2014, 1)))

###  Análisis inicial de cada una de las dos series: Qué se observa visualmente
### (tendencia o no, estacionalidad o no), justificando el análisis con datos
### objetivos (procedentes del análisis visual preliminar de la serie y sus
### componentes).

## Visualización de los datos
## No se observa tendencia, pero fuerte estacionalidad
ggplot2::autoplot(serie.month)


## Descomposición de componentes
## Parece que la varianza no varia mucho, luego no se realizará ninguna transformación
## Patrón estacional fuerte
## No hay tendencia, pero sí que hay ciclos, esto afecta
## al componente restante, que muestra ciertos patrones,
## No creo que sea adecuado modelar un ciclo
ggplot2::autoplot(decompose(serie.month))



## Dividir en train y test, cogemos 4 años para train, 2 meses para test
mesesTrain <- c(2017, 12)
nPred <- 2

(train <- window(serie.month,
                 end = mesesTrain))

(test <- window(serie.month,
                start = c(2018, 1),
                end = c(2018, 2)))

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
## Se ve claramente que hay estacionliadad, se elimina
forecast::ggAcf(train)

## Se supuso estacionalidad 12

## Claramente hay estacionalidad
forecast::ggsubseriesplot(serie.month)

k <- 12
seasson <- decompose(serie.month)$seasonal[1:k]
aux <- rep(seasson, length(train) / length(seasson))
train.no.seasson <- train - aux
## LE quitamos la componente estacional de enero y febrerop al test
test.no.seasson <- test - seasson[1:2]

## Como se muetras, se ha eliminado bien la estacionalidad
ggplot2::autoplot(train - decompose(train)$seasonal, series = "no-estanionalidad") +
    autolayer(test.no.seasson, series = "no-estacionalidad-test") +
    autolayer(train, series = "estacionalidad") +
    autolayer(test, series = "estacionalidad-test")

## El test pasa (p-vale <.05, luego es estacionaria y no hay que hacer nada más
adf.test(train.no.seasson)
forecast::ggAcf(train.no.seasson)

## No parece que sea necesario ajustar la varianza,
## Ahora es claramente estacionaria,
## para PACF muestra un posible AR(1) ARIMA(1, 1, 0)
ggtsdisplay(train.no.seasson, main="")


## No parece que sea necesario ajustar la varianza,
## Ahora es claramente estacionaria,
## para PACF muestra un posible AR(1) ARIMA(1, 1, 0)
## Puede ser intereante intentar los siguientes Arimas:
forecast::ggtsdisplay(train.no.seasson)

## Modelamos un Arima(1,1,0)
(fit <- Arima(train.no.seasson, order = c(0,0,1)))
(a1.0.1 <- Arima(train.no.seasson, order = c(1,0,1)))
(a11.0.1 <- Arima(train.no.seasson, order = c(1,0,11)))
(a15.0.1 <- Arima(train.no.seasson, order = c(1,0,15)))
(a11.0.0 <- Arima(train.no.seasson, order = c(0,0,11)))
(a15.0.0 <- Arima(train.no.seasson, order = c(0,0,15)))
## De todos, el de menor AICc es a.0.0.1, con un valor de 151


## Los resultados del acf de los residuos
## muestran que los residuos se comportan como ruido blanco
## y el test Ljunj indica que es ruido blanco, porque es >> .05
## Ademas los residuos son normales
forecast::checkresiduals(fit)
ggplot2::autoplot(forecast(fit, h = nPred))

(train.no.seasson.adj <- train.no.seasson + fit$residuals)
(predictions <- forecast(fit, h = nPred))
(predicted.values <- predictions$mean)

(errorTr <- sum((fit$residuals)^2))
(errorTs <- sum((predicted.values - test.no.seasson)^2))

ggplot2::autoplot(train.no.seasson, main = "Valores de la serie sin estacionalidad y el modelo") +
    autolayer(train.no.seasson.adj, series = "Adjusted") +
    autolayer(test.no.seasson, series = "Test") +
    autolayer(predicted.values, series = "Predicted")

autoplot(train.no.seasson, series="Data") +
    autolayer(fitted(fit), series="Fitted") +
    autolayer(forecast(fit, h = nPred), series = "2-ahead") +
    autolayer(predict(fit, n.ahead = nPred)$pred, series = "predict") +
    xlab("Month") + ylab("") +
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

(serie.xts <- xts(serie[,"Tmax"],
                  order.by = as.Date(serie[,"Fecha"]),
                  frequency = 1))

(serie.month <- apply.monthly(serie.xts,
                              FUN = median))
serie <- serie.month
(aux <- ts(serie,
           frequency = 12,
           start = c(2013, 5),
           end = c(2018,2)))
aux <- window(aux,
              start = c(2014, 1))
serie <- aux
aux <- decompose(aux)$seasonal
season <- as.numeric(aux[1:12])
aux <- rep(season, length(serie)/length(season))
serie.no.season <- serie - aux
fit <- Arima(serie.no.season, order=c(1,0,0))
adjusted.values <- serie.no.season + fit$residuals
pred <- forecast(fit, h = nPred)
pred.values <- pred$mean
adjusted.values <- adjusted.values + aux
pred.values <- pred.values + season[3:4]

t <- time(serie)

t.pred <- t + time(1:nPred)

ggplot2::autoplot(serie) +
    autolayer(adjusted.values, series = "adjusted") +
    autolayer(pred.values, series = "pred") +
    autolayer(pred, series = "Forecast")

pred.values
