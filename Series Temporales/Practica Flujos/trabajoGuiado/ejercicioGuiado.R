require(tseries)

readClassificationsCorrect <- function(name, dir="output", y = 5) {
    filenames <- list.files(path = dir,
                            pattern=paste(name,".*.csv", sep=""),
                            full.names=TRUE)

    ldf <- lapply(filenames, read.csv)
    ldf <- lapply(ldf, as.data.frame)

    sapply(ldf, function(x) x[nrow(x), y])
}

nb <- readClassificationsCorrect(name = "nb")
hoeff <- readClassificationsCorrect(name = "hoeff")

### Siguen una normal según los resultados
snb <- shapiro.test(nb)
shoeff <- shapiro.test(hoeff)
jbng <- tseries::jarque.bera.test(nb)
jbhoeff <- tseries::jarque.bera.test(hoeff)

## El test indica que hay diferencias significativas entre
## los porcentajes de clasificación
ttest <- t.test(nb, hoeff)

meannb <- mean(nb)
meanhoeff <- mean(hoeff)

cat("p-value shafiro for NB:", snb$p.value, "\n",
    "p-value shafiro for HOEFF:", shoeff$p.value, "\n",
    "p-value jarque for NB:", jbng$p.value, "\n",
    "p-value jarque for HOeff:", jbhoeff$p.value, "\n",
    "p-value t-test for both:", ttest$p.value, "\n",
    "mean for NB:", meannb,"\n",
    "mean for Hoeff", meanhoeff, "\n")
