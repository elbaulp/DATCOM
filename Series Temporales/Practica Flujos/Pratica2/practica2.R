rm(list = ls())
require(dplyr)

readClassificationsCorrect <- function(name, dir="output", y = c(5, 6)) {
    filenames <- list.files(path = dir,
                            pattern=paste(name,".*.csv", sep=""),
                            full.names=TRUE)

    ldf <- lapply(filenames, read.csv)
    ldf <- lapply(ldf, as.data.frame)

    res <- lapply(ldf, function(x) x[nrow(x), y])
    res <- purrr::transpose(res)
    res <- unlist(res)
    res <- data.frame(
        kappa = res[31:60],
        acc = res[1:30]
    )
    res
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.my.dataframe <- function(dat) {
    results <- mapply(function(x,i) as.numeric.factor(dat[[i]][,1])[c(2,3)],
                  dat, 1:length(dat), SIMPLIFY = F)
    res <- purrr::transpose(results)
    res <- unlist(res)
    res <- data.frame(
        kappa = res[31:60],
        acc = res[1:30]
    )
    res
}

readKappaAndClassPercents <- function(name, dir="output") {
    filenames <- list.files(path = dir,
                            pattern=paste(name,".*.csv", sep=""),
                            full.names=TRUE)

    ldf <- lapply(filenames, read.csv, header = F, sep = "=")
    ldf <- lapply(ldf, as.data.frame)

    lapply(ldf, function(x) as.data.frame(x$V2))
}

results1.1 <- readKappaAndClassPercents("ejer1.1")
results1.1 <- as.my.dataframe(results1.1)
write.table(results, file = "res/ejer1.1.table")

results1.2 <- readKappaAndClassPercents("ejer1.2")
results1.2 <- as.my.dataframe(results1.2)
write.table(results1.2, file = "res/ejer1.2.table")

shapiro.test(results1.1$kappa)
shapiro.test(results1.1$acc)
shapiro.test(results1.2$acc)
shapiro.test(results1.2$kappa)

t.test(results1.1$acc, results1.2$acc)
t.test(results1.1$kappa, results1.2$kappa)

mean(results1.1$acc)
mean(results1.2$acc)

### Ejercicio 2
ejer2.1 <- readClassificationsCorrect(name = "ejer2.1")
ejer2.2 <- readClassificationsCorrect(name = "ejer2.2")
write.table(ejer2.1, file = "res/ejer2.1.table", row.names = F)
write.table(ejer2.2, file = "res/ejer2.2.table", row.names = F)

shapiro.test(ejer2.1$acc)
shapiro.test(ejer2.2$acc)
shapiro.test(ejer2.1$kappa)
shapiro.test(ejer2.2$kappa)

wilcox.test(ejer2.1$acc, ejer2.2$acc)
wilcox.test(ejer2.1$kappa, ejer2.2$kappa)

mean(ejer2.1$acc)
mean(ejer2.1$kappa)
mean(ejer2.2$acc)
mean(ejer2.2$kappa)

### Ejercicio 3
ejer3.1 <- readClassificationsCorrect(name = "ejer3.1")
ejer3.2 <- readClassificationsCorrect(name = "ejer3.2")

shapiro.test(ejer3.1$acc)
shapiro.test(ejer3.2$acc)

t.test(ejer3.1$acc, ejer3.2$acc)
mean(ejer3.1$acc)
mean(ejer3.2$acc)

shapiro.test(ejer3.1$kappa)
shapiro.test(ejer3.2$kappa)

wilcox.test(ejer3.1$kappa, ejer3.2$kappa)
mean(ejer3.1$kappa)
mean(ejer3.2$kappa)

### Ejercicio 4
ejer4.1 <- readClassificationsCorrect(name = "ejer4.1")
ejer4.2 <- readClassificationsCorrect(name = "ejer4.2")
write.table(ejer4.1, file = "res/ejer4.1.table", row.names = F)
write.table(ejer4.2, file = "res/ejer4.2.table", row.names = F)
