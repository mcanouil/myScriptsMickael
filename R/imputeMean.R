imputeMean <- function (df) {
    newDf <- as.data.frame(apply(df, 2, function (x) {ifelse(is.na(x), mean(x, na.rm = TRUE), x)}))
    cat("Nombre de valeurs manquantes imputees : ", sum(apply(df, 2, is.na))-sum(apply(newDf, 2, is.na)), "/", sum(apply(df, 2, is.na)), " (", ((sum(apply(df, 2, is.na))-sum(apply(newDf, 2, is.na)))/sum(apply(df, 2, is.na)))*100, "%)", "\n", sep = "")
    return(newDf)
}
