quali2Bin <- function (df, var) {
    df.temp <- df[, var]
    acm.util <- function (i) {
        cl <- df.temp[, i]
        cha <- names(df.temp)[i]
        n <- length(cl)
        cl <- as.factor(cl)
        if (length(levels(cl))>2) {
            x <- matrix(0, n, length(levels(cl)))
            x[(1:n) + n * (unclass(cl) - 1)] <- 1
            dimnames(x) <- list(row.names(df.temp), paste(cha, levels(cl),
                sep = "_Dis."))
            return(x)
        } else {
            return(matrix(rep(NA, nrow(df.temp)), dimnames = list(row.names(df.temp), c("Trash"))))
        }
    }
    newDf <- lapply(1:ncol(df.temp), acm.util)
    newDf <- data.frame(cbind(df, newDf), check.names = FALSE)
    newDf <- newDf[, setdiff(names(newDf), "Trash")]
    return(newDf)
}
