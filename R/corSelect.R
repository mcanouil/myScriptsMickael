corSelect <- function(df, alpha = 0.95) {
    for (i in 1:nrow(df)) {
        df[i, i:ncol(df)] <- 0
    }
    COORD <- which(df>alpha, arr.ind = TRUE)

    Names <- function(COORD, M = df) {
        return(c(rownames(M)[COORD[1]], colnames(M)[COORD[2]]))
    }
    resultat <- apply(COORD, 1, Names)
    colnames(resultat) <- NULL
    return(t(resultat[, order(resultat[1, ])]))
}
