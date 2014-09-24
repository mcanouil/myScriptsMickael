disj2Table <- function(df, tabdisj) {
    nbdummy <- rep(1, ncol(df))
    is.quali <- which(!unlist(lapply(df, is.numeric)))
    nbdummy[is.quali] <- unlist(lapply(df[, is.quali, drop = FALSE], nlevels))
    vec = c(0, cumsum(nbdummy))
    newDf <- df
    for (i in 1:ncol(df)) {
        if (i %in% is.quali)
          newDf[, i] <- as.factor(levels(df[, i])[apply(tabdisj[, (vec[i] + 1):vec[i + 1]], 1, which.max)])
        else newDf[, i] <- tabdisj[, vec[i] + 1]
    }
    return(newDf)
}

# disj2Table <- function(tabdisj) {
    # columns <- unique(gsub("\\.[0-9]", "", colnames(tabdisj)))
    # rows <- sort(unique(gsub("[^.]*\\.([0-9])", "\\1", colnames(tabdisj))))
    # result <- as.data.frame(matrix(NA, ncol = length(columns), nrow = length(rows)))
    # dimnames(result) <- list(rows, columns)
    # for (iVar in columns) {
        # varDisj <- grep(iVar, colnames(tabdisj), value = TRUE)
        # for (iCol in varDisj) {
            # result[grep(1, tabdisj[, iCol]), iVar] <- gsub("[^.]*\\.([0-9])", "\\1", iCol)
        # }
    # }
    # return(result)
# }