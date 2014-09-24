removeVarNA <- function (df, freqNa.var = 0.5, freqNa.ind = 0.2, freqMaxMod = 0.95, cex = 0.4, draw = FALSE) { # p: freq max de NA par variable
    sommeNA <- function (df, margin) {
        apply(apply(df, margin, is.na), 2, sum)
    }
    nombreNA_Var <- sommeNA(df, 2)
    nombreNA_Ind <- sommeNA(df, 1)
    listVarRemoved <- names(df)[nombreNA_Var > round(freqNa.ind*nrow(df))]
    listIndRemoved <- df[nombreNA_Ind > round(freqNa.var*ncol(df)), "ref"]
    newDf <- df[nombreNA_Ind <= round(freqNa.var*ncol(df)), setdiff(names(df), listVarRemoved)] # Correction du retrait de VAr

    nombreNA_Var2 <- sommeNA(newDf, 2)
    nombreNA_Ind2 <- sommeNA(newDf, 1)

    if (draw == TRUE) {
        par(mfrow = c(2, 2))
        plot(sort(nombreNA_Ind), xlab = "Individus", ylab = "Nb Var NA", pch = 1, cex = cex, xlim = c(0, nrow(df)), ylim = c(0, ncol(df)))
        abline(h = max(nombreNA_Ind2), col = 2)
        abline(v = dim(newDf)[1], col = 2)
        plot(sort(nombreNA_Ind2), xlab = "Individus", ylab = "Nb Var NA", pch = 1, cex = cex, xlim = c(0, nrow(df)), ylim = c(0, ncol(df)))

        plot(sort(nombreNA_Var), xlab = "Var", ylab = "Nb Individus NA", pch = 1, cex = cex, xlim = c(0, ncol(df)), ylim = c(0, nrow(df)))
        abline(h = max(nombreNA_Var2), col = 2)
        abline(v = dim(newDf)[2], col = 2)
        plot(sort(nombreNA_Var2), xlab = "Var", ylab = "Nb Individus NA", pch = 1, cex = cex, xlim = c(0, ncol(df)), ylim = c(0, nrow(df)))
        par(mfrow = c(1, 1))
    }

    varTooMuch <- unlist(lapply(lapply(lapply(apply(newDf, 2, table), function(x){x/sum(x)}), function(x){x <= freqMaxMod}), any))
    varMod95Removed <- names(newDf)[varTooMuch]
    finDf <- newDf[, setdiff(names(newDf), names(newDf)[varTooMuch])]

    cat(" *Nombre d'individus retirees (>", freqNa.var*100, "% de NAs):  ", length(listIndRemoved), "/", nrow(df), " (", round((length(listIndRemoved)/nrow(df))*100, digits = 2), "%)\n", sep = "")
    cat(" *Nombre de variables retirees:             ", length(listVarRemoved)+sum(varTooMuch), "/", ncol(df), " (", round(((length(listVarRemoved)+sum(varTooMuch))/ncol(df))*100, digits = 2), "%)\n", sep = "")
    cat("    -Nombre de NAs > ", freqNa.ind*100, "%:                   ", length(listVarRemoved), "/", ncol(df), " (", round((length(listVarRemoved)/ncol(df))*100, digits = 2), "%)\n", sep = "")
    cat("    -Frequence modalite > ", freqMaxMod*100, "%:              ", sum(varTooMuch), "/", length(names(newDf)), " (", round((sum(varTooMuch)/length(names(newDf)))*100, digits = 2), "%)\n", sep = "")

    return(list(data = finDf, list(listIndRemoved = listIndRemoved, listVarRemoved = listVarRemoved, varMod95Removed = varMod95Removed)))
}
