multipleLM <- function (df, var, cov, ...) {
    newDf <- df
    for (iVar in var) {
        formul <- as.formula(paste(iVar, "~", paste(cov, collapse = "+"), sep = ""))
        varExp <- attr(terms(formul), "term.labels")
        if (length(varExp[all.vars(formul)[1] != varExp]) != 0) {
            newFormul <- as.formula(paste(iVar, "~", paste(varExp[all.vars(formul)[1] != varExp], collapse = "+"), sep = ""))
            residualLM <- residuals(lm(newFormul, df))
            newName <- paste(iVar, "_AD", sep = "")
            newDf[names(residualLM), newName] <- residualLM
        } else {
            newName <- paste(iVar, "_AD", sep = "")
            newDf[, newName] <- newDf[, iVar]
        }
    }
    return(newDf)
}
