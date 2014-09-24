multipleLM.mix <- function (df, variable) {
    type <- NULL
    newDf <- df
    for (iVar in colnames(df)) {
        if (length(setdiff(variable, iVar)) != 0) {
            setdiff(variable, iVar)
            if (is.factor(df[, iVar])) {
                type <- c(type, "f")
                if (nlevels(df[, iVar])>2) {
                    newName <- paste(iVar, "_AD", sep = "")
                    newDf[, newName] <- newDf[, iVar]
                } else {
                    formul <- as.formula(paste(iVar, "~", paste(variable, collapse = "+"), sep = ""))
                    varExp <- attr(terms(formul), "term.labels")
                    newFormul <- as.formula(paste(iVar, "~", paste(varExp[all.vars(formul)[1] != varExp], collapse = "+"), sep = ""))
                    residualLM <- glm(formula = newFormul, family = binomial, data = df)$fitted.values
                    newName <- paste(iVar, "_AD", sep = "")
                    newDf[names(residualLM), newName] <- residualLM
                }
            } else {
                type <- c(type, "n")
                formul <- as.formula(paste(iVar, "~", paste(variable, collapse = "+"), sep = ""))
                varExp <- attr(terms(formul), "term.labels")
                newFormul <- as.formula(paste(iVar, "~", paste(varExp[all.vars(formul)[1] != varExp], collapse = "+"), sep = ""))
                residualLM <- residuals(lm(formula = newFormul, data = newDf))
                newName <- paste(iVar, "_AD", sep = "")
                newDf[names(residualLM), newName] <- residualLM
            }
        } else {
            newName <- paste(iVar, "_AD", sep = "")
            newDf[, newName] <- newDf[, iVar]
        }
    }
    return(newDf)
}
