var2MuchMod <- function (df  , freqMaxMod = 0.95) {
    varTooMuch <- unlist(lapply(lapply(lapply(apply(df  , 2  , table)  , function(x){x/sum(x)})  , function(x){x >= freqMaxMod})  , any))
    newDf <- df[  , setdiff(names(df)  , names(df)[varTooMuch])]
    cat("Nombre de variables retirees (modalite > "  , freqMaxMod*100  , "%):  "  , sum(varTooMuch)  , "/"  , length(names(df))  , " ("  , round((sum(varTooMuch)/length(names(df)))*100 , digits = 2)  , "%)\n"  , sep = "")
    return(list(data = newDf  , varMod95Removed = names(df)[varTooMuch]))
}
