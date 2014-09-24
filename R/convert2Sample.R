convert2Sample <- function(dataFrame, rmDup = FALSE, dupCharacter = "-", fileDir = getwd(), writeFile = FALSE, comb = FALSE) {
    fileName <- substitute(dataFrame)
    res <- NULL
    dataFrame[, "Sample_Name"] <- as.character(dataFrame[, "Sample_Name"])
    splitId <- function(x) {strsplit(x, dupCharacter)}
    identAll <- splitId(dataFrame[, "Sample_Name"])
    duplicat <- duplicated2(sapply(identAll, function(x){x[1]}))
    if (any(duplicat)) {
        duplicatIdent <- sapply(identAll[duplicat], function(x){paste(x, collapse = dupCharacter)})
        nbDuplicat <- length(unique(sapply(splitId(duplicatIdent), function(x){x[1]})))
        if (rmDup==TRUE) {
            ident <- setdiff(dataFrame[, "Sample_Name"], duplicatIdent[grep(dupCharacter, duplicatIdent)])
        } else {
            ident <- dataFrame[, "Sample_Name"]
        }
        res$nbDuplicat <- nbDuplicat
        res$idDuplicat <- sort(duplicatIdent)
    } else {
        ident <- sapply(identAll, function(x){paste(x, collapse = dupCharacter)})
        res$nbDuplicat <- 0
        res$idDuplicat <- "None"
    }
    cat("Results:\n")
    cat(paste(" - Duplicated (", res$nbDuplicat, "):", sep = ""), res$idDuplicat, "\n")

    newDf <- as.data.frame(dataFrame[dataFrame[, "Sample_Name"] %in% ident, c("Sample_Name", "Sample_Group")])
    rownames(newDf) <- 1:nrow(newDf)
    newDf <- newDf[order(newDf[, "Sample_Name"]), ]

    if (writeFile==TRUE) {
        fileNameData <- paste(fileDir, "/", fileName, "-SampleSheet.txt", sep = "")
        write.table(x = newDf, file = fileNameData, sep = "\t", quote = FALSE, row.names = FALSE)
        cat(" - Data:", fileNameData, "\n")
    } else {
        cat(" - Data: .$data \n")
    }
    res$data <- newDf

    if (nlevels(as.factor(newDf[, "Sample_Group"]))>2 & comb==TRUE) {
        for (iVar in combn(levels(as.factor(newDf[, "Sample_Group"])), 2, simplify = FALSE)) {
            if (writeFile==TRUE) {
                fileNameVar <- paste(fileDir, "/", fileName, "-SampleSheet-", paste(iVar, collapse = "-"), ".txt", sep = "")
                write.table(x = newDf[newDf[, "Sample_Group"] %in% iVar, ],
                            file = fileNameVar,
                            sep = "\t", quote = FALSE, row.names = FALSE)
                cat(paste(" - Data (", paste(iVar, collapse = "-"), "):", sep = ""), fileNameVar, "\n")
            } else {
                cat(paste(" - Data (", paste(iVar, collapse = "-"), "):", sep = ""), paste(".$", paste(substr(iVar, 1, 3), collapse = "."), sep = ""), "\n")
            }
            eval(parse(text = paste("res$", paste(substr(iVar, 1, 3), collapse = "."), " <- newDf[newDf[, 'Sample_Group'] %in% iVar, ]", sep = "")))
        }
    }
    if (writeFile==TRUE) {
        invisible()
    } else {
        return(res)
    }
}
