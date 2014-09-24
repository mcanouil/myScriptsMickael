readFast <- function(file, header = TRUE, mc.cores = 1, what = character(), sep = "\t", ...) {
    firstLine <- scan(file = file, skip = 0, nlines = 1, what = character(), quiet = TRUE, sep = sep, ...)
    nbRows <- as.numeric(unlist(strsplit(system(paste("wc -l", file), intern = TRUE), " "))[1])
    nbCols <- length(firstLine)
    tab <- do.call("rbind", mclapply(seq(1, nbRows, floor(nbRows/mc.cores)), mc.cores = mc.cores, function(line){
        return(matrix(scan(file = file, skip = line, nlines = floor(nbRows/mc.cores), what = what, quiet = TRUE, sep = sep, ...), ncol = nbCols, byrow = TRUE))
    }))
    if (header) {
        colnames(tab) <- firstLine
    } else {
        tab <- rbind(firstLine, tab)
    }

    # nmColClasses <- names(colClasses)
    # col.names <- colnames(tab)
    # cols <- ncol(tab)
    # temp <- rep_len(NA_character_, cols)
    # names(temp) <- col.names
    # if (any(nmColClasses == "") | any(is.na(nmColClasses)) | is.null(nmColClasses)) {
        # i <- match(nmColClasses, col.names, 0L)
        # temp[i[i > 0L]] <- colClasses[i[i > 0L]]
        # temp[i == 0L] <- colClasses[i == 0L]
        # colClasses <- temp
    # } else {}

    # what <- rep.int(list("character"), cols)
    # names(what) <- col.names
    # colClasses[colClasses %in% c("real", "double")] <- "numeric"
    # colClasses[colClasses %in% "NULL"] <- "character"
    # known <- colClasses %in% c("logical", "integer", "numeric", "complex", "character", "raw")
    # what[known] <- sapply(colClasses[known], do.call, list(0))
    # tabList <- as.data.frame(lapply(seq(cols), function(iCol){eval(call(paste0("as.", colClasses[iCol]), tab[, iCol]))}))
    # dimnames(tabList) <- dimnames(tab)
    return(tab)
}
