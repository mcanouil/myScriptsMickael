extractExpr <- function(pattern, text) {
    return(unlist(regmatches(text, regexec(pattern, text))))
}
# extractExpr("[0-9]{1, }", "Chrom1")
