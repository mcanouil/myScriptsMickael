rowSd <- function(x, na.rm = TRUE) {
    if (na.rm) {
        n <- rowSums(!is.na(x))
    } else {
        n <- ncol(x)
    }
    rowVar <- rowMeans(x*x, na.rm = na.rm) - (rowMeans(x, na.rm = na.rm))^2
    return(sqrt(rowVar * n/(n-1)))
}