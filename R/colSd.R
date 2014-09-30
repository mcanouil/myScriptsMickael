colSd <- function(x, na.rm = TRUE) {
    if (na.rm) {
        n <- colSums(!is.na(x))
    } else {
        n <- nrow(x)
    }
    colVar <- colMeans(x*x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
    return(sqrt(colVar * n/(n-1)))
}