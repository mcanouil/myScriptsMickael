header <- function(x, n = 10) {
    if (is.matrix(x) | is.data.frame(x)) {
        n1 <- min(nrow(x), n)
        n2 <- min(ncol(x), n)
        return(x[seq(n1), seq(n2)])
    } else {
        return(head(x, n))
    }
}

h <- header