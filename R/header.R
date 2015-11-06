header <- function(x, n = 10) {
    if (is.matrix(x) | is.data.frame(x)) {
        n1 <- min(nrow(x), n)
        n2 <- min(ncol(x), n)
        return(x[seq_len(n1), seq_len(n2)])
    } else {
        return(head(x, n))
    }
}

h <- header