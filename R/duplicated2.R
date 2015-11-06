duplicated2 <- function(x){
    if (sum(dup <- duplicated(x)) == 0) {
        return(dup)
    }
    if (class(x) %in% c("data.frame", "matrix")) {
        return(duplicated(rbind(x[dup, ], x))[-seq_len(sum(dup))])
    } else {
        return(duplicated(c(x[dup], x))[-seq_len(sum(dup))])
    }
}
