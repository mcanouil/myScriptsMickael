# GC <- function(verbose = getOption("verbose"), reset = FALSE) {
    # while (any(.Internal(gc(verbose, reset))[7:8] != .Internal(gc(verbose, reset))[7:8])) {}
    # res <- .Internal(gc(verbose, reset))
    # res <- matrix(res, 2L, 7L, dimnames = list(c("Ncells", "Vcells"), c("used", "(Mb)", "gc trigger", "(Mb)", "limit (Mb)", "max used", "(Mb)")))
    # if (all(is.na(res[, 5L]))) {
        # return(res[, -5L])
    # } else {
        # return(res)
    # }
# }

GC <- function(verbose = getOption("verbose"), reset = FALSE) {
    while (!identical(gc(verbose, reset)[, 4], gc(verbose, reset)[, 4])) {}
    return(gc(verbose, reset))
}