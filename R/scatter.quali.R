scatter.quali <- function (dudi.obj, xax = 1, yax = 2, csub = 2, possub = "topleft", lim = 0.05, ...) {
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    tabcomplet <- eval(as.list(dudi.obj$call)[[2]], sys.frame(0))
    indexation <- dudi.obj$index == "f"
    oritab <- tabcomplet[, indexation]
    quali.cr  <- which(sort(apply(dudi.obj$cr[dudi.obj$index == "f", ], 1, function(x){sum(x >= lim)}), decreasing = TRUE) >= 1)
    noms <- names(quali.cr)
    oritab <- oritab[, noms]
    if (!is.data.frame(oritab)) {
        oritab <- as.data.frame(oritab)
    }
    nvar <- ncol(oritab)
    par(mfrow = n2mfrow(nvar))
    for (i in 1:nvar) {
        s.class(dudi.obj$li, oritab[, i], xax = xax, yax = yax ,
            clabel = 1.5, sub = paste(names(oritab[i]), " (", paste(round(dudi.obj$cr[names(oritab[i]) , c(xax , yax)], digits = 3), collapse = ", "), ")", sep = ""), csub = csub ,
            possub = possub, col = rainbow(nlevels(oritab[, i])) ,
            cgrid = 0, cstar = 0)
    }
}
