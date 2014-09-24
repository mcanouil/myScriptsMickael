s.arrowClass <- function(dfxy, xax = 1, yax = 2, fac, clabel = 1, cpoint = 1, nbVar = "all",
                            cstar = 1, cellipse = 1.5, axesell = TRUE,
                            col = rainbow(length(levels(fac))), pch = 20, boxes = TRUE, xlim = NULL, ylim = NULL, grid = TRUE, cgrid = 1,
                            sub = "", csub = 1.25, possub = "bottomleft", add.plot = FALSE, ...) {

    dfxyco <- trunc(max(dfxy$li)/max(dfxy$co))*dfxy$co*0.8
    s.class(dfxy$li, xax = xax, yax = yax, grid = grid, fac = fac, col = col, sub = sub, ylim = ylim, xlim = xlim, cstar = cstar, cellipse = 0, clabel = 0,
            possub = possub, csub = csub, cgrid = cgrid, cpoint = cpoint, ...)
    if (nbVar == "all") {
        s.arrow(dfxyco, xax = xax, yax = yax, grid = FALSE, add.plot = TRUE, clabel = clabel*0.5, pch = pch, boxes = boxes)
    } else {
        iner <- apply(inertia.dudi(dfxy, col.inertia = TRUE)$col.rel[, -5], 1, sum)
        distri <- summary(abs(iner))
        names(distri) <- c("min", "0.25", "median", "mean", "0.75", "max")
        selec <- names(iner[abs(iner) >= distri[nbVar]])
        s.arrow(dfxyco[selec, ], xax = xax, yax = yax, grid = FALSE, add.plot = TRUE, clabel = clabel*0.6, pch = pch, boxes = boxes)
    }
    s.class(dfxy$li, xax = xax, yax = yax, grid = FALSE, fac = fac, col = col, cpoint = 0, cstar = 0, add.plot = TRUE, clabel = clabel,
            cellipse = cellipse, axesell = axesell)
}
