multiplot <- function (..., plotlist = NULL, rows = 1, cols = 1, layout = NULL) {
    plots <- c(list(...), plotlist)
    numPlots <- length(plots)
    if (numPlots>1) {
        dimPlot <- n2mfrow(numPlots)
        rows <- dimPlot[1]
        cols <- dimPlot[2]
    } else {}
    if (is.null(layout)) {
        # layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = rows, byrow = TRUE)
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), nrow = rows, ncol = cols, byrow = TRUE)
    } else {}
    if (numPlots==1) {
        print(plots[[1]])
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
    }
}