makeReport <- function (file, biblio = TRUE) {
    dirTmp <- gsub("[^/]*\\.rnw$", "", file)
    fileTmp <- gsub(".rnw", "", gsub(dirTmp, "", file))
    setwd(dirTmp)
    Sweave(paste0(fileTmp, ".rnw"), results = "hide")
    system(paste0("pdflatex ", fileTmp, ".tex"), ignore.stdout = TRUE)
    if (biblio) {
        system(paste0("bibtex ", fileTmp))
        system(paste0("pdflatex ", fileTmp, ".tex"), ignore.stdout = TRUE)
    } else {}
    system(paste0("pdflatex ", fileTmp, ".tex"))
    system(paste0("gvfs-open ", fileTmp, ".pdf"))
    paste0(fileTmp, c(".aux", ".bbl", ".blg", ".out", ".toc"))
    system(paste0("rm ", paste0(fileTmp, c(".aux", ".bbl", ".blg", ".out", ".toc"), collapse = " ")))
    return(invisible())
}
