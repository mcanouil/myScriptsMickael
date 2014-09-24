makeFigure <- function(imagePath, imageTitle, width = 5, height = 5, scale = NULL){
    code <- paste("\\begin{figure}[Ht]", "\n", sep = "")
    code <- paste(code, "    \\begin{center}", "\n", sep = "")
    if (!is.null(scale)) {
        code <- paste(code, "        \\includegraphics[scale = ", scale, "]{", imagePath, "}", "\n", sep = "")
    } else {
        code <- paste(code , "        \\includegraphics[width = ", width, "cm, height = ", height, "cm]{", imagePath, "}\n", sep = "")
    }
    code <- paste(code, "    \\end{center}", "\n", sep = "")
    if (!is.null(imageTitle)) {
        code <- paste(code, "    \\caption{", imageTitle, "}", "\n", sep = "")
    }
    code <- paste(code, "\\end{figure}", "\n", sep = "")

    return(code)
}
