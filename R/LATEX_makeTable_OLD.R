makeTable <- function(tab, fileName = paste("tab-", substitute(tab), sep = ""), filePath = "",
                      type = "longtable", fontSize = "scriptsize", size = "16cm", struct = paste(rep("c", ncol(tab)), collapse = ""),
                      nbRow = nrow(tab), caption = c(NULL, NULL), label = substitute(tab)) {
    code <- paste("\\begin{center}\n", sep = "")
    code <- paste(code, "    \\begin{", fontSize, "}\n", sep = "")
    if (type == "tabularx") {
         code <- paste(code, "        \\begin{", type, "}{", size, "}{", struct, "}\n", sep = "")
    } else {
         code <- paste(code, "        \\begin{", type, "}{", struct, "}\n", sep = "")
    }

    if (type == "longtable") {
        if (!is.null(caption)) {
            if (length(caption) == 1) {
                list(caption, NULL)
            } else {
                as.list(caption)
            }
            code <- paste(code, "          ", paste("\\caption[", caption[2], "]{\\label{tab-", label, "}", caption[1], "}\\\\\n", sep = ""))
            code <- paste(code, "            \\hline\n", sep = "")
            code <- paste(code, "            ", paste("\\textbf{", colnames(tab), "}", collapse = " & ", sep = ""), "\\\\\n", sep = "")
            code <- paste(code, "            \\hline\n", sep = "")
            code <- paste(code, "            \\endfirsthead\n", sep = "")
        }
        code <- paste(code, "            \\hline\n", sep = "")
        code <- paste(code, "            ", paste("\\textbf{", colnames(tab), "}", collapse = " & ", sep = ""), "\\\\\n", sep = "")
        code <- paste(code, "            \\hline\n", sep = "")
        code <- paste(code, "            \\endhead\n", sep = "")
        code <- paste(code, "            \\hline\n", sep = "")
        code <- paste(code, "            \\multicolumn{", ncol(tab), "}{r}{Continued on next page}\\\\\n", sep = "")
        code <- paste(code, "            \\endfoot\n", sep = "")
        code <- paste(code, "            \\hline\n", sep = "")
        code <- paste(code, "            \\endlastfoot\n", sep = "")
    } else {
        code <- paste(code, "            ", paste("\\textbf{", colnames(tab), "}", collapse = " & ", sep = ""), "\\\\\n", sep = "")
        code <- paste(code, "            \\hline\n", sep = "")
    }
    code <- paste(code, paste(apply(tab, 1, function(x){paste("            ", paste(x, collapse = " & "), "\\\\\n", sep = "")})[1:nbRow], collapse = ""), sep = "")
    code <- paste(code, "        \\end{", type, "}\n", sep = "")
    code <- paste(code, "    \\end{", fontSize, "}\n", sep = "")
    code <- paste(code, "\\end{center}\n", sep = "")
    code <- gsub("_", "\\_", code, fixed = TRUE)

    texfile <- paste(filePath, fileName, ".tex", sep = "")
    file.create(texfile)
    cat(code, file = texfile)
    cat(" -Fichier disponible a:", paste(filePath, fileName, ".tex", sep = ""), "\n")
    return(code)
}
