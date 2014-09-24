makeTable <- function(tab,
                        file = paste0("tab-", substitute(tab), ".tex"),
                        type = "tabularx",
                        fontSize = "scriptsize",
                        width = "16cm",
                        align = paste0(rep("c", ncol(tab)), collapse = ""),
                        caption = NULL,
                        captionShort = NULL,
                        label = substitute(tab),
                        row.names = TRUE,
                        col.names = TRUE,
                        style = "plain") {

    if (style!="plain") {
        hFontsize <- paste0("\\begin{", fontSize, "}\n")
        eFontsize <- paste0("\\end{", fontSize, "}\n")
        hCenter <- "\\begin{center}\n"
        eCenter <- "\\end{center}\n"
        if (is.null(captionShort)) {
            if (is.null(caption)) {
                texCaption <- "% \\caption[]{}\n"
            } else {
                texCaption <- paste0("\\caption{", caption, "}\n")
            }
        } else {
            if (is.null(caption)) {
                texCaption <- "% \\caption[]{}\n"
            } else {
                texCaption <- paste0("\\caption[", captionShort, "]{", caption, "}\n")
            }
        }
        if (is.null(label)) {
            texLabel <- paste0("% \\label{tab-", label, "}\n")
        } else {
            texLabel <- paste0("\\label{tab-", label, "}\n")
        }
    } else {
        hFontsize <- ""
        eFontsize <- ""
        hCenter <- ""
        eCenter <- ""
        texCaption <- ""
        captionShort <- ""
        texLabel <- ""
    }

    if (row.names) {
        if (nchar(align)==ncol(tab)) {
            align <- paste0("r|", align)
        } else {}
        if (col.names) {
            header <- switch(EXPR = type,
                "tabularx" = {
                    paste0("\\hline\n", paste0("\\textbf{", c("", colnames(tab)), "}", collapse = " & "), "\\\\\n", "\\hline\n")
                },
                "longtable" = {texLongTable <- "\\hline\n"
                    texLongTable <- paste0(texLongTable, paste0("\\textbf{", c("", colnames(tab)), "}", collapse = " & "), "\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endfirsthead\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, paste0("\\textbf{", c("", colnames(tab)), "}", collapse = " & "), "\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endhead\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\multicolumn{", ncol(tab)+1, "}{r}{Continued on next page}\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\endfoot\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endlastfoot\n")
                },
                paste0("\\hline\n", paste0("\\textbf{", c("", colnames(tab)), "}", collapse = " & "), "\\\\\n", "\\hline\n")
            )
        } else {
            header <- ""
        }
        tab.dimnames <- dimnames(tab)
        tab <- sapply(seq(ncol(tab)), function (iCol) {
            col <- tab[, iCol]
            if (all(is.numeric(col))) {
                if (min(abs(col), na.rm = TRUE)<1e-2) {
                    format(col, digits = 3, scientific = TRUE, trim = TRUE)
                } else {
                    format(col, digits = 3, scientific = FALSE, trim = TRUE)
                }
            } else {
                col
            }

        })
        dimnames(tab) <- tab.dimnames
        body <- paste0(apply(cbind(paste0("\\textbf{", rownames(tab), "}"), tab), 1, function(x){paste0(paste0(x, collapse = " & "), "\\\\\n")})[seq(nrow(tab))], collapse = "")
    } else {
        if (col.names) {
            header <- switch(EXPR = type,
                "tabularx" = {
                    paste0("\\hline\n", paste0("\\textbf{", colnames(tab), "}", collapse = " & "), "\\\\\n", "\\hline\n")
                },
                "longtable" = {texLongTable <- "\\hline\n"
                    texLongTable <- paste0(texLongTable, paste0("\\textbf{", colnames(tab), "}", collapse = " & "), "\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endfirsthead\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, paste0("\\textbf{", colnames(tab), "}", collapse = " & "), "\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endhead\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\multicolumn{", ncol(tab), "}{r}{Continued on next page}\\\\\n")
                    texLongTable <- paste0(texLongTable, "\\endfoot\n")
                    texLongTable <- paste0(texLongTable, "\\hline\n")
                    texLongTable <- paste0(texLongTable, "\\endlastfoot\n")
                },
                paste0("\\hline\n", paste0("\\textbf{", colnames(tab), "}", collapse = " & "), "\\\\\n", "\\hline\n")
            )
        } else {
            header <- ""
        }
        tab.dimnames <- dimnames(tab)
        tab <- sapply(seq(ncol(tab)), function (iCol) {
            col <- tab[, iCol]
            if (all(is.numeric(col))) {
                if (min(abs(col), na.rm = TRUE)<1e-2) {
                    format(col, digits = 3, scientific = TRUE, trim = TRUE)
                } else {
                    format(col, digits = 3, scientific = FALSE, trim = TRUE)
                }
            } else {
                col
            }

        })
        tab <- matrix(tab, ncol = length(tab.dimnames[[2]]))
        dimnames(tab) <- tab.dimnames
        body <- paste0(apply(tab, 1, function(x){paste0(paste0(x, collapse = " & "), "\\\\\n")})[seq(nrow(tab))], collapse = "")
    }

    hType <- switch(EXPR = type,
        "tabularx" = {paste0("\\begin{", type, "}{", width, "}{", align, "}\n")},
        "longtable" = {paste0("\\begin{", type, "}{", align, "}\n")},
        paste0("\\begin{", type, "}{", align, "}\n")
    )
    eType <- paste0("\\end{", type, "}\n")
    headerbody <- gsub("_", "\\_", paste0(header, body), fixed = TRUE)
    TEX <- switch(EXPR = type,
        "tabularx" = {paste0(hCenter, hFontsize, hType, headerbody, "\\hline\n", eType, eFontsize, eCenter, texCaption, texLabel)},
        "longtable" = {paste0(hCenter, hFontsize, hType, texCaption, "\n\\vspace{-15pt}\\\\\n", texLabel, "\\\\\n", headerbody, eType, eFontsize, eCenter, "\\vspace{-30pt}")},
        paste0(hCenter, hFontsize, hType, headerbody, "\\hline\n", eType, eFontsize, eCenter, texCaption, texLabel)
    )

    # if (addTable) {
        # TEX <- paste0("\\begin{table}\n", TEX, "\n\\end{table}\n")
    # } else {}

    file.create(file)
    cat(TEX, file = file)
    cat(" -Fichier disponible a:", file, "\n")
    return(TEX)
}
