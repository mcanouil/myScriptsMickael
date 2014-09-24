classS4Creator <- function(name, field, type, path = getwd()){
    lengthField <- length(field)

    if (path!="") {
        file <- paste(path, "/", name, "Class.R", sep = "")
    } else {
        file <- ""
    }


    nb <- paste(rep("#", floor((70 - nchar(paste0(" Class ", name, " ")))/2)), collapse = "")
    a00 <- paste(nb, "Class", name, nb)
    if (nchar(a00)<70) {
        a01 <- "######################################################################\n"
        a02 <- paste(a00, "\n", sep = "#")
        a03 <- "############################## Creation ##############################\n"
        a04 <- "######################################################################\n\n\n"
        a0 <- paste(a01, a02, a03, a04, sep = "")
    } else {
        a01 <- "######################################################################\n"
        a02 <- paste(a00, "\n", sep = "")
        a03 <- "############################## Creation ##############################\n"
        a04 <- "######################################################################\n\n\n"
        a0 <- paste(a01, a02, a03, a04, sep = "")
    }
    cat(a0, file = file, sep = "")


    b0 <- '### Class definition ###\n'
    b1 <- paste('setClass(\n    Class = "', name, '", \n    representation = representation(\n', sep = '')
    b2 <- NULL
    for (i in 1:(lengthField-1)) {
        b2 <- paste(b2, '        ', field[i], ' = "', type[i], '", \n', sep = '')
    }
    b2 <- paste(b2, '        ', field[lengthField], ' = "', type[lengthField], '"\n', sep = '')
    b3 <- '    ), \n    prototype = prototype(\n'
    b4 <- NULL
    for (i in 1:(lengthField-1)) {
        b4 <- paste(b4, '        ', field[i], ' = ', ifelse(type[i]=='matrix', 'matrix(, nrow = 0, ncol = 0)', paste(type[i], '()', sep = '')), ', \n', sep = '')
    }
    b4 <- paste(b4, '        ', field[lengthField], ' = ', ifelse(type[lengthField]=='matrix', 'matrix(, nrow = 0, ncol = 0)', paste(type[lengthField], '()', sep = '')), '\n', sep = '')
    b5 <- paste('    ), \n    validity = function(object){\n        cat("**** validity ', name, ' <empty> ****")\n        return(TRUE)\n    }\n)\n\n\n', sep = '')
    cat(b0, b1, b2, b3, b4, b5, sep = "", file = file, append = TRUE)


    c0 <- '### Constructor ###\n'
    c1 <- paste('setGeneric(name = "new', name, '", def = function(', paste(paste(field[-lengthField], collapse = ', '), field[lengthField], sep = ', '), '){standardGeneric("new', name, '")})\n', sep = '')
    c2 <- paste('setMethod(f = "new', name, '", signature = c(', paste(rep('"missing"', lengthField), collapse = ", "), '), definition = function(', paste(paste(field[-lengthField], collapse = ', '), field[lengthField], sep = ', '), '){new("', name, '")})\n', sep = '')

    c3 <- paste('setMethod(f = "new', name, '", signature = c(', paste(rep('"ANY"', lengthField), collapse = ", "), '), definition = function(', paste(paste(field[-lengthField], collapse = ', ', sep = ''), paste(field[lengthField], '){\n', sep = ''), sep = ', '), sep = "")
    c4 <- paste(paste('    if (missing(', field, ')) {', field, ' <- ', type, '()} else {}\n', sep = ''), collapse = '')
    c5 <- paste('    return(new("', name, '"', paste(', ', field, " = ", field, collapse = '', sep = ''), '))\n', sep = '')
    c6 <- paste('})\n\n\n')
    cat(c0, c1, c2, c3, c4, c5, c6, sep = "", file = file, append = TRUE)


    d0 <- '### Is ###\n'
    d1 <- paste('setGeneric(name = "is.', name, '", def = function(object){standardGeneric("is.', name, '")})\n', sep = '')
    d2 <- paste('setMethod(f = "is.', name, '", signature = "ANY", definition = function(object){\n',
            '    if (length(object)>1) {\n',
            '        return(sapply(object, is.', name, '))\n',
            '    } else {\n',
            '        if (class(object) == "', name, '") {\n',
            '            return(TRUE)\n',
            '        } else {\n',
            '            return(FALSE)\n',
            '        }\n',
            '    }\n',
            '})\n\n\n', sep = '')
    cat(d0, d1, d2, sep = "", file = file, append = TRUE)


    e0 <- '### Show ###\n'
    e1 <- paste('setMethod(f = "show", signature = "', name, '", definition = function(object){\n    cat("    ~~~ Class:", class(object), "~~~ ")\n', sep = '')
    e2 <- NULL
    for (i in 1:lengthField) {
        if (type[i]=="numeric" | type[i]=="character") {
            e2 <- paste(e2, '    cat("\\n ~ ', field[i], ' : ", ifelse(length(object@', field[i], ')==0, NA, ifelse(length(object@', field[i], ')>1, paste("[", length(object@', field[i], '), "] ", paste(head(object@', field[i], '), collapse = " "), sep = ""), object@', field[i], ')), sep = "")\n', sep = '')
        } else {
            if (type[i]=="list") {
                e2 <- paste(e2, '    cat("\\n ~ ', field[i], ' : ", ifelse(length(object@', field[i], ')==0, NA, paste("List of ", length(object@', field[i], '), sep = "")), sep = "")\n', sep = '')
            } else {
                if (type[i]=="matrix" | type[i]=="data.frame") {
                    e2 <- paste(e2, '    cat("\\n ~ ', field[i], ' : [", paste(dim(object@', field[i], '), collapse = "x"), "]", sep = "")\n', sep = '')
                    e2 <- paste(e2, '        nrowShow <- min(5, nrow(object@',  field[i], '))\n', sep = '')
                    e2 <- paste(e2, '        ncolShow <- min(5, ncol(object@',  field[i], '))\n', sep = '')
                    e2 <- paste(e2, '        if (nrow(object@',  field[i], ')!=0) {\n', sep = '')
                    e2 <- paste(e2, '            cat("\\n")\n', sep = '')
                    e2 <- paste(e2, '            print(object@',  field[i], '[1:nrowShow, 1:ncolShow], quote = FALSE)\n', sep = '')
                    e2 <- paste(e2, '            if(all(dim(object@', field[i], ')>c(nrowShow, ncolShow))){cat("   ..... .....\\n")}\n', sep = '')
                    e2 <- paste(e2, '        } else {\n', sep = '')
                    e2 <- paste(e2, '            cat(" NA")\n', sep = '')
                    e2 <- paste(e2, '        }\n', sep = '')
                } else {
                    e2 <- paste(e2, '    cat("\\n ~ ', field[i], ' : ", object@', field[i], ', sep = "")\n', sep = '')
                }
            }
        }
    }
    e3 <- paste('    cat("\\n")\n    invisible()\n})\n\n\n', sep = '')
    cat(e0, e1, e2, e3, sep = "", file = file, append = TRUE)


    f0 <- '### Getteur ###\n'
    f1 <- paste('setMethod(f = "[", signature = "', name, '", definition = function(x, i, j, drop){\n    switch(EXPR = i, \n', sep = '')
    f2 <- NULL
    for (i in 1:lengthField) {
        if (type[i]=="list") {
            f2 <- paste(f2, '        "', field[i], '" = {\n', sep = '')
            f2 <- paste(f2, '            if (missing(j)) {\n', sep = '')
            f2 <- paste(f2, '                return(x@', field[i], ')\n', sep = '')
            f2 <- paste(f2, '            } else {\n', sep = '')
            f2 <- paste(f2, '                if (j>length(x@', field[i], ')) {\n', sep = '')
            f2 <- paste(f2, '                    stop("[', name, ':get] indice out of limits")\n', sep = '')
            f2 <- paste(f2, '                } else {\n', sep = '')
            f2 <- paste(f2, '                    return(x@', field[i], '[[j]])\n', sep = '')
            f2 <- paste(f2, '                }\n', sep = '')
            f2 <- paste(f2, '            }\n', sep = '')
            f2 <- paste(f2, '        }, \n', sep = '')
        } else {
            if (type[i]=="matrix" | type[i]=="data.frame") {
                f2 <- paste(f2, '        "', field[i], '" = {return(x@', field[i], ')}, \n', sep = '')
            } else {
                f2 <- paste(f2, '        "', field[i], '" = {\n', sep = '')
                f2 <- paste(f2, '            if (missing(j)) {\n', sep = '')
                f2 <- paste(f2, '                return(x@', field[i], ')\n', sep = '')
                f2 <- paste(f2, '            } else {\n', sep = '')
                f2 <- paste(f2, '                if (j>length(x@', field[i], ')) {\n', sep = '')
                f2 <- paste(f2, '                    stop("[', name, ':get] indice out of limits")\n', sep = '')
                f2 <- paste(f2, '                } else {\n', sep = '')
                f2 <- paste(f2, '                    return(x@', field[i], '[j])\n', sep = '')
                f2 <- paste(f2, '                }\n', sep = '')
                f2 <- paste(f2, '            }\n', sep = '')
                f2 <- paste(f2, '        }, \n', sep = '')
            }
        }
    }
    stopGet <- paste('"[', name, ':get] ", i, " is not a \\"', name, '\\" slot"', sep = "")
    f3 <- paste("        stop(", stopGet, ")\n    )\n    invisible()\n}", sep = '')
    f4 <- paste(')\n\n\n', sep = '')
    cat(f0, f1, f2, f3, f4, sep = "", file = file, append = TRUE)


    g0 <- '### Setteur ###\n'
    g1 <- paste('setMethod(f = "[<-", signature = "', name, '", definition = function(x, i, j, value){\n    switch(EXPR = i, \n', sep = '')
    g2 <- NULL
    for (i in 1:lengthField) {
        if (type[i]=="list") {
            g2 <- paste(g2, '       "', field[i], '" = {\n', sep = '')
            g2 <- paste(g2, '            if (missing(j)) {\n', sep = '')
            g2 <- paste(g2, '               x@', field[i], ' <- value\n', sep = '')
            g2 <- paste(g2, '            } else {\n', sep = '')
            g2 <- paste(g2, '               if (j>length(x@', field[i], ')) {\n', sep = '')
            g2 <- paste(g2, '                   stop("[', name, ':set] indice out of limits")\n', sep = '')
            g2 <- paste(g2, '               } else {\n', sep = '')
            g2 <- paste(g2, '                   x@', field[i], '[[j]] <- value\n', sep = '')
            g2 <- paste(g2, '               }\n', sep = '')
            g2 <- paste(g2, '            }\n', sep = '')
            g2 <- paste(g2, '        }, \n', sep = '')
        } else {
            if (type[i]=="matrix" | type[i]=="data.frame") {
                g2 <- paste(g2, '        "', field[i], '" = {x@', field[i], ' <- value}, \n', sep = '')
            } else {
                g2 <- paste(g2, '         "', field[i], '" = {\n', sep = '')
                g2 <- paste(g2, '            if (missing(j)) {\n', sep = '')
                g2 <- paste(g2, '                x@', field[i], ' <- value\n', sep = '')
                g2 <- paste(g2, '            } else {\n', sep = '')
                g2 <- paste(g2, '                if (j>length(x@', field[i], ')) {\n', sep = '')
                g2 <- paste(g2, '                    stop("[', name, ':set] indice out of limits")\n', sep = '')
                g2 <- paste(g2, '                } else {\n', sep = '')
                g2 <- paste(g2, '                    x@', field[i], '[j] <- value\n', sep = '')
                g2 <- paste(g2, '                }\n', sep = '')
                g2 <- paste(g2, '            }\n', sep = '')
                g2 <- paste(g2, '        }, \n', sep = '')
            }
        }
    }
    stopSet <- paste('"[', name, ':set] ", i, " is not a \\"', name, '\\" slot"', sep = "")
    g3 <- paste("        stop(", stopSet, ")\n", sep = '')
    g4 <- paste('    )\n    validObject(x)\n    return(x)\n})\n\n\n', sep = '')
    cat(g0, g1, g2, g3, g4, sep = "", file = file, append = TRUE)


    h0 <- '### Summary ###\n'
    h1 <- paste('setMethod(f = "summary", signature = "', name , '", definition = function(object){\n', sep = '')
    h2 <- paste('    if (missing(object)){\n', sep = '')
    h3 <- paste('        stop("[', name, ':summary] \\"object\\" is missing", call. = FALSE)\n', sep = '')
    h4 <- paste('        invisible()\n', sep = '')
    h5 <- paste('    } else {}\n', sep = '')
    h6 <- paste('    warning("[', name, ':summary] No summary method defined for \\"', name, '\\" object!", call. = FALSE)\n', sep = '')
    h7 <- paste('    return(object)\n', sep = '')
    h8 <- paste('})\n', sep = '')
    cat(h0, h1, h2, h3, h4, h5, h6, h7, h8, sep = "", file = file, append = TRUE)


    cat("Class file created in", file, "\n")
    invisible()
}

# classS4Creator(name = "myClass", field = c("fieldName1", "fieldName2", "fieldName3", "fieldName4"), type = c("numeric", "list", "matrix", "data.frame"), path = getwd())
