lsos <- function(pos = 1, pattern, order.by = "Size", decreasing = TRUE, head = TRUE, n = 10) {
    .ls.objects <- function (pos = 1, pattern, order.by, decreasing = FALSE, head = FALSE, n = 5) {
        napply <- function(names, fn) {
            sapply(names, function(x) {
                fn(get(x, pos = pos))
            })
        }
        names <- ls(pos = pos, pattern = pattern)
        if (length(names) == 0) {
            return(character(0))
        } else {}
        obj.class <- napply(names, function(x) {
            as.character(class(x))[1]
        })
        obj.mode <- napply(names, mode)
        obj.type <- if (is.na(obj.class)) {
            obj.mode
        } else {
            obj.class
        }
        obj.prettysize <- napply(names, function(x) {
            utils::capture.output(print(utils::object.size(x), units = "auto"))
        })
        obj.size <- napply(names, utils::object.size)
        obj.dim <- t(napply(names, function(x) {
            as.numeric(dim(x))[1:2]
        }))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
        names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
        if (!missing(order.by)) {
            out <- out[order(out[[order.by]], decreasing = decreasing), ]
        } else {}
        if (head) {
            out <- utils::head(out, n)
        } else {}
        return(out)
    }
    .ls.objects(pos = 1, pattern, order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}
# lsos()