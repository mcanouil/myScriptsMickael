lsos <- function(pos = 1, pattern, order.by = "Size", decreasing = TRUE, head = TRUE, n = 10) {
    .ls.objects <- function (pos, pattern, order.by, decreasing, head, n) {
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
        obj.type <- sapply(seq_along(obj.class), function (i) {
            if (is.na(obj.class[i])) {
                obj.mode[i]
            } else {
                obj.class[i]
            }
        })
        obj.prettysize <- napply(names, function(x) {
            utils::capture.output(print(utils::object.size(x), units = "auto"))
        })
        obj.size <- napply(names, utils::object.size)
        obj.dim <- t(napply(names, function(x) {
            as.numeric(dim(x))[c(1, 2)]
        }))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
        names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
        if (!missing(order.by)) {
            out <- out[order(out[[order.by]], decreasing = decreasing), ]
        } else {}
        if (head) {
            if (is.null(n)) {
                n <- nrow(out)
            } else {}
            out <- utils::head(out, n)
        } else {}
        return(out)
    }
    .ls.objects(pos = pos, pattern, order.by = order.by, decreasing = decreasing, head = head, n = n)
}
# lsos()