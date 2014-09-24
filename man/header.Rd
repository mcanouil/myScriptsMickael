\name{header}
\alias{header}
\alias{h}

\title{
Return the First Part of an Object
}

\description{
Returns the first parts of a vector, matrix, table, data frame or function.  Since \code{header()} and \code{h()} are generic functions, they may also have been extended to other classes.
}

\usage{
header(x, n = 10)
h(x, n = 10)
}

\arguments{
    \item{x}{an object}
    \item{n}{a single integer. If positive, size for the resulting object:
                number of elements for a vector (including lists), rows for a
                matrix or data frame or lines for a function. If negative,
                all but the \code{n} first number of elements of \code{x}.}
}

\details{
For matrices, 2-dim tables and data frames, \code{head()} (\code{tail()})
returns the first (last) \code{n} rows when \code{n > 0} or all but the last
(first) \code{n} rows when \code{n < 0}.  \code{head.matrix()} and
\code{tail.matrix()} are exported.  For functions, the lines of the
deparsed function are returned as character strings.

If a matrix has no row names, then \code{tail()} will add row names of
the form \code{[n,]} to the result, so that it looks similar to the
last lines of \code{x} when printed.  Setting \code{addrownums = FALSE}
suppresses this behaviour.
 }

\value{
An object (usually) like \code{x} but generally smaller.  For \code{ftable}
objects \code{x}, a transformed \code{format(x)}.
}

% \references{}
% \author{}
% \note{}
% \seealso{}

\examples{
head(letters)
head(letters, n = -6L)
head(freeny.x, n = 10L)
head(freeny.y)
}

\keyword{header}
\keyword{h}
