\name{lsos}
\alias{lsos}

\title{
Extend \code{ls()} function with memory information
}

\description{
Extend \code{ls()} function with memory information, disks occupancy, \dots
}

\usage{
lsos(pos = 1, pattern, order.by = "Size",
     decreasing = TRUE, head = TRUE, n = 10)
}

\arguments{
    \item{pos}{["numeric"]: An alternative argument to \code{name} (see \code{ls()}) for specifying the environment as a position in the search list.
                    Mostly there for back compatibility.}
    \item{pattern}{["character"]: An optional regular expression. Only names matching \code{pattern} are returned.
                                \code{glob2rx} can be used to convert wildcard patterns to regular expressions.}
    \item{order.by}{["character"]: Column where the resulst should be ordered (e.g. \code{Type}, \code{Size}, \code{PrettySize}, \code{Rows}, \code{Columns}).}
    \item{decreasing}{["logical"]: Should the sort be increasing or decreasing? Not available for partial sorting.}
    \item{head}{["logical"]: Geader should be print.}
    \item{n}{["numeric"]: Number of elements if \code{head=TRUE}.}
}

% \details{}

\value{
Return a table with each row is a R object and columns are \code{Type}, \code{Size}, \code{"PrettySize"}, \code{"Rows"} and \code{"Columns"}.
}

% \references{}
% \author{}
% \note{}
% \seealso{}

\examples{
obj1 <- matrix(NA, 10, 10)
obj2 <- rnorm(100, 0, 1)
lsos()
}

\keyword{lsos}
