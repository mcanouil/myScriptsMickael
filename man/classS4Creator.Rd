\name{classS4Creator}
\alias{classS4Creator}

\title{
\code{classS4Creator} generate S4 class
}

\description{
\code{classS4Creator} is a class S4 generator which produces R code for setter/getter/show/constructor methods for an S4 object.
}

\usage{
classS4Creator(name, field, type, path = getwd())
}

\arguments{
    \item{name}{["character"]: Name of the class S4.}
    \item{field}{["character"]: Name of the slots in the S4 object.}
    \item{type}{["character"]: Type of the slots in the S4 object. (e.g. \code{numeric}, \code{character}, \dots).}
    \item{path}{["character"]: A character vector of full path names; the default corresponds to the working directory, \code{getwd()}.
                Tilde expansion (see \code{path.expand}) is performed.}
}

% \details{}

\value{
Return a R file with all R code to generate an S4 class.
}

% \references{}
% \author{}
% \note{}
% \seealso{}

\examples{
classS4Creator(name = "myClass",
               field = c("fieldName1", "fieldName2", "fieldName3", "fieldName4"),
               type = c("numeric", "list", "matrix", "data.frame"), path = "")
classS4Creator(name = "myClass",
               field = c("fieldName1", "fieldName2", "fieldName3", "fieldName4"),
               type = c("numeric", "list", "matrix", "data.frame"), path = getwd())
}

\keyword{classS4Creator}
