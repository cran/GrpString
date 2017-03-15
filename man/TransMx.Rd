\name{TransMx}
\alias{TransMx}
\title{
Transition matrices in one group of strings
}
\description{
TransMx discovers transition matrix of a string vector and the related information.

    A transition is defined as a substring (in the forward order) with length of 2 characters.
It can be considered as a special common pattern (length of 2).
}
\usage{
TransMx(strings.vec, indiv = FALSE)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
  \item{indiv}{
Whether export transition matrix for each string in the current directory. Default value is FALSE.
}
}

\value{
The function returns a list, which contains the transition matrix, the normalized matrix, and the sorted numbers of transitions.
}
\seealso{
TransInfo
}
\examples{
# simple strings
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransMx(strs.vec)

# simple strings, export an individual transition matrix for each string
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransMx(strs.vec, indiv = TRUE)
}

\keyword{programming}
