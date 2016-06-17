\name{scoreResponses}
\alias{scoreResponses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to score a vector of responses
}
\description{
Returns a vector of scores, given a vector of responses to a specific item, a vector with the correct response(s) ("key") and the item id.
}
\usage{
scoreResponses(responses, key, item_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{responses}{
    A numeric vector with the responses to an item
}
  \item{key}{
    A numeric vector with the correct response to the item. If the response format is Multiple Choice, this will be a single integer value. If the response format is Free (such as some items in Athena), this will be two values representing the lower and the upper bounds of the range of acceptable responses.
}
  \item{item_id}{
    A character vector with the item id
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A numeric score vector with 1 for correct responses, 0 for incorrect responses and NA for missing values
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Morgan Strom
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

responses <- c(1, 0, 1.05, 0.99, 2)

#Multiple choice with 2 as the correct answer
key <- 2
scoreResponses(responses, key, "test_item")

#Free response with lower bound 1 and upper bound 1.1
key <- c(1, 1.1)
scoreResponses(responses, key, "test_item")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }% use one of  RShowDoc("KEYWORDS")

