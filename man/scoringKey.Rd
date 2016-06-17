\name{scoringKey}
\alias{scoringKey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a scoring key for the \code{scoreData} function
}
\description{
Creates a list with one entry per item, where the name is the item id and the value is the correct response. The value can either be a single number, when there is only one correct answer, or a vector of two numbers, where the correct answer is within a range (Athena items for example)
}
\usage{
scoringKey(key.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key.df}{
    Data frame containing one column with item ID (named "id") and one column with the corresponding correct responses (named "key"). If the correct response is a range, use the following format: "RANGE: 1 - 1.1".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List with one entry per item. The entry is named after the item id, and the value is the correct response to the item.
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

df <- data.frame(id = c("item_1", "item_2", "item_3"),
                 key = c("2", 3, "RANGE: 1 - 1.1"))

scoringKey(df)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }% use one of  RShowDoc("KEYWORDS")

