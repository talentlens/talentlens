\name{scoreDataFrame}
\alias{scoreDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to score an entire data frame
}
\description{
Loops over all items in "key" and returns a score matrix with named columns. NOTE: the rows will be in the same order as the original data frame "df", but the columns may be in a different order.
}
\usage{
scoreDataFrame(df, key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    A data frame containing one candidate per row and one item per column (may also include additional columns, like candidate id etc). NOTE: the column names for the items need to correspond to the item ids in the column "id" in the "key"" data frame.
}
  \item{key}{
    A data frame containing one column for item ids ("id") and one column for the correct responses ("key"). If the correct response is a range, this should be represented in the following format: "RANGE: 1 - 1.1".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A numeric score matrix, where the rows correspond to participants (same order as in the original data frame df) and the columns correspond to item ids (may be in different order from the original data frame). 1 represents a correct response, 0 an incorrect response and NA represents missing data.
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

key <- data.frame(id = c("item_1", "item_2", "item_3"),
                  key = c("2", 3, "RANGE: 1 - 1.1"))

df <- data.frame(id = c("cand_1", "cand_2", "cand_3"),
                 item_1 = c(2, 1.99, 2.01),
                 item_2 = c(3, 2.99, 3.01),
                 item_3 = c(1, 0.99, 1.01))

scoreDataFrame(df, key)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }% use one of  RShowDoc("KEYWORDS")

