\name{findAnchors}
\alias{findAnchors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to suggest optimal anchor items from a score matrix
}
\description{
This function selects optimal anchor items in two steps: first, based on CTT analyses (proportion of participants answering the item correctly and item-total biserial correlation) and second, based on a chi-square test of item fit under the chosen IRT model.
}
\usage{
findAnchors(score_matrix, model = c("rasch", "2pl"),
bagging = FALSE, R = NA,
min_cor = 0.2, min_p = 0.1, max_p = 0.9, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score_matrix}{
    A numeric matrix where each row represent the scores of a participant, and each row represent a item. NOTE the columns need to be named with item ids!
}
  \item{model}{
    The IRT model to use for parameter fitting. Either "rasch" (for smaller sample sizes) or "2pl" (for larger sample sizes)
}
  \item{bagging}{
    If TRUE, the parameters will be estimated using bagging (see package baggedParams). If FALSE, parameters will be estimated using one single run of the item fitting algorithm.
}
  \item{R}{
    For bagging, this is the number of bootstrap iterations.
}
  \item{min_cor}{
    The minimum allowed item-total biserial correlation. Default: 0.2
}
  \item{min_p}{
    The minimum allowed proportion of participants answering a given item correctly. Default: 0.1
}
  \item{max_p}{
    The maximum allowed proportion of participants answering a given item correctly. Default: 0.9
}
  \item{verbose}{
    If TRUE, information about the item selection will be printed to the console.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A two-column matrix of item parameters for the suggested anchor items. The rows are named after the item ids, and the columns are named "b" (difficulty parameter) and "a" (discrimination parameter).
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }% use one of  RShowDoc("KEYWORDS")

