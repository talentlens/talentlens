\name{baggedParams}
\alias{baggedParams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate bagged IRT parameters from a score matrix
}
\description{
Performs IRT fitting algorithm repeatedly (R times) on a random sample of the score matrix (with replacement), according to a bootstrap procedure. The results are then aggregated (bagging) and a list of parameter and error estimates is returned.
}
\usage{
baggedParams(score_mat, R, model = c("2pl", "rasch"),
central.fun = mean, spread.fun = sd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score_mat}{
    (nxk) numeric matrix rows corresponding to participants and columns corresponding to items. NOTE: columns need to be named after item ids.
}
  \item{R}{
    Number of iterations for the bootstrap loop. Higher R increases the time the function needs for completion, but also gives better estimates of bagged parameters and bootstrapped errors. Default: 199 (on a Dell laptop, this takes about 1 min to run on a dataset with 99 rows).
}
  \item{model}{
    The IRT model to use. This function currently supports rasch and 2pl models.
}
  \item{central.fun}{
    The function used for aggregating results and find the central values. Default: mean.
}
  \item{spread.fun}{
    The function used for estimating parameter errors. Default: sd.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

  \item{params }{A (kx2) matrix of IRT parameters}
  \item{error }{A (kx2) matrix of estimation errors}

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
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

