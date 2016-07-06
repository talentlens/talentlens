\name{eap}
\alias{eap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to estimate ability using EAP algorithm
}
\description{
Takes a named score vector and a data frame with known item parameters, and returns the estimated theta.
}
\usage{
eap(x, params, D = 1.702)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector with correct responses coded as 1 and incorrect responses as 0. NOTE: the values need to be named with item ids
}
  \item{params}{
Data frame with unique item names and corresponding item parameters on each row, NOTE: columns need to be named "id", "a", "b" and "c"
}
  \item{D}{
Scaling constant D (Defaults to 1.702 for normal ogive model)
}
}
\details{
The theta estimation algorithm is based on the implementation of the EAP algorithm in the GetFeedback platform. Instructions from Louis-Charles Vannier.

An alternative to using this function is \code{\link{eapEst}} from the \code{catR} package.
}
\value{
Estimated theta value.
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
  \code{\link{eapEst}}
  \code{\link{eapSem}}
}
\examples{

#Create named score vector
score_vec <- c(1,1,0,1,0)
names(score_vec) <- c("item1", "item2", "item3", "item4", "item5")

#Create parameter data frame
params <- data.frame(id = c("item1", "item2", "item3", "item4", "item5"),
                     a = c(0.7, 0.8, 0.9, 1, 1.1),
                     b = c(-2, -1, 0, 1, 2),
                     c = c(0,0,0,0,0))

#Estimate theta
theta <- eap(score_vec, params)

#Using the function for a matrix with 4 observations
score_mat <- matrix(c(1,1,0,1,0,
                      1,0,0,0,0,
                      1,1,1,1,0,
                      1,1,1,1,1),
                    nrow = 4, ncol = 5, byrow = TRUE)
colnames(score_mat) <- c("item1", "item2", "item3", "item4", "item5")

thetas <- apply(score_mat, 1, eap, params=params)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }

