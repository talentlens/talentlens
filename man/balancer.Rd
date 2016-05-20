\name{balancer}
\alias{balancer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to balance a sample with regards to a dominant subgroup
}
\description{
  Takes a group variable (factor vector) f and the desired maximum proportion of the largest subgroup (factor level) p, samples randomly from the largest group and returns a logical vector, indicating which cases to retain to reach the desired proportion of the largest group.
}
\usage{
  balancer(f, p, silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
    Factor vector representing a group variable to be balanced, where the levels represent groups/categories in the data. For example, this could be a column in a dataframe, indicating the gender of the participants.
  }
  \item{p}{
    Desired maximum proportion for the largest subgroup. For example, if we want a balanced sample with regards to gender, we may want to allow for no more than 50 \% of the total sample to be from either gender.
  }
  \item{silent}{
    If TRUE (default), messages are not printed to the console. Set to FALSE if you want to see how many cases are dropped randomly.
  }
}
\details{
Formula for determining the necessary reduction of the sample size (delta_n) for the dominant subgroup: \deqn{\Delta n = \frac{ N(p_2 - p_1) } {(1 - p_2)} }{delta_n = N (p2 - p1) / (1 - p2)}
}
\value{
Logical vector of the same length as f, with values TRUE for cases to keep and FALSE for cases to drop.
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
##Generate factor vector with unbalanced levels
f <- factor(x = c(rep("A", 20), rep("B", 50), rep("C", 30)))
table(f)

#Calculate proportions of the groups
table(f) / length(f)

#Use balancer() function to reduce the largest group to 40\%

#First, create the index vector
i <- balancer(f, p = 0.40, silent = FALSE)

#Second, subset the factor vector and store in a new object, f2
f2 <- f[i]

#Calculate new proportions
table(f2) / length(f2)

}

\keyword{ ~misc }

