\name{targetUKCensus}
\alias{targetUKCensus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate target proportions from UK Census data
}
\description{
Returns a dataframe with 60 rows, with target proportions for each possible subgroup with regards to age, gender, ethnicity and education level
}
\usage{
targetUKCensus()
}
%- maybe also 'usage' for other objects documented here.
\details{
Age is divided into 2 groups: 16-24 and 25-64.
Gender is also divided into 2 groups: Female and Male.
Ethnicity is divided into 5 groups: White, Mixed, Asian, Black/African/Caribbean (BAC) and Other.
Finally, Education is divided into 3 groups: No University Education (Levels 1-3), University Education (Level 4) and Other.
}
\value{
Data frame containing all possible combinations of the groups described above (2 * 2 * 5 * 3 = 60 rows), where each row represent a unique combination. The column "ExpectedP" contains the target proportion for that specific intersection of groups.
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
