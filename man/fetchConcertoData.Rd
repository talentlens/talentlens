\name{fetchConcertoData}
\alias{fetchConcertoData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fetch data from Concerto
}
\description{
Returns candidate scores and demographics in a data frame format
}
\usage{
fetchConcertoData(dbname, host, user, password, backup = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbname}{
String containing the name of the database where the test is stored. E.g "concerto_2" for the UK version of Watson-Glaser. This information can be found in the spreadsheet "Concerto data collection" on Google Drive.
}
  \item{host}{
String containing the IP address of the Concerto server. This information can also be found in the spreadsheet "Concerto data collection" on Google Drive.
}
  \item{user}{
String containing the user name for accessing the database. To get access to this information, please email the author of this package.
}
  \item{password}{
String containing the password for accessing the database. To get access to this information, please email the author of this package.
}
  \item{backup}{
Boolean value - if TRUE (default), all candidate information from the server (tables candidate_summary and candidate_responses) will be stored as .csv files in the working directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

  \item{complete_data }{A dataframe containing both demographic information and item scores from the concerto test. If you stored this in a dataframe called \code{df}, and you wish to save this as a .csv file in your working directory, type \code{write.csv(df, file = "df.csv", na = "", row.names = FALSE)} in the console and press Enter.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Morgan Strom, TalentLens UK
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
\keyword{ ~dplyr }% __ONLY ONE__ keyword per line
