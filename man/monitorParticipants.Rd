\name{monitorParticipants}
\alias{monitorParticipants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to monitor participants from MTurk projects
}
\description{
Provides MTurk Worker ID along with time of completion, number of attempted items and total time for completion (in seconds).
}
\usage{
monitorParticipants(dbname, host, user, password)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame containing the columns \code{session_id}, \code{worker_id}, \code{timestamp}, \code{attempted} and \code{total_time}.
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
\keyword{ ~IO }
\keyword{ ~misc }
