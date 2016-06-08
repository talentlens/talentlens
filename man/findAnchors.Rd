\name{findAnchors}
\alias{findAnchors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to find a set of optimal anchors from a score matrix
}
\description{
Given a numeric score matrix, where the columns are named after item ids and each row represents a participant, the function will by default return a matrix of IRT parameters
}
\usage{
findAnchors(score_matrix, model = c("rasch", "2pl"), min_cor = 0.2, min_p = 0.1, max_p = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score_matrix}{
    Numeric matrix with observed scores. The columns need to be named.
}
  \item{model}{
    Which IRT model do you wish to apply? The alternatives are "rasch" or "2pl"
}
  \item{min_cor}{
    The minimum allowed item-total biserial correlation for anchor items. Defaults to 0.2, to avoid numerical problems when fitting IRT parameters.
}
  \item{min_p}{
    The minimum allowed proportion of participants answering the item correctly. Defaults to 0.1, to avoid numerical problems when fitting IRT parameters.
}
  \item{max_p}{
    The maximum allowed proportion of participants answering the item correctly - i.e. how easy we allow them to be. Defaults to 0.9, to avoid numerical problems when fitting IRT parameters.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (score_matrix, model = c("rasch", "2pl"), min_cor = 0.2,
    min_p = 0.1, max_p = 0.9, include_dropped = FALSE)
{
    out <- list()
    p <- colMeans(score_matrix)
    easy <- p >= max_p
    out$drop$easy <- p[easy]
    hard <- p <= min_p
    out$drop$hard <- p[hard]
    cor <- numeric(ncol(score_matrix))
    for (i in 1:ncol(score_matrix)) {
        cor[i] <- ltm::biserial.cor(x = rowSums(score_matrix,
            na.rm = TRUE), y = score_matrix[, i], level = 2,
            use = "complete.obs")
        names(cor)[i] <- colnames(score_matrix)[i]
    }
    low <- cor <= min_cor
    out$drop$low_cor <- cor[low]
    score_matrix_ <- score_matrix[, !low & !easy & !hard]
    if (model == "rasch") {
        fit <- ltm::rasch(score_matrix_, IRT.param = TRUE, control = list(iter.qN = 150,
            GHk = 21, verbose = TRUE))
    }
    else if (model == "2pl") {
        fit <- ltm::ltm(formula = score_matrix_ ~ z1, IRT.param = TRUE,
            control = list(iter.em = 40, GHk = 21, verbose = TRUE))
    }
    else {
        stop("Please choose IRT model")
    }
    plot(fit)
    items <- item.fit(fit)
    bad_fit <- items$p.values < 0.05
    out$drop$bad_fit <- items$Tobs[bad_fit]
    score_matrix_ <- score_matrix_[, !bad_fit]
    out$anchors <- coefficients(fit)[!bad_fit, ]
    if (include_dropped) {
        return(out)
    }
    else {
        return(out$anchors)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
