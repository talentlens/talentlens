#balancer.R
#Function to identify the biggest level of a categorical variable f,
#subsample from this level to reduce the proportion to p,
#and return an index vector with TRUE for the cases to keep and FALSE
#for the cases to drop.

#Input:
#f - factor vector representing the categorical variable to balance
#p - desired proportion of largest factor level
#silent - logical value indicating if you want messages printed to the console

#Output:
#Logical index vector indicating which rows to keep and
#which to drop after undersampling

balancer <- function(f, p, silent = TRUE) {

  if(!is.factor(f)) {
    print("Converting to factor class")
    f <- factor(f)
  }

  #Find the name of the most frequent factor level
  largest <- names(which.max(table(f)))

  if (!silent) print( paste("The largest level is", largest) )

  #Create numeric index vector for cases with this factor level
  i_largest <- which(f == largest)

  #Total number of cases
  N <- length(f)

  #Calculate current proportion of largest factor level
  p1 <- length(i_largest) / N

  #Calculate delta_n = required change in number of cases
  #in the largest level to reach desired proportion p
  delta_n <- round(N * (p1 - p) / (1 - p))

  #If delta_n is positive, the largest factor level needs to be
  #reduced to reach the desired proportion.

  if (delta_n > 0) {
    if (!silent) print( paste("Dropping", delta_n, "cases from this level") )

    #Sample delta_n cases from the largest factor level
    redundant <- base::sample(x = i_largest, size = delta_n)

    #Create logical index vector for cases to keep
    i_keep <- rep(TRUE, N)

    #Set redundant cases to FALSE
    i_keep[redundant] <- FALSE

    return(i_keep)

  } else {

    if (!silent) print( paste("No cases dropped - proportion is",
                              round(p1, 2)) )
    #If delta_n is negative, return an index vector with all TRUE
    return(rep(TRUE, N))
  }

}
