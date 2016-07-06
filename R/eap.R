#Functions to estimate ability using EAP algorithm
#Author: Morgan Strom
#Date: 2016-07-06

eap <- function(x, params, D=1.702) {

  #Define item parameters
  items <- names(which(!is.na(x))) #Identify answered items
  i <- which(params$id %in% items) #Find indicies for these items
  x <- x[i]

  #Difficulty (Threshold) parameter b (1PL, 2PL and 3PL model)
  b <- params$b[i]

  #Slope parameter a (2PL and 3PL models)
  a <- rep(1, length(items)) #a parameters set to 1 (1PL model)
  if("a" %in% names(params)) { #If there are a values in params
    a <- params$a[i] #then set a parameters
  }

  #Guessing parameter c (3PL model)
  c <- rep(0, length(items)) #c parameters set to 0 (1PL and 2PL models)
  if("c" %in% names(params)) { #If there are c values in params
    c <- params$c[i] #then set c parameters
  }


  #1. Compute prior distribution with 33 quadrature points
  theta <- seq(from=-4, to=4, by=0.25)
  prior <- stats::dnorm(theta, mean=0, sd=1)


  #2. Compute probability of correctly answering the items
  #given parameters a, b and c as well as scaling constant D
  #Store in a matrix with k columns (number of items)
  #and 33 rows (different levels of theta)

  #Vectorized item probability function for 3PL model
  #Input:
  #Item parameter vectors "a", "b" and "c" of length k
  #Theta vector "thetas" of length n
  #Output:
  #k x n probability matrix for n theta values and k items
  p_thetas <- function(thetas, a, b, c, D) {

    #Function to calculate k x 1 exponent vector for k items at a particular theta
    L_fun <- function(theta, a, b, D) {
      L <- D * a * (theta - b)
    }

    #Calculate k x n exponent matrix for k items and n thetas
    L_matrix <- sapply(X = thetas, FUN = L_fun, a=a, b=b, D=D)

    #Calculate k x n probability matrix for k items and n thetas (3PL)
    p_matrix <- c + (1 - c) / (1 + exp(-L_matrix))

    #Return k x n probability matrix
    p_matrix

  }

  #Apply function to 33  quadrature points and store in matrix
  p <- p_thetas(theta, a=a, b=b, c=c, D)
  rownames(p) <- items
  p <- t(p) #Transpose to a n x k matrix


  #3. Compute joint probability of the response pattern

  #Keep p for correctly answered items
  p_i <- p

  #Replace with q = 1 - p for incorrectly answered items
  correct <- x == 1 #Index vector
  p_i[, !correct] <- 1 - p[, !correct]

  #Joint probability = geometrical sum of item response probabilities
  p_joint <- apply(X=p_i, MARGIN=1, FUN=prod)

  #4. Compute denominator
  #p(theta) * p(x | theta)
  den <- prior * p_joint

  #5. Compute numerator
  #theta * p(theta) * p(x | theta)
  num <- theta * prior * p_joint

  #6. Integration by successive summation

  #Calculate areas under the denominator curve
  width <- rep(0.25, length(theta))
  height_den <- rowMeans(cbind(den[1:33], den[c(2:33, 33)]))
  area_den <- width * height_den

  #Sum areas under the denominator curve
  sum_den <- sum(area_den)

  #Calculate areas under the numerator curve
  height_num <- rowMeans(cbind(num[1:33], num[c(2:33, 33)]))
  area_num <- width * height_num

  #Sum areas under the numerator curve
  sum_num <- sum(area_num)


  #7. Compute person ability
  ability = sum_num / sum_den

  #Return ability estimate
  ability

}
