#Function to calculate bagged IRT parameters
#Author: Morgan Strom
#Date: 2016-06-21

baggedParams <- function(score_mat, R = 199, model = c("2pl", "rasch"),
                       central.fun = mean, spread.fun = sd) {

  if(class(central.fun) != "function") {
    stop("Please provide a valid function for centrality (e.g. mean, median)")
  }

  if(class(spread.fun) != "function") {
    stop("Please provide a valid function for spread (e.g. sd, mad)")
  }

  #Important variables
  n <- nrow(score_mat)
  k <- ncol(score_mat)
  item_id <- colnames(score_mat)

  #Parameter fitting function
  if (model == "2pl") {
    fitter <- function(mat, i) {
      fit <- ltm::ltm(formula = mat[i,] ~ z1, IRT.param = TRUE,
                      control = list(iter.em = 40, GHk = 15, verbose = FALSE))
      return(coef(fit))
    }
  } else if (model == "rasch") {
    fitter <- function(mat, i) {
      fit <- ltm::rasch(mat[i,], IRT.param = TRUE,
                        control = list(iter.qN = 150, GHk = 21, verbose = FALSE))
      return(coef(fit))
    }
  } else {
    stop("Please provide a IRT model")
  }

  #Bootstrap function
  #params <- boot::boot(score_mat, fitter, R, "ordinary", "i")
  #return(params)

  #Data array for storing parameters
  params <- array(dim = c(k, 2, R),
                  dimnames = list(item_id, c("b", "a"), NULL))

  #Bootstrap samples
  boot_samples <- replicate(R, sample(1:n, n, replace=TRUE))

  #Bootstrap loop
  for (i in 1:R) {
    params[,,i] <- fitter(score_mat, boot_samples[, i])
  }

  #Calculate central parameter value estimations
  central <- apply(params, c(1,2), central.fun)

  #Calculate spread of the parameters
  spread <- apply(params, c(1,2), spread.fun)

  #Return parameters and errors
  return(list(params = central, error = spread))

}
