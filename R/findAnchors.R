#Function to propose anchor items
#Author: Morgan Strom
#Date: 2016-06-08

#Input: score matrix with named columns, IRT model and criteria
#for dropping items (minimum correlation allowed, minimum proportion
#correct responses and maximum proportion correct responses)
#Output: item parameters for the best items or a list of 2

findAnchors <- function(score_matrix, model = c("rasch", "2pl"),
                        bagging = FALSE, R = NA,
                        min_cor = 0.2, min_p = 0.1, max_p = 0.9,
                        verbose = FALSE) {

  #Check if score_matrix has any missing values
  if (any(is.na(score_matrix))) {
    warning("There are missing values in score_matrix.")
  }

  #CTT analyses -------------------------------

  drop <- list()

  #Percent correct (items)
  p <- colMeans(score_matrix, na.rm=TRUE)

  easy <- p >= max_p
  drop$easy <- p[easy] #Items with high proportions of correct answers
  hard <- p <= min_p
  drop$hard <- p[hard] #Items with low proportions

  #Item-total correlations
  cor <- numeric(ncol(score_matrix))
  for(i in 1:ncol(score_matrix)) {

    cor[i] <- ltm::biserial.cor(x = rowSums(score_matrix, na.rm = TRUE),
                                y = score_matrix[,i],
                                level = 2, use = "complete.obs")
    names(cor)[i] <- colnames(score_matrix)[i]

  }

  low <- cor <= min_cor
  drop$low_cor <- cor[low] #Items with low item-total correlations

  #Drop bad items
  score_matrix_ <- score_matrix[, !(low | easy | hard)]

  #Print messages to console
  if (verbose) {

    cat(paste("Dropping", sum(low | easy | hard), "items\n"))

    if(length(drop$easy > 0)) {
      cat(paste("\nToo easy (prop correct >", max_p, "):\n"))
      for (i in 1:length(drop$easy)) cat(paste0("\t", names(drop$easy)[i], ": ", round(drop$easy[i], 2), "\n"))
    }

    if(length(drop$hard > 0)) {
      cat(paste("\nToo hard (prop correct <", min_p, "):\n"))
      for (i in 1:length(drop$hard)) cat(paste0("\t", names(drop$hard)[i], ": ", round(drop$hard[i], 2), "\n"))
    }

    if(length(drop$low_cor > 0)) {
      cat(paste("\nLow item-total correlation (biserial r <", min_cor, "):\n"))
      for (i in 1:length(drop$low_cor)) cat(paste0("\t", names(drop$low_cor)[i], ": ", round(drop$low_cor[i], 2), "\n"))
    }
  }

  #IRT analyses -------------------------------

  if (!bagging) {
    #Rasch model
    if (model == "rasch") {
      if (verbose) cat("\nFitting IRT parameters:\n")
      fit <- ltm::rasch(score_matrix_, IRT.param = TRUE,
                        control = list(iter.qN = 150, GHk = 21, verbose = verbose))

      #2PL
    } else if (model == "2pl") {
      if (verbose) cat("\nFitting IRT parameters:\n")
      fit <- ltm::ltm(formula = score_matrix_ ~ z1, IRT.param = TRUE,
                      control = list(iter.em = 40, GHk = 21, verbose = verbose))

    } else {
      stop("Please choose IRT model")
    }

    #Indicate items with bad fit
    items <- item.fit(fit)
    bad_fit <- items$p.values < 0.05
    drop$bad_fit <- items$Tobs[bad_fit]

    if (verbose & length(drop$bad_fit > 0)) {
      cat("\nNOTE: Items not fitting the model:\n")
      for(i in 1:length(drop$bad_fit)) cat(paste0("\t", names(drop$bad_fit)[i], "\n"))
    }

    #Anchor item coefficients
    anchors <- coefficients(fit)
    colnames(anchors) <- c("b", "a")

  } else {

    if(is.na(R)) R <- 199

    #Bagged IRT parameters
    anchors <- baggedParams(score_matrix_, R, model)

  }

  #Return optimal anchor items -------------------

  return(anchors)

}
