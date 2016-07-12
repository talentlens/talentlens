#scoreDataFrame.R
#Author: Morgan Strom
#Date: 2016-06-17

#Inputs:
#data frame "df" to score, with columns named after items
#list "key" containing correct answers, named after items
#Output: matrix of scores for all respondents and items
scoreDataFrame <- function(df, key) {

  #Check the key argument
  if(class(key) == "data.frame") {
    if(all(c("id", "key") %in% names(key))) {
      key <- scoringKey(key) #Create key list using scoringKey function
    } else {
      stop("The 'key' dataframe do not contain the columns 'id' and 'key'.")
    }

  } else if(class(key) != "list") {
    stop("Please make sure that the key is in the correct format.
         Type '?scoringKey' for help.")
  }

  #Initiate matrix to store scores
  score_matrix <- matrix(nrow = nrow(df), ncol = length(key))

  #Iterate scoreResponse over all items in scoring key
  for(i in 1:length(key)) {
    item_id <- names(key)[i]
    score_matrix[, i] <- scoreResponses(responses = df[, item_id],
                                        key[[item_id]],
                                        item_id)
  }

  #Name columns
  colnames(score_matrix) <- names(key)

  #Return matrix
  score_matrix

}

