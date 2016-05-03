#scoreData.R
#Author: Morgan Strom
#Date: 29-06-2015

#Inputs:
#data frame "data" to score, with columns named after items
#list "key" containing correct answers, named after items
#Output: matrix of scores for all respondents and items
scoreData = function(data, key) {


  #Check the key argument
  if(class(key) == "data.frame") {
    try(
      key <- scoringKey(key) #Create key list using scoringKey function
    )
  } else if(class(key) != "list") {
    stop("Please make sure that the key is in the correct format,
         with entries named after item id and values corresponding to the correct response")
  }

  #Item scoring function
  #Input: data frame, scoring key and item number
  #Output: vector with all scores for the item
  checkResponse = function(data, key, item_no) {
    item_name = names(key)[item_no]
    value = data[, item_name]

    if(length(key[[item_no]]) == 1) {
      ifelse(test = value == key[[item_no]],
             yes = 1, no = 0)
    } else if(length(key[[item_no]]) == 2) {
      ifelse(test = value >= key[[item_no]][1] & value <= key[[item_no]][2],
             yes = 1, no = 0)
    } else NA
  }

  #Initiate matrix to store scores
  score_matrix <- matrix(nrow = nrow(data), ncol = length(key))

  #Iterate check_response over all items in scoring key
  for(i in 1:length(key)) {
    score_matrix[, i] <- checkResponse(data = data, key = key, item_no = i)
  }

  #Name columns
  colnames(score_matrix) <- names(key)

  #Return matrix
  score_matrix

}

