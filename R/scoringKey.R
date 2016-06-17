#scoringKey.R
#Author: Morgan Strom
#Date: 18-08-2015

#Function to create scoring key from dataframe with scoring key
#Input:
#Data frame "key.df" containing item ID and keys
#(Name of Item ID column = "id", Key column = "key")
#Output:
#List containing scoring key
scoringKey <- function(key.df) {

  #Make sure that the key column is in character format
  key.df$key <- as.character(key.df$key)

  #Identify scoring ranges for open form items
  range <- grepl("RANGE:", key.df$key)

  #Extract minimum correct reply
  min <- substr(key.df$key[range], start=8, stop=regexpr("-", key.df$key[range]) - 1)
  min <- as.numeric(gsub(" ", "", min))

  #Extract maximum correct reply
  max <- substr(key.df$key[range], start=regexpr("-", key.df$key[range]) + 1, stop=100)
  max <- as.numeric(gsub(" ", "", max))

  #Create key as list
  key <-list()

  #Closed form items
  for(i in 1:nrow(key.df)) {
    key[[i]] = as.numeric(key.df[i, "key"])
  }

  #Open form items
  j <- 1
  for(i in which(range)) {
    key[[i]] = c(min[j], max[j])
    j <- j + 1
  }

  #Item names
  names(key) <- as.character(key.df$id)

  #Return list
  key
}
