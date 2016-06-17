#scoreResponses.R
#Author: Morgan Strom
#Date: 2016-06-17

#Input: vector of responses, key and item id for the given item
#Output: vector with all scores for the item
scoreResponses <- function(responses, key, item_id) {
  out <- numeric(length = length(responses))

  if(is.na(key)) {
    stop(paste0("The item ", item_id, "doesn't have a key"))
  } else if(length(key) == 1) {
    out <- ifelse(test = responses == key,
           yes = 1, no = 0)
  } else if(length(key) == 2) {
    out <- ifelse(test = responses >= key[1] &
                    responses <= key[2],
           yes = 1, no = 0)
  } else {
    warning(paste0("The item ", item_id, " did not have a valid key."))
    out <- rep(NA, length(responses))
  }

  return(out)

}
