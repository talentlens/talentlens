#fetchConcertoData.R
#Author: Morgan Strom
#Date: 2016-05-03

#Function to grab data from concerto
#Input: strings database name, host (IP), user & password
#backup - if TRUE (default), the summary and response data will be saved to wd
#Output: Data frame with demographic info and item scores
#Writes 2 csv files in wd: demographic data and item responses

fetchConcertoData <- function(dbname, host, user, password, backup = TRUE) {
  #Requires libraries RMySQL, dplyr & tidyr

  #Fetch data --------------------------------------------------

  print("Fetching data from Concerto...")

  #Open connection
  con <- RMySQL::dbConnect(RMySQL::MySQL(), host = host,
                   user = user, password = password,
                   dbname = dbname)

  #Get responses
  res <- RMySQL::dbSendQuery(con, "select * from candidate_responses")

  #Fetch data in chunks of 1000
  candidate_responses <- data.frame()
  while (!RMySQL::dbHasCompleted(res)) {
    chunk <- RMySQL::dbFetch(res, 1000)
    candidate_responses <- rbind(candidate_responses, chunk)
  }

  #Clear result set
  RMySQL::dbClearResult(res)

  #Get summaries
  res <- RMySQL::dbSendQuery(con, "select * from candidate_summary")

  #Fetch data in chunks of 1000
  candidate_summary <- data.frame()
  while (!RMySQL::dbHasCompleted(res)) {
    chunk <- RMySQL::dbFetch(res, 1000)
    candidate_summary <- rbind(candidate_summary, chunk)
  }

  #Clear result set
  RMySQL::dbClearResult(res)

  #Close connection
  RMySQL::dbDisconnect(con)

  #Write backup data to csv in the working directory
  if (backup) {
    write.csv(candidate_summary,
              file = paste0(dbname, "_summary_", Sys.Date(), ".csv"),
              row.names = FALSE, na = "")
    write.csv(candidate_responses,
              file = paste0(dbname, "_responses_", Sys.Date(), ".csv"),
              row.names = FALSE, na = "")

    print(paste("Done! Data is saved in directory", getwd()))
  }


  #Data processing ----------------------------------------------------

  print("Processing data...")

  #Make sure data is in numeric format
  suppressWarnings(candidate_responses$response <- as.numeric(candidate_responses$response))
  suppressWarnings(candidate_responses$correct <- as.numeric(candidate_responses$correct))
  suppressWarnings(candidate_responses$time_taken <- as.numeric(candidate_responses$time_taken))

  #Make sure session_id is character
  suppressWarnings(candidate_responses$session_id <- as.character(candidate_responses$session_id))
  suppressWarnings(candidate_summary$session_id <- as.character(candidate_summary$session_id))

  #Get last, not NA, response to each item
  last_responses <- candidate_responses %>%
    dplyr::filter(!is.na(response)) %>%
    dplyr::arrange(desc(id)) %>%
    dplyr::group_by(session_id) %>%
    dplyr::distinct(item_id)

  #Get unique session_id in candidate_summary
  dup <- duplicated(candidate_summary$session_id)
  if (any(dup)) {

    warning(paste("Dropping duplicated session_id in the database:",
                  candidate_summary$session_id[dup], "\n"))

    candidate_summary <- candidate_summary[!dup,]

  }

  #Get score matrix
  score_matrix <- last_responses %>%
    dplyr::select(session_id, item_id, correct) %>%
    tidyr::spread(key = item_id, value = correct)

  #Calculate total raw score and number of items attempted
  score_matrix$n_attempted <- apply(score_matrix[, -1], 1, function(x) sum(!is.na(x)))
  score_matrix$raw_score <- rowSums(score_matrix[, -1], na.rm = TRUE)

  #Merge tables
  complete_data <- dplyr::inner_join(candidate_summary, score_matrix,
                              by = c("session_id" = "session_id"))



  print("Done!")

  return(complete_data)

}
