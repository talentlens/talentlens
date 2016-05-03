#fetchConcertoData.R
#Author: Morgan Strom
#Date: 2016-05-03

#Function to grab data from concerto
#Input: strings database name, host (IP), user & password
#Output: Data frame with demographic info and item scores
#Writes 2 csv files in wd: demographic data and item responses

fetchConcertoData <- function(dbname, host, user, password) {
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

  #Get summaries
  res <- RMySQL::dbSendQuery(con, "select * from candidate_summary")

  #Fetch data in chunks of 1000
  candidate_summary <- data.frame()
  while (!RMySQL::dbHasCompleted(res)) {
    chunk <- RMySQL::dbFetch(res, 1000)
    candidate_summary <- rbind(candidate_summary, chunk)
  }

  #Close connection
  suppressWarnings(RMySQL::dbDisconnect(con))

  #Write raw data to csv
  write.csv(candidate_summary,
            file = paste0(dbname, "_summary_", Sys.Date(), ".csv"),
            row.names = FALSE, na = "")
  write.csv(candidate_responses,
            file = paste0(dbname, "_responses_", Sys.Date(), ".csv"),
            row.names = FALSE, na = "")

  print(paste("Done! Data is saved in directory", getwd()))

  #Data processing ----------------------------------------------------

  print("Processing data...")

  #Make sure data is in numeric format
  suppressWarnings(candidate_responses$response <- as.numeric(candidate_responses$response))
  suppressWarnings(candidate_responses$correct <- as.numeric(candidate_responses$correct))
  suppressWarnings(candidate_responses$time_taken <- as.numeric(candidate_responses$time_taken))

  #Make sure session_id is character
  suppressWarnings(candidate_responses$session_id <- as.character(candidate_responses$session_id))

  #Get last response to each item
  last_responses <- candidate_responses %>%
    dplyr::arrange(desc(id)) %>%
    dplyr::group_by(session_id) %>%
    distinct(item_id)

  #Get score matrix
  score_matrix <- last_responses %>%
    dplyr::select(session_id, item_id, correct) %>%
    tidyr::spread(key = item_id, value = correct)

  #Merge tables
  complete_data <- inner_join(candidate_summary, score_matrix,
                              by = c("session_id" = "session_id"))

  print("Done!")

  return(complete_data)

}
