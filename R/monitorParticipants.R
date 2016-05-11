#monitorParticipants.R
#Author: Morgan Strom
#Date: 2016-05-09

#Function to summarise performance of participants from a Concerto test
#Returns an ordered dataframe:
#(session_id)
#Email address of the participant
#Name
#IP address
#Number of attempted items
#Total time spent on the test

monitorParticipants <- function(dbname, host, user, password) {
  #Open connection to the database
  con <- RMySQL::dbConnect(RMySQL::MySQL(), host = host,
                           user = user, password = password,
                           dbname = dbname)

  #Query number of attempted items, and total time taken per session_id
  res <- RMySQL::dbSendQuery(con,
    "SELECT b.session_id, a.worker_id, a.ip,
     COUNT(distinct(b.item_id)) as attempted,
     SUM(b.time_taken) as total_time
     FROM candidate_summary a LEFT JOIN candidate_responses b
     ON a.session_id=b.session_id
     GROUP BY b.session_id;")

  #Fetch data
  monitor <- RMySQL::dbFetch(res, -1)

  #Clear result set
  RMySQL::dbClearResult(res)


  #Close connection
  RMySQL::dbDisconnect(con)

  return(monitor)

}
