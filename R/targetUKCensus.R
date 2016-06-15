#Function to calculate target proportions from UK Census data
#Author: Morgan Strom
#Date: 2016-06-15


#Calculates expected proportions, based on age, gender, ethnicity and education
targetUKCensus <- function() {
  #Targets in wide format
  gender <- data.frame(Age = c("16-24", "25-64"),
                       Male = c(0.506112445699143, 0.495260749537577),
                       Female = c(0.493887554300857, 0.504739250462423))

  #rowSums(gender[, -1])

  ethnicity <- data.frame(Age = c("16-24", "25-64"),
                          White = c(0.832076920150908, 0.876834572321288),
                          Mixed = c(0.0298345817221408, 0.012781266554823),
                          Asian = c(0.092025837245992, 0.0707309471990752),
                          BAC = c(0.0346840528714982, 0.0302027942235578),
                          Other = c(0.0113786080094611, 0.00945041970125592)
  )

  #rowSums(ethnicity[, -1])

  education <- data.frame(Age = c("16-24", "25-64"),
                          NoQual = c(0.141300611374007, 0.165608213960538),
                          L1 = c(0.177225905980983, 0.153626443727905),
                          L2 = c(0.254169883278531, 0.148567707526631),
                          L3 = c(0.257021008545131, 0.146921478279923),
                          L4 = c(0.136306290284813, 0.329952282607307),
                          Other = c(0.0339763005365347, 0.0553238738976964)
  )

  #rowSums(education[, -1])

  #Group education levels due to few observations in dataset
  education2 <- education %>%
    transmute(Age = Age, Uni = L4, NoUni = NoQual + L1 + L2 + L3, Other = Other)

  #Number of cells in combined matrix
  N <- nrow(gender) * ncol(gender[, -1]) * ncol(ethnicity[, -1]) * ncol(education2[, -1])


  #Output
  out <- data.frame(
    Age = gl(2, N/2, length = N, labels = c("16-24", "25-64")),
    Gender = gl(2, N/(2*2), length = N, labels = c("Male", "Female")),
    Ethnicity = gl(5, N/(2*2*5), length = N, labels = c("White", "Mixed", "Asian", "BAC", "Other")),
    Education = gl(3, 1, length = N, labels = c("Uni", "NoUni", "Other"))
  )

  #Calculate combined proportions

  #Insert gender vs age proportions
  out$AgeGenderP <- c(rep(gender$Male[1], N/4), rep(gender$Female[1], N/4),
                      rep(gender$Male[2], N/4), rep(gender$Female[2], N/4))

  #Divide by 2 (To sum to 1)
  out$AgeGenderP <- out$AgeGenderP / 2

  #Insert ethnicity proportions
  out$EthnicityP <- c(rep(unlist(ethnicity[1, -1]), times = 2, each = 3),
                      rep(unlist(ethnicity[2, -1]), times = 2, each = 3))

  #Calculate proportion of ethnicities within age and gender groups
  out$AgeGenderEthnP <- out$AgeGenderP * out$EthnicityP

  #Insert education proportions
  out$EducationP <- c(rep(unlist(education2[1, -1]), times = 10),
                      rep(unlist(education2[2, -1]), times = 10))

  #Calculate education proportions within ethnicities, age, and gender groups
  out$ExpectedP <- out$AgeGenderEthnP * out$EducationP

  #sum(out$ExpectedP) # Sums to 1, confirming that this is a valid distribution

  return(out[, c("Age", "Gender", "Ethnicity", "Education", "ExpectedP")])
}
