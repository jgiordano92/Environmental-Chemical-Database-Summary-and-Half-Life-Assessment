#' Import Environmental Chemical Data
#' Imports a CSV file containing environmental chemical information
#' @param file Path to the CSV file
#' @return A data frame containing the imported chemical data.
#' @export

import_chemical_data <- function(file) {
  
  chemical_data <- read.csv(file, stringsAsFactors = FALSE)
  
  return(chemical_data)
  
}