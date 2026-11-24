#' Clean Chemical Data
#'
#' Removes control treatments
#' Combines Trial ID and Treatment number into new column (unique trial ID)
#' Filters out columns with information not needed for our assessment
#'
#' @param data A dataframe containing chemical residue study data.
#'
#' @return A dataframe containing the selected variables for analysis, excluding control samples and organizing data in a useful way.
#'
#' @examples
#' cleaned_data <- Clean_Chemical_Data(test_data)
#'
#' @export


Clean_Chemical_Data <- function(data) {
  
  data_subset <- data %>%
    dplyr::filter(Form..Type != "Control") %>%
    
    # Combine Trial ID and Trt Num
    dplyr::mutate(
      Trial.Trt.ID = paste(Trial.ID, Trt.Num, sep = ".")
    ) %>%
    
    dplyr::select(
      TSite.Country,
      Reg..Region,
      Form..Type,
      Trial.Type,
      Analyte,
      LOQ,
      Trial.Trt.ID,
      Crop,
      Method,
      Spray.Vol.,
      Spray.Vol..UOM,
      Appl..Rate,
      Appl..Rate.UOM,
      No..of.Trt.s.,
      Commodity,
      Rpt.Conc.SE,
      DAT..d,
      Growth.Stage.at.Trt.s.,
    )
}