#' Clean Chemical Data
#'
#' Removes control treatments and trials where the necessary application information is missing
#' Gives all application rate units in kg ai/ha, including unit conversion where necessary
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
    dplyr::filter(
      Form..Type != "Control",
      Appl..Rate != ""
    ) %>%
    
    #Convert all units of application rate to kg ai/ha
    dplyr::mutate(
      Appl..Rate = dplyr::case_when(
        Appl..Rate.UOM == "g ai/ha" ~ Appl..Rate / 1000,
        Appl..Rate.UOM == "lb ai/acre" ~ Appl..Rate * 1.12,
        TRUE ~ Appl..Rate
      ),
      
      Appl..Rate.UOM = dplyr::case_when(
        Appl..Rate.UOM == "g ai/ha" ~ "Kg ai/ha",
        Appl..Rate.UOM == "lb ai/acre" ~ "Kg ai/ha",
        TRUE ~ Appl..Rate.UOM
      )
    ) %>%
    
    # Combine Trial ID and Trt Num
    dplyr::mutate(
      Trial.Trt.ID = paste(Trial.ID, Trt.Num, sep = ".")
    ) %>%
    
    dplyr::select(
      TSite.Country,
      Form..Type,
      Trial.Type,
      Analyte,
      LOQ,
      Trial.Trt.ID,
      Crop,
      Method,
      Appl..Rate,
      Appl..Rate.UOM,
      No..of.Trt.s.,
      Commodity,
      Rpt.Conc.SE,
      DAT..d,
      Growth.Stage.at.Trt.s.,
    )
}